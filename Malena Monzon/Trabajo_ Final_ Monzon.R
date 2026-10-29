library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

url_base_api <- "https://api.bcra.gob.ar/estadisticas/v4.0/monetarias"

FECHA_DESDE <- as.Date("2003-01-01")
FECHA_HASTA <- Sys.Date()

dir.create("bases", showWarnings = FALSE)

catalogo <- fromJSON(content(
  GET(url_base_api, config(ssl_verifypeer = FALSE)),
  "text", encoding = "UTF-8"
))$results

ids <- c(
  circulante  = 17,    # Billetes y monedas en poder del público (millones de $)
  dep_pesos   = 91,    # Depósitos en pesos, sectores público y privado no financieros
  cta_cte     = 85,    # Cuentas corrientes en pesos
  caja_ah     = 86,    # Cajas de ahorro en pesos
  plazo_fijo  = 87,    # Plazo fijo en pesos no ajustable por CER/UVA
  plazo_uva   = 88,    # Plazo fijo en pesos ajustable por CER/UVA
  dep_usd_ars = 103,   # Depósitos en dólares, valuados en millones de $
  dep_usd     = 107    # Depósitos en dólares, en millones de USD
)

tibble(serie = names(ids), idVariable = unname(ids)) %>%
  left_join(select(catalogo, any_of(c("idVariable", "descripcion", "moneda",
                                      "unidadExpresion", "primerFechaInformada"))),
            by = "idVariable") %>%
  write_csv("bases/ids_series.csv")

bajar_serie_bcra <- function(id, desde = FECHA_DESDE, hasta = FECHA_HASTA) {
  
  url     <- paste0(url_base_api, "/", id)
  limite  <- 3000
  offset  <- 0
  paginas <- list()
  
  repeat {
    respuesta <- GET(
      url,
      config(ssl_verifypeer = FALSE),
      query = list(
        desde  = format(desde, "%Y-%m-%d"),
        hasta  = format(hasta, "%Y-%m-%d"),
        limit  = limite,
        offset = offset
      )
    )
    
    if (status_code(respuesta) != 200) {
      stop("Error en la serie id = ", id, " (offset ", offset,
           "). Código: ", status_code(respuesta))
    }
    
    datos_json <- fromJSON(content(respuesta, "text", encoding = "UTF-8"))
    pagina <- datos_json$results$detalle[[1]]
    
    if (is.null(pagina) || nrow(pagina) == 0) break
    paginas[[length(paginas) + 1]] <- pagina
    if (nrow(pagina) < limite) break
    
    offset <- offset + limite
    Sys.sleep(0.5)
  }
  
  bind_rows(paginas) %>%
    mutate(fecha = as.Date(fecha), valor = as.numeric(valor)) %>%
    distinct(fecha, .keep_all = TRUE) %>%
    arrange(fecha) %>%
    select(fecha, valor)
}

series_diarias <- map(ids, bajar_serie_bcra)
series_diarias %>%
  imap_dfr(~ tibble(serie = .y, desde = min(.x$fecha), hasta = max(.x$fecha), n = nrow(.x))) %>%
  print()

iwalk(series_diarias, ~ write_csv(.x, paste0("bases/bcra_", .y, "_diario.csv")))


series <- c("circulante","dep_pesos","cta_cte","caja_ah","plazo_fijo","plazo_uva","dep_usd_ars","dep_usd")

base <- series %>%
  set_names() %>%
  map(~ read_csv(paste0("bases/bcra_", .x, "_diario.csv"), show_col_types = FALSE) %>%
        mutate(mes = floor_date(fecha, "month")) %>%
        summarise(valor = mean(valor, na.rm = TRUE), .by = mes) %>%
        rename(!!.x := valor)) %>%
  reduce(inner_join, by = "mes") %>%
  filter(mes < as.Date("2026-10-01")) %>%
  mutate(
    dep_m3    = dep_pesos / (circulante + dep_pesos) * 100,
    m3bi      = circulante + dep_pesos + dep_usd_ars,
    pesos_m3bi = dep_pesos / m3bi * 100,
    usd_m3bi  = dep_usd_ars / m3bi * 100,
    plazo     = (plazo_fijo + plazo_uva) / dep_pesos * 100,
    vista     = (cta_cte + caja_ah) / dep_pesos * 100,
    usd_mm    = dep_usd / 1000
  )

ind <- c("dep_m3","pesos_m3bi","usd_m3bi","plazo","vista","usd_mm")

base %>% mutate(anio = year(mes)) %>%
  summarise(across(all_of(ind), ~ round(mean(.x), 1)), .by = anio) %>%
  print(n = Inf)

base %>% select(mes, all_of(ind)) %>%
  pivot_longer(-mes) %>%
  group_by(name) %>%
  summarise(minimo = round(min(value), 1), fecha_min = mes[which.min(value)],
            maximo = round(max(value), 1), fecha_max = mes[which.max(value)]) %>%
  print()


hitos <- as.Date(c("2008-08-01","2008-12-01","2011-10-01","2012-06-01","2015-11-01","2016-06-01",
                   "2018-04-01","2018-12-01","2019-07-01","2019-12-01","2020-02-01","2020-12-01",
                   "2023-11-01","2024-07-01","2024-12-01","2025-03-01","2025-09-01","2026-09-01"))
base %>% filter(mes %in% hitos) %>%
  mutate(across(all_of(ind), ~ round(.x, 1))) %>%
  select(mes, all_of(ind)) %>%
  print(n = Inf)



