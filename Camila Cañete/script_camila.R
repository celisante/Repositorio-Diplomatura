# ============================================================
# Trabajo final - Polarización y estructura ocupacional del empleo
# Camila Cañete
# ============================================================

# ============================================================
# 1. PAQUETES
# ============================================================
# Carga de librerías necesarias para el análisis.
library(eph)
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# 2. CARPETAS
# ============================================================
# Creación de carpetas para guardar las bases y resultados procesados.
dir.create("bases", showWarnings = FALSE)
dir.create("datos_procesados", showWarnings = FALSE)

# ============================================================
# 3. DESCARGA DE LAS BASES
# ============================================================
# Descarga de microdatos de la EPH (4to trimestre) y guardado local.
eph_2017 <- get_microdata(year = 2017, period = 4, type = "individual")
eph_2021 <- get_microdata(year = 2021, period = 4, type = "individual")
eph_2024 <- get_microdata(year = 2024, period = 4, type = "individual")

saveRDS(eph_2017, "bases/eph_2017_individual.rds")
saveRDS(eph_2021, "bases/eph_2021_individual.rds")
saveRDS(eph_2024, "bases/eph_2024_individual.rds")

# ============================================================
# 4. CLASIFICACIÓN DE LA CALIFICACIÓN
# ============================================================
# Clasificación de la ocupación en 4 estratos según el último dígito del CNO.
clasificar_calificacion <- function(base) {
  base %>%
    mutate(
      digito_calificacion = as.numeric(PP04D_COD) %% 10,
      CALIFICACION = case_when(
        digito_calificacion == 1 ~ "Profesionales",
        digito_calificacion == 2 ~ "Técnicos",
        digito_calificacion == 3 ~ "Operativos",
        digito_calificacion == 4 ~ "No calificados",
        TRUE ~ NA_character_
      )
    )
}

eph_2017 <- clasificar_calificacion(eph_2017)
eph_2021 <- clasificar_calificacion(eph_2021)
eph_2024 <- clasificar_calificacion(eph_2024)

# ============================================================
# 5. NOMBRES DE LOS AGLOMERADOS
# ============================================================
# Asignación de nombres a los aglomerados y unificación de CABA y GBA.
nombres_aglomerados <- tibble(
  AGLOMERADO = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17, 18, 19, 20, 22, 23, 25, 26, 27, 29, 30, 31, 32, 33, 34, 36, 38, 91, 93),
  aglomerado_nombre = c("Gran La Plata", "Bahía Blanca-Cerri", "Gran Rosario", "Gran Santa Fe", "Gran Paraná", "Posadas", "Gran Resistencia", "Comodoro Rivadavia-Rada Tilly", "Gran Mendoza", "Corrientes", "Gran Córdoba", "Concordia", "Formosa", "Neuquén-Plottier", "Santiago del Estero-La Banda", "Jujuy-Palpalá", "Río Gallegos", "Gran Catamarca", "Gran Salta", "La Rioja", "Gran San Luis", "Gran San Juan", "Gran Tucumán-Tafí Viejo", "Santa Rosa-Toay", "Ushuaia-Río Grande", "Ciudad Autónoma de Buenos Aires", "Partidos del Gran Buenos Aires", "Mar del Plata", "Río Cuarto", "San Nicolás-Villa Constitución", "Rawson-Trelew", "Viedma-Carmen de Patagones")
)

nombres_aglomerados_analisis <- nombres_aglomerados %>%
  mutate(
    AGLOMERADO_ANALISIS = ifelse(AGLOMERADO %in% c(32, 33), 32, AGLOMERADO),
    aglomerado_nombre = ifelse(AGLOMERADO_ANALISIS == 32, "Gran Buenos Aires", aglomerado_nombre)
  ) %>%
  select(AGLOMERADO_ANALISIS, aglomerado_nombre) %>%
  distinct()

# ============================================================
# 6. PREPARACIÓN DE LA MUESTRA
# ============================================================
# Filtro de población objetivo: Ocupados de 18 a 65 años inclusive.
preparar_base <- function(base) {
  base %>%
    filter(
      ESTADO == 1,
      CH06 >= 18,
      CH06 <= 65,
      CALIFICACION %in% c("Profesionales", "Técnicos", "Operativos", "No calificados")
    ) %>%
    mutate(AGLOMERADO_ANALISIS = ifelse(AGLOMERADO %in% c(32, 33), 32, AGLOMERADO))
}

ocupados_2017 <- preparar_base(eph_2017)
ocupados_2021 <- preparar_base(eph_2021)
ocupados_2024 <- preparar_base(eph_2024)

saveRDS(ocupados_2017, "datos_procesados/eph_2017_ocupados.rds")
saveRDS(ocupados_2021, "datos_procesados/eph_2021_ocupados.rds")
saveRDS(ocupados_2024, "datos_procesados/eph_2024_ocupados.rds")

# ============================================================
# 7. TAMAÑO DE LAS MUESTRAS
# ============================================================
# Control de la cantidad de casos (N) en las bases depuradas.
n_muestra <- tibble(
  anio = c(2017, 2021, 2024),
  casos_iniciales = c(nrow(eph_2017), nrow(eph_2021), nrow(eph_2024)),
  ocupados = c(
    sum(eph_2017$ESTADO == 1, na.rm = TRUE),
    sum(eph_2021$ESTADO == 1, na.rm = TRUE),
    sum(eph_2024$ESTADO == 1, na.rm = TRUE)
  ),
  muestra_final = c(nrow(ocupados_2017), nrow(ocupados_2021), nrow(ocupados_2024))
)

# ============================================================
# 8. COMPOSICIÓN TOTAL DEL EMPLEO
# ============================================================
# Cálculo de la composición del empleo a nivel nacional utilizando PONDERA.
calcular_composicion <- function(base, anio) {
  base %>%
    group_by(CALIFICACION) %>%
    summarise(
      cantidad = sum(PONDERA, na.rm = TRUE),
      n_casos = n(),
      .groups = "drop"
    ) %>%
    mutate(
      porcentaje = cantidad / sum(cantidad) * 100,
      anio = anio
    )
}

composicion_2017 <- calcular_composicion(ocupados_2017, 2017)
composicion_2021 <- calcular_composicion(ocupados_2021, 2021)
composicion_2024 <- calcular_composicion(ocupados_2024, 2024)

composicion_total <- bind_rows(composicion_2017, composicion_2021, composicion_2024)
saveRDS(composicion_total, "datos_procesados/composicion_total.rds")

# ============================================================
# 9. COMPOSICIÓN POR AGLOMERADO
# ============================================================
# Cálculo de la composición del empleo desagregada por mercado local.
calcular_composicion_aglomerado <- function(base, anio) {
  base %>%
    group_by(AGLOMERADO_ANALISIS, CALIFICACION) %>%
    summarise(
      cantidad = sum(PONDERA, na.rm = TRUE),
      n_casos = n(),
      .groups = "drop"
    ) %>%
    group_by(AGLOMERADO_ANALISIS) %>%
    mutate(
      porcentaje = cantidad / sum(cantidad) * 100,
      anio = anio
    ) %>%
    ungroup()
}

composicion_2017_aglomerado_31 <- calcular_composicion_aglomerado(ocupados_2017, 2017) %>% left_join(nombres_aglomerados_analisis, by = "AGLOMERADO_ANALISIS")
composicion_2021_aglomerado_31 <- calcular_composicion_aglomerado(ocupados_2021, 2021) %>% left_join(nombres_aglomerados_analisis, by = "AGLOMERADO_ANALISIS")
composicion_2024_aglomerado_31 <- calcular_composicion_aglomerado(ocupados_2024, 2024) %>% left_join(nombres_aglomerados_analisis, by = "AGLOMERADO_ANALISIS")

saveRDS(composicion_2017_aglomerado_31, "datos_procesados/composicion_2017_aglomerado.rds")
saveRDS(composicion_2021_aglomerado_31, "datos_procesados/composicion_2021_aglomerado.rds")
saveRDS(composicion_2024_aglomerado_31, "datos_procesados/composicion_2024_aglomerado.rds")

# ============================================================
# 10. CAMBIO 2017-2024
# ============================================================
# Cálculo de la variación en puntos porcentuales entre 2017 y 2024.
cambio_calificacion_31 <- bind_rows(
  composicion_2017_aglomerado_31 %>% mutate(anio = 2017),
  composicion_2024_aglomerado_31 %>% mutate(anio = 2024)
) %>%
  filter(CALIFICACION %in% c("Profesionales", "Técnicos", "Operativos", "No calificados")) %>%
  select(AGLOMERADO_ANALISIS, aglomerado_nombre, CALIFICACION, anio, porcentaje) %>%
  pivot_wider(names_from = anio, values_from = porcentaje, names_prefix = "porcentaje_") %>%
  mutate(cambio_pp = porcentaje_2024 - porcentaje_2017)

saveRDS(cambio_calificacion_31, "datos_procesados/cambio_calificacion_31.rds")

# ============================================================
# 11. POLARIZACIÓN TERRITORIAL
# ============================================================
# Identificación de aglomerados donde aumentan simultáneamente ambos extremos.
polarizacion_31 <- cambio_calificacion_31 %>%
  filter(CALIFICACION %in% c("Profesionales", "No calificados")) %>%
  select(AGLOMERADO_ANALISIS, aglomerado_nombre, CALIFICACION, cambio_pp) %>%
  pivot_wider(names_from = CALIFICACION, values_from = cambio_pp) %>%
  mutate(ambos_extremos_suben = Profesionales > 0 & `No calificados` > 0) %>%
  select(AGLOMERADO_ANALISIS, aglomerado_nombre, `No calificados`, Profesionales, ambos_extremos_suben) %>%
  arrange(desc(ambos_extremos_suben))

saveRDS(polarizacion_31, "datos_procesados/polarizacion_31.rds")

# ============================================================
# 12. POLARIZACIÓN AGREGADA
# ============================================================
# Resumen del crecimiento conjunto de extremos vs. intermedios a nivel nacional.
polarizacion_total <- composicion_total %>%
  filter(CALIFICACION %in% c("Profesionales", "No calificados", "Técnicos", "Operativos")) %>%
  group_by(anio) %>%
  summarise(
    extremos = sum(porcentaje[CALIFICACION %in% c("Profesionales", "No calificados")]),
    intermedios = sum(porcentaje[CALIFICACION %in% c("Técnicos", "Operativos")]),
    .groups = "drop"
  )

saveRDS(polarizacion_total, "datos_procesados/polarizacion_total.rds")

# ============================================================
# 13. INGRESO LABORAL PROMEDIO RELATIVO
# ============================================================
# Cálculo del ingreso promedio relativo tomando como base a los No calificados.
calcular_ingresos <- function(base, anio) {
  base %>%
    filter(!is.na(P21), P21 > 0, !is.na(PONDIIO), PONDIIO > 0) %>%
    group_by(CALIFICACION) %>%
    summarise(
      ingreso_promedio = sum(P21 * PONDIIO, na.rm = TRUE) / sum(PONDIIO, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(anio = anio)
}

ingresos_total <- bind_rows(
  calcular_ingresos(ocupados_2017, 2017),
  calcular_ingresos(ocupados_2021, 2021),
  calcular_ingresos(ocupados_2024, 2024)
) %>%
  group_by(anio) %>%
  mutate(
    ingreso_no_calificados = ingreso_promedio[CALIFICACION == "No calificados"],
    veces_no_calificados = ingreso_promedio / ingreso_no_calificados
  ) %>%
  ungroup()

saveRDS(ingresos_total, "datos_procesados/ingresos_total.rds")

# ============================================================
# 14. GRÁFICOS (ggplot2)
# ============================================================

# Gráfico 1: Estructura ocupacional agregada
grafico_composicion <- ggplot(composicion_total, aes(x = factor(anio), y = porcentaje, fill = CALIFICACION)) +
  geom_col() +
  labs(title = "Composición del empleo según calificación de la ocupación", subtitle = "Aglomerados urbanos relevados por la EPH", x = "Año", y = "Participación en el empleo (%)", fill = "Calificación") +
  theme_minimal()

grafico_composicion 

# Gráfico 2: Dispersión de la polarización territorial
grafico_polarizacion <- ggplot(polarizacion_31, aes(x = Profesionales, y = `No calificados`)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(aes(color = ambos_extremos_suben), size = 3) +
  geom_text(data = subset(polarizacion_31, ambos_extremos_suben), aes(label = aglomerado_nombre), vjust = -1, size = 3, check_overlap = TRUE) +
  labs(title = "Cambio en profesionales y no calificados, 2017–2024", subtitle = "Variación en puntos porcentuales por aglomerado", x = "Cambio en profesionales (p.p.)", y = "Cambio en no calificados (p.p.)", color = "Ambos extremos aumentan") +
  theme_minimal()

grafico_polarizacion 

# Gráfico 3: Evolución de las brechas de ingreso
grafico_ingresos <- ggplot(ingresos_total, aes(x = CALIFICACION, y = veces_no_calificados)) +
  geom_col() +
  facet_wrap(~ anio) +
  labs(title = "Ingreso laboral promedio relativo a los no calificados", subtitle = "Aglomerados urbanos relevados por la EPH", x = "Calificación de la ocupación", y = "Veces el ingreso promedio de los no calificados") +
  theme_minimal()

grafico_ingresos 

# ============================================================
# FIN DEL SCRIPT
# ============================================================