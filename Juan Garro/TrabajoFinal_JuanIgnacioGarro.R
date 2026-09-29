
system2("C:/Program Files/Git/bin/git.exe",
        c("config", "--global", "user.name", shQuote("Juan Ignacio Garro")))

system2("C:/Program Files/Git/bin/git.exe",
        c("config", "--global", "user.email", shQuote("juanii.garro@gmail.com")))


system2("C:/Program Files/Git/bin/git.exe", c("config", "--global", "user.name"))
system2("C:/Program Files/Git/bin/git.exe", c("config", "--global", "user.email"))

# TRABAJO FINAL - EJECUTAR DESPUES DE IMPORT DATASET
# Este codigo usa los tres objetos que ya aparecen en tu Environment.
# Abri este archivo en la misma sesion de RStudio y presiona Source.
# Conserva las bases originales y trabaja sobre copias.
#
# Si falta algun paquete, ejecutar una sola vez en la consola:
# install.packages(c("dplyr", "ggplot2"), repos = "https://cloud.r-project.org")

library(dplyr)
library(ggplot2)
options(scipen = 999)

# ============================================================
# 1. BASES YA IMPORTADAS Y CORRECCION DE ENCABEZADOS
# ============================================================

base_2019 <- usu_individual_T219
base_2023 <- usu_individual_T223
base_2025 <- usu_individual_T225

# Si Import Dataset no reconocio los encabezados, las columnas se llaman
# V1, V2... o similares y la primera fila contiene CODUSU, ANO4, etc.
# En ese caso usamos esa fila como nombres y luego quitamos esa fila.
# Si la importacion ya tiene encabezados correctos, no se modifica.

if (!("ANO4" %in% names(base_2019))) {
  names(base_2019) <- as.character(unlist(base_2019[1, ]))
  base_2019 <- base_2019[-1, ]
}

if (!("ANO4" %in% names(base_2023))) {
  names(base_2023) <- as.character(unlist(base_2023[1, ]))
  base_2023 <- base_2023[-1, ]
}

if (!("ANO4" %in% names(base_2025))) {
  names(base_2025) <- as.character(unlist(base_2025[1, ]))
  base_2025 <- base_2025[-1, ]
}

# Las columnas numericas se convierten a numeros en el bloque siguiente.
# Esto es necesario porque una importacion sin encabezados puede haber
# guardado todas las columnas como texto.

# ============================================================
# 2. SELECCIÓN Y UNIÓN DE LAS BASES
# ============================================================

base_2019 <- base_2019 %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, TRIMESTRE,
         AGLOMERADO, CH04, CH06, ESTADO, CAT_OCUP, INTENSI, PP07H, PONDERA) %>%
  mutate(
    CODUSU = as.character(CODUSU),
    across(-CODUSU, ~ as.numeric(as.character(.x)))
  )

base_2023 <- base_2023 %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, TRIMESTRE,
         AGLOMERADO, CH04, CH06, ESTADO, CAT_OCUP, INTENSI, PP07H, PONDERA) %>%
  mutate(
    CODUSU = as.character(CODUSU),
    across(-CODUSU, ~ as.numeric(as.character(.x)))
  )

base_2025 <- base_2025 %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, TRIMESTRE,
         AGLOMERADO, CH04, CH06, ESTADO, CAT_OCUP, INTENSI, PP07H, PONDERA) %>%
  mutate(
    CODUSU = as.character(CODUSU),
    across(-CODUSU, ~ as.numeric(as.character(.x)))
  )

base <- bind_rows(base_2019, base_2023, base_2025)

base_proc <- base %>%
  rename(
    anio = ANO4,
    trimestre = TRIMESTRE,
    sexo = CH04,
    edad = CH06,
    estado = ESTADO,
    cat_ocup = CAT_OCUP,
    pondera = PONDERA
  )

# ============================================================
# 3. LIMPIEZA Y CONSTRUCCIÓN DE VARIABLES
# ============================================================

# Control previo: cantidad de filas y observaciones fuera del universo.
control_limpieza <- base_proc %>%
  group_by(anio) %>%
  summarise(
    personas_base = n(),
    entrevistas_no_realizadas = sum(estado == 0, na.rm = TRUE),
    edad_fuera_del_recorte = sum(edad < 16 | edad > 59, na.rm = TRUE),
    ponderador_invalido = sum(is.na(pondera) | pondera <= 0),
    .groups = "drop"
  )

# Los controles anteriores pueden superponerse: no deben sumarse.
base_proc <- base_proc %>%
  filter(
    trimestre == 2,
    edad >= 16, edad <= 59,
    estado %in% c(1, 2, 3),
    !is.na(pondera), pondera > 0
  ) %>%
  mutate(
    grupo_edad = case_when(
      edad >= 16 & edad <= 24 ~ "16-24",
      edad >= 25 & edad <= 44 ~ "25-44",
      edad >= 45 & edad <= 59 ~ "45-59"
    ),
    grupo_edad = factor(grupo_edad, levels = c("16-24", "25-44", "45-59")),
    sexo = factor(sexo, levels = c(1, 2), labels = c("Varones", "Mujeres")),
    ocupado = if_else(estado == 1, 1, 0),
    desocupado = if_else(estado == 2, 1, 0),
    activo = if_else(estado %in% c(1, 2), 1, 0),
    asalariado = if_else(estado == 1 & cat_ocup == 3, 1, 0),

    # INTENSI ya clasifica la subocupación por insuficiencia horaria.
    # 1 = subocupado; 2, 3 y 4 = otras situaciones de ocupación.
    # Un desocupado pertenece a la PEA, pero no es subocupado.
    subocupado = case_when(
      estado == 2 ~ 0,
      estado == 1 & INTENSI == 1 ~ 1,
      estado == 1 & INTENSI %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    # PP07H: 1 = tiene descuento jubilatorio; 2 = no tiene.
    # Se define únicamente para asalariados ocupados.
    no_registrado = case_when(
      asalariado == 1 & PP07H == 2 ~ 1,
      asalariado == 1 & PP07H == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Revisar la respuesta válida antes de calcular las tasas.
control_datos <- base_proc %>%
  group_by(anio, grupo_edad) %>%
  summarise(
    personas_muestra = n(),
    activos_muestra = sum(activo),
    activos_sin_dato_subocupacion = sum(activo == 1 & is.na(subocupado)),
    asalariados_muestra = sum(asalariado),
    asalariados_sin_dato_registro = sum(asalariado == 1 & is.na(no_registrado)),
    personas_sin_dato_sexo = sum(is.na(sexo)),
    .groups = "drop"
  )

print(control_limpieza, width = Inf)
print(control_datos, width = Inf)

# ============================================================
# 4. TABLAS DE INDICADORES POR EDAD
# ============================================================

tabla_desocupacion <- base_proc %>%
  filter(activo == 1) %>%
  group_by(anio, grupo_edad) %>%
  summarise(
    casos_pea = n(),
    pea = sum(pondera),
    desocupados = sum(pondera * desocupado),
    tasa_desocupacion = desocupados / pea * 100,
    .groups = "drop"
  )

tabla_subocupacion <- base_proc %>%
  filter(activo == 1, !is.na(subocupado)) %>%
  group_by(anio, grupo_edad) %>%
  summarise(
    casos_pea_validos = n(),
    pea_con_dato = sum(pondera),
    subocupados = sum(pondera * subocupado),
    tasa_subocupacion = subocupados / pea_con_dato * 100,
    .groups = "drop"
  )

# Si hay datos faltantes, la tasa anterior utiliza la PEA con información.
# En las bases entregadas se controla que no haya faltantes entre los activos.

tabla_no_registro <- base_proc %>%
  filter(asalariado == 1, !is.na(no_registrado)) %>%
  group_by(anio, grupo_edad) %>%
  summarise(
    casos_asalariados_validos = n(),
    asalariados_validos = sum(pondera),
    asalariados_no_registrados = sum(pondera * no_registrado),
    porcentaje_no_registro = asalariados_no_registrados / asalariados_validos * 100,
    .groups = "drop"
  )

# Tabla resumida para leer y presentar. Tasas expresadas en porcentajes.
tabla_resumen <- tabla_desocupacion %>%
  select(anio, grupo_edad, tasa_desocupacion) %>%
  left_join(
    tabla_subocupacion %>% select(anio, grupo_edad, tasa_subocupacion),
    by = c("anio", "grupo_edad")
  ) %>%
  left_join(
    tabla_no_registro %>% select(anio, grupo_edad, porcentaje_no_registro),
    by = c("anio", "grupo_edad")
  ) %>%
  mutate(
    tasa_desocupacion = round(tasa_desocupacion, 1),
    tasa_subocupacion = round(tasa_subocupacion, 1),
    porcentaje_no_registro = round(porcentaje_no_registro, 1)
  )

print(tabla_resumen, width = Inf)

# ============================================================
# 5. FALTA DE REGISTRO ENTRE JÓVENES, POR SEXO
# ============================================================

tabla_jovenes_sexo <- base_proc %>%
  filter(grupo_edad == "16-24", asalariado == 1,
         !is.na(no_registrado), !is.na(sexo)) %>%
  group_by(anio, sexo) %>%
  summarise(
    casos_asalariados_validos = n(),
    asalariados_validos = sum(pondera),
    asalariados_no_registrados = sum(pondera * no_registrado),
    porcentaje_no_registro = asalariados_no_registrados / asalariados_validos * 100,
    .groups = "drop"
  )

print(tabla_jovenes_sexo, width = Inf)

# ============================================================
# 6. GRÁFICO 1: DESOCUPACIÓN SEGÚN GRUPO DE EDAD
# ============================================================

grafico_1 <- ggplot(tabla_desocupacion,
  aes(x = factor(anio), y = tasa_desocupacion, fill = grupo_edad)) +
  geom_col(position = "dodge", width = 0.75) +
  geom_text(
    aes(label = paste0(round(tasa_desocupacion, 1), "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(values = c("16-24" = "#285F82", "25-44" = "#AC612B", "45-59" = "#657848")) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  labs(
    title = "Desocupación según grupo de edad",
    subtitle = "Personas de 16 a 59 años | Porcentaje de la PEA de cada grupo",
    x = "Segundo trimestre de cada año", y = "Porcentaje (%)", fill = "Edad (años)",
    caption = "Fuente: elaboración propia con microdatos de la EPH-INDEC. Aglomerados relevados."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

print(grafico_1)

# ============================================================
# 7. GRÁFICO 2: SUBOCUPACIÓN HORARIA SEGÚN GRUPO DE EDAD
# ============================================================

grafico_2 <- ggplot(tabla_subocupacion,
  aes(x = factor(anio), y = tasa_subocupacion, fill = grupo_edad)) +
  geom_col(position = "dodge", width = 0.75) +
  geom_text(
    aes(label = paste0(round(tasa_subocupacion, 1), "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(values = c("16-24" = "#285F82", "25-44" = "#AC612B", "45-59" = "#657848")) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  labs(
    title = "Subocupación horaria según grupo de edad",
    subtitle = "Personas de 16 a 59 años | Porcentaje de la PEA de cada grupo",
    x = "Segundo trimestre de cada año", y = "Porcentaje (%)", fill = "Edad (años)",
    caption = "Fuente: elaboración propia con microdatos de la EPH-INDEC. Aglomerados relevados."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

print(grafico_2)

# ============================================================
# 8. GRÁFICO 3: EMPLEO ASALARIADO NO REGISTRADO SEGÚN EDAD
# ============================================================

grafico_3 <- ggplot(tabla_no_registro,
  aes(x = factor(anio), y = porcentaje_no_registro, fill = grupo_edad)) +
  geom_col(position = "dodge", width = 0.75) +
  geom_text(
    aes(label = paste0(round(porcentaje_no_registro, 1), "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(values = c("16-24" = "#285F82", "25-44" = "#AC612B", "45-59" = "#657848")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title = "Empleo asalariado no registrado según edad",
    subtitle = "Personas de 16 a 59 años | Porcentaje de asalariados con respuesta válida",
    x = "Segundo trimestre de cada año", y = "Porcentaje (%)", fill = "Edad (años)",
    caption = "Fuente: elaboración propia con microdatos de la EPH-INDEC. Aglomerados relevados."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

print(grafico_3)

# ============================================================
# 9. GRÁFICO 4: JÓVENES: EMPLEO ASALARIADO NO REGISTRADO POR SEXO
# ============================================================

grafico_4 <- ggplot(tabla_jovenes_sexo,
  aes(x = factor(anio), y = porcentaje_no_registro, fill = sexo)) +
  geom_col(position = "dodge", width = 0.75) +
  geom_text(
    aes(label = paste0(round(porcentaje_no_registro, 1), "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.5, size = 3.5
  ) +
  scale_fill_manual(values = c("Varones" = "#285F82", "Mujeres" = "#AC612B")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title = "Jóvenes: empleo asalariado no registrado por sexo",
    subtitle = "Personas de 16 a 24 años | Porcentaje de asalariados con respuesta válida",
    x = "Segundo trimestre de cada año", y = "Porcentaje (%)", fill = "Sexo",
    caption = "Fuente: elaboración propia con microdatos de la EPH-INDEC. Aglomerados relevados."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 11),
    legend.position = "bottom"
  )

print(grafico_4)


# ============================================================
# 10. MOSTRAR LOS RESULTADOS AL TERMINAR
# ============================================================

message("PROCESAMIENTO COMPLETO. Esta es la tabla resumen:")
print(tabla_resumen, width = Inf)

# En RStudio se abre una pestana con la tabla, como al abrir una base.
# Los cuatro graficos estan en Plots; se recorren con las flechas.
if (interactive()) {
  View(tabla_resumen, title = "RESULTADOS - Tasas por edad")
}
message("Graficos disponibles en Plots. Usa las flechas para ver los cuatro.")

