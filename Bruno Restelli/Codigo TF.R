rm(list = ls())

options(scipen = 999) 

# ============================================================
# LIBRERÍAS
# ============================================================

library(tidyverse)
library(eph)
library(dplyr)
library(ggplot2)
library(paletteer)

# ==============================================================================
# 1. VARIABLES A DESCARGAR
# ==============================================================================

mis_variables <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "CH06", "PP3E_TOT", "PP03G", "PP03H","PP03J", "PONDERA", "PONDIH", "ESTADO", "CAT_OCUP", "PP04B_COD", "PP04A", "PP04B1", "PP04C", "PP04C99", "PP07H", "P21", "PONDIIO", "ITF", "INTENSI")

# ==============================================================================
# 2. DESCARGA BASES (2ºT 2016 a 1ºT 2026)
# ==============================================================================


# Año 2016
base_16_2 <- get_microdata(year = 2016, period = 2, type = "individual", vars = mis_variables)
base_16_3 <- get_microdata(year = 2016, period = 3, type = "individual", vars = mis_variables)
base_16_4 <- get_microdata(year = 2016, period = 4, type = "individual", vars = mis_variables)

# Año 2017
base_17_1 <- get_microdata(year = 2017, period = 1, type = "individual", vars = mis_variables)
base_17_2 <- get_microdata(year = 2017, period = 2, type = "individual", vars = mis_variables)
base_17_3 <- get_microdata(year = 2017, period = 3, type = "individual", vars = mis_variables)
base_17_4 <- get_microdata(year = 2017, period = 4, type = "individual", vars = mis_variables)

# Año 2018
base_18_1 <- get_microdata(year = 2018, period = 1, type = "individual", vars = mis_variables)
base_18_2 <- get_microdata(year = 2018, period = 2, type = "individual", vars = mis_variables)
base_18_3 <- get_microdata(year = 2018, period = 3, type = "individual", vars = mis_variables)
base_18_4 <- get_microdata(year = 2018, period = 4, type = "individual", vars = mis_variables)

# Año 2019
base_19_1 <- get_microdata(year = 2019, period = 1, type = "individual", vars = mis_variables)
base_19_2 <- get_microdata(year = 2019, period = 2, type = "individual", vars = mis_variables)
base_19_3 <- get_microdata(year = 2019, period = 3, type = "individual", vars = mis_variables)
base_19_4 <- get_microdata(year = 2019, period = 4, type = "individual", vars = mis_variables)

# Año 2020
base_20_1 <- get_microdata(year = 2020, period = 1, type = "individual", vars = mis_variables)
base_20_2 <- get_microdata(year = 2020, period = 2, type = "individual", vars = mis_variables)
base_20_3 <- get_microdata(year = 2020, period = 3, type = "individual", vars = mis_variables)
base_20_4 <- get_microdata(year = 2020, period = 4, type = "individual", vars = mis_variables)

# Año 2021
base_21_1 <- get_microdata(year = 2021, period = 1, type = "individual", vars = mis_variables)
base_21_2 <- get_microdata(year = 2021, period = 2, type = "individual", vars = mis_variables)
base_21_3 <- get_microdata(year = 2021, period = 3, type = "individual", vars = mis_variables)
base_21_4 <- get_microdata(year = 2021, period = 4, type = "individual", vars = mis_variables)

# Año 2022
base_22_1 <- get_microdata(year = 2022, period = 1, type = "individual", vars = mis_variables)
base_22_2 <- get_microdata(year = 2022, period = 2, type = "individual", vars = mis_variables)
base_22_3 <- get_microdata(year = 2022, period = 3, type = "individual", vars = mis_variables)
base_22_4 <- get_microdata(year = 2022, period = 4, type = "individual", vars = mis_variables)

# Año 2023
base_23_1 <- get_microdata(year = 2023, period = 1, type = "individual", vars = mis_variables)
base_23_2 <- get_microdata(year = 2023, period = 2, type = "individual", vars = mis_variables)
base_23_3 <- get_microdata(year = 2023, period = 3, type = "individual", vars = mis_variables)
base_23_4 <- get_microdata(year = 2023, period = 4, type = "individual", vars = mis_variables)

# Año 2024
base_24_1 <- get_microdata(year = 2024, period = 1, type = "individual", vars = mis_variables)
base_24_2 <- get_microdata(year = 2024, period = 2, type = "individual", vars = mis_variables)
base_24_3 <- get_microdata(year = 2024, period = 3, type = "individual", vars = mis_variables)
base_24_4 <- get_microdata(year = 2024, period = 4, type = "individual", vars = mis_variables)

# Año 2025
base_25_1 <- get_microdata(year = 2025, period = 1, type = "individual", vars = mis_variables)
base_25_2 <- get_microdata(year = 2025, period = 2, type = "individual", vars = mis_variables)
base_25_3 <- get_microdata(year = 2025, period = 3, type = "individual", vars = mis_variables)
base_25_4 <- get_microdata(year = 2025, period = 4, type = "individual", vars = mis_variables)

# Año 2026
base_26_1 <- get_microdata(year = 2026, period = 1, type = "individual", vars = mis_variables)


### UNION Y ORDEN DE LAS BASES ###

nombres_bases <- ls(pattern = "^base_[0-9]{2}_[1-4]$")

base_completa <- mget(nombres_bases) %>%
  bind_rows(.id = "nombre_base")

base_completa <- base_completa %>%
  mutate(
    año = 2000 + as.numeric(substr(nombre_base, 6, 7)),
    trimestre = as.numeric(substr(nombre_base, 9, 9)),
    periodo = paste0(trimestre, "T", año)
  ) %>% 

arrange(año, trimestre) %>%
  
  select(-nombre_base)

base_completa <- base_completa %>%
  relocate(periodo, año, trimestre, .before = 1)


# ============================================================
# CATEGORIAS OCUPACIONALES
# ============================================================

base_proc <- base_completa %>%
  rename(
    codusu = CODUSU,
    nro_hogar = NRO_HOGAR,
    componente = COMPONENTE,
    pondera = PONDERA,
    pondera_hogar = PONDIH,
    edad = CH06,
    cat_ocup = CAT_OCUP,
    estado = ESTADO,
    pondera_in = PONDIIO,
    ingreso_hogar = ITF
  ) %>% 
  
  mutate(
    
    ocupado = if_else(
      estado == 1,
      1,
      0),
    
    desocupado = if_else(
      estado == 2,
      1,
      0),
    
    activo = if_else(
      estado %in% c(1, 2),
      1,
      0),
    
    subocupado = if_else(
      ocupado == 1 &
        PP3E_TOT < 35 &
        PP03G == 1 &
        PP03H == 1,
      1,
      0),
    
    ocupado_demandante = if_else(
      ocupado == 1 &
        PP03J == 1,
      1,
      0
    ),
    asalariado = if_else(
      ocupado == 1 &
        cat_ocup == 3,
      1,
      0),
    
    no_asal = if_else(
      ocupado == 1 &
        cat_ocup %in% c(1, 2, 4),
      1,
      0),
    
    asareg = if_else(
      ocupado == 1 &
        cat_ocup == 3 &
        PP07H == 1,
      1,
      0),
    
    asanreg = if_else(
      ocupado == 1 &
        cat_ocup == 3 &
        PP07H == 2,
      1,
      0),
    
    # 1 = informal / no registrado
    informal = if_else(
      asanreg == 1,
      1,
      0),
    
    cuenta_prop = if_else(
      ocupado == 1 &
        cat_ocup == 2,
      1,
      0),
    
    patron = if_else(
      ocupado == 1 &
        cat_ocup == 1,
      1,
      0),
    fam_no_remu = if_else(
      ocupado == 1 &
        cat_ocup == 4,
      1,
      0),
    
    serdom = if_else(
      ocupado == 1 &
        PP04B1 == 1,
      1,
      0)
  )

# ============================================================
# TASAS LABORALES
# ============================================================


tasas_laborales <- base_proc %>%
  filter(edad >= 14, estado %in% c(1, 2, 3)) %>%
  group_by(año, trimestre) %>%
  summarise(
    poblacion = sum(pondera, na.rm = TRUE),
    ocupados = sum(pondera[estado == 1], na.rm = TRUE),
    desocupados = sum(pondera[estado == 2], na.rm = TRUE),
    ocupados_demandantes = sum(ocupado_demandante * pondera, na.rm = TRUE),
    PEA = sum(pondera[estado %in% c(1, 2)], na.rm = TRUE),
    
    
    tasa_actividad = PEA / poblacion * 100,
    
    tasa_ocupacion = ocupados / poblacion * 100,
    
    tasa_desocupacion = desocupados / PEA * 100,
    
    tasa_ocupados_demandantes =
      ocupados_demandantes / PEA * 100,
    
    .groups = "drop"
  )

tasas_laborales <- tasas_laborales %>% 
  arrange(año, trimestre)%>%
  mutate(
    across(
      everything(),
      ~ round(.x , 1)
    )
  )

groups(tasas_laborales)

