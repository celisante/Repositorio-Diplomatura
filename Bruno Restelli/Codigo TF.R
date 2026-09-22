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
mis_variables <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE", "CH06", "ESTADO", "CAT_OCUP", 
                   "PP07H", "INTENSI", "PP07C", "PONDERA","PP03G")


# ==============================================================================
# 2. DESCARGA BASES (2ºT 2016 a 2ºT 2026)
# ==============================================================================


base_16_2 <- get_microdata(year = 2016, trimester = 2, vars = mis_variables)
base_16_3 <- get_microdata(year = 2016, trimester = 3, vars = mis_variables)
base_16_4 <- get_microdata(year = 2016, trimester = 4, vars = mis_variables)

base_17_1 <- get_microdata(year = 2017, trimester = 1, vars = mis_variables)
base_17_2 <- get_microdata(year = 2017, trimester = 2, vars = mis_variables)
base_17_3 <- get_microdata(year = 2017, trimester = 3, vars = mis_variables)
base_17_4 <- get_microdata(year = 2017, trimester = 4, vars = mis_variables)

base_18_1 <- get_microdata(year = 2018, trimester = 1, vars = mis_variables)
base_18_2 <- get_microdata(year = 2018, trimester = 2, vars = mis_variables)
base_18_3 <- get_microdata(year = 2018, trimester = 3, vars = mis_variables)
base_18_4 <- get_microdata(year = 2018, trimester = 4, vars = mis_variables)

base_19_1 <- get_microdata(year = 2019, trimester = 1, vars = mis_variables)
base_19_2 <- get_microdata(year = 2019, trimester = 2, vars = mis_variables)
base_19_3 <- get_microdata(year = 2019, trimester = 3, vars = mis_variables)
base_19_4 <- get_microdata(year = 2019, trimester = 4, vars = mis_variables)

base_20_1 <- get_microdata(year = 2020, trimester = 1, vars = mis_variables)
base_20_2 <- get_microdata(year = 2020, trimester = 2, vars = mis_variables)
base_20_3 <- get_microdata(year = 2020, trimester = 3, vars = mis_variables)
base_20_4 <- get_microdata(year = 2020, trimester = 4, vars = mis_variables)

base_21_1 <- get_microdata(year = 2021, trimester = 1, vars = mis_variables)
base_21_2 <- get_microdata(year = 2021, trimester = 2, vars = mis_variables)
base_21_3 <- get_microdata(year = 2021, trimester = 3, vars = mis_variables)
base_21_4 <- get_microdata(year = 2021, trimester = 4, vars = mis_variables)

base_22_1 <- get_microdata(year = 2022, trimester = 1, vars = mis_variables)
base_22_2 <- get_microdata(year = 2022, trimester = 2, vars = mis_variables)
base_22_3 <- get_microdata(year = 2022, trimester = 3, vars = mis_variables)
base_22_4 <- get_microdata(year = 2022, trimester = 4, vars = mis_variables)

base_23_1 <- get_microdata(year = 2023, trimester = 1, vars = mis_variables)
base_23_2 <- get_microdata(year = 2023, trimester = 2, vars = mis_variables)
base_23_3 <- get_microdata(year = 2023, trimester = 3, vars = mis_variables)
base_23_4 <- get_microdata(year = 2023, trimester = 4, vars = mis_variables)

base_24_1 <- get_microdata(year = 2024, trimester = 1, vars = mis_variables)
base_24_2 <- get_microdata(year = 2024, trimester = 2, vars = mis_variables)
base_24_3 <- get_microdata(year = 2024, trimester = 3, vars = mis_variables)
base_24_4 <- get_microdata(year = 2024, trimester = 4, vars = mis_variables)

base_25_1 <- get_microdata(year = 2025, trimester = 1, vars = mis_variables)
base_25_2 <- get_microdata(year = 2025, trimester = 2, vars = mis_variables)
base_25_3 <- get_microdata(year = 2025, trimester = 3, vars = mis_variables)
base_25_4 <- get_microdata(year = 2025, trimester = 4, vars = mis_variables)

base_26_1 <- get_microdata(year = 2026, trimester = 1, vars = mis_variables)
base_26_2 <- get_microdata(year = 2026, trimester = 2, vars = mis_variables)