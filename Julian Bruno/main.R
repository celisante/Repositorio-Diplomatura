rm(list = ls())

options(scipen = 999)
#librerias

library(tidyverse)
library(dplyr)
library(ggplot2)
library(paletteer)

install.packages("WDI")

library(WDI)

#DATOS
base_wdi <- WDI(country = "all", indicator = c(
  "vam_pct" = "NV.IND.MANF.ZS",
  "vam_constante" = "NV.IND.MANF.KD"),
  start = 1990,
  end = 2025
  ) 
# filtramos con 3 paises para comparar
base_filtrada <- base_wdi %>% 
  filter(iso2c %in% c ("AR","BR","KR"))

#graficamos 
ggplot(base_filtrada, aes(x = year, y = vam_pct, color = country)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # Formato % en el eje Y
  theme_minimal(base_size = 12) +
  labs(
    title = "Evolución de la Participación Industrial en el PIB (1990-2024)",
    subtitle = "Valor Agregado Manufacturero (% del PIB)",
    x = "Año",
    y = "Participación del VAM (%)",
    color = "País",
    caption = "Fuente: Elaboración propia en base a WDI (Banco Mundial)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
    
  )
#                                                 Indicador 2


# 1. Descarga de datos para Argentina
exportaciones_wdi <- WDI(country = "ARG",indicator = c("alta_tecnologia" = "TX.VAL.TECH.MF.ZS",
                                                       "manufacturas_total" = "TM.VAL.MANF.ZS.UN",
                                                       "alimentos_procesados" = "TX.VAL.FOOD.ZS.UN",
                                                       "agricolas_primarios" = "TX.VAL.AGRI.ZS.UN" ),
                         start = 1990,
                         end = 2025)


# 2. Pivoteo de la tabla de Argentina

exportaciones_wdi_pivot <- exportaciones_wdi %>%
  select(year, alta_tecnologia, alimentos_procesados, agricolas_primarios) %>%
  pivot_longer(cols = -year, names_to = "categoria", values_to = "porcentaje") %>%
  drop_na()

# 3. Gráfico 1: Evolución de la Canasta Exportadora de Argentina
ggplot(exportaciones_wdi_pivot,aes(x = year, y = porcentaje, color = categoria)) +
  geom_line(size = 1.2)  +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # Formato % en el eje Y
  labs(
    title = "Evolución de la Canasta Exportadora Argentina (WDI)",
    x = "Año",
    y = "Porcentaje del total exportado (%)",
    caption = "Fuente: Elaboración propia en base a World Development Indicators."
  ) +
  theme_minimal()


#VCR CON BACI


library(countrycode)   
library(writexl)

# base BACI 



baci <- read_csv("C:/Users/Julian Bruno/OneDrive/Escritorio/Materiales/Repositorio-Diplomatura/Julian Bruno/BACI_HS22_Y2024_V202601.csv")

#Detallamos el Pais
baci <- baci %>%
  mutate(iso3_exportador = countrycode(i, origin = "un", destination = "iso3c"))



# Filtrar la base de BACI para Argentina (i = 32)
baci_arg <- baci %>% 
  filter(i == 32)

# Ver las primeras filas
head(baci_arg)

exp_totales <- baci %>% group_by(i,k) %>% 
  summarise(v_pais_prod = sum(v,na.rm = TRUE), .groups = "drop")

# Totales por producto a nivel mundial
exp_mundo_prod <- exp_totales %>%
  group_by(k) %>%
  summarise(v_mundo_prod = sum(v_pais_prod, na.rm = TRUE), .groups = "drop")
# Total de exportaciones de todo el mundo
total_mundial_fob <- sum(exp_totales$v_pais_prod, na.rm = TRUE)

# Total de exportaciones de Argentina (Código ISO numérico: 32)
total_arg_fob <- exp_totales %>%
  filter(i == 32) %>%
  summarise(total = sum(v_pais_prod, na.rm = TRUE)) %>%
  pull(total)
# Índice de Balassa (VCR) para Argentina
vcr_arg_2024 <- exp_totales %>%
  filter(i == 32) %>%
  rename(v_arg_prod = v_pais_prod) %>%
  inner_join(exp_mundo_prod, by = "k") %>%
  mutate(
    share_arg = v_arg_prod / total_arg_fob,
    share_mundo = v_mundo_prod / total_mundial_fob,
    vcr = share_arg / share_mundo
  )

# 1. Filtrar los 15 productos con mayor Índice de Balassa (VCR) para Argentina en 2024
top15_arg_vcr <- vcr_arg_2024 %>%
  filter(vcr > 1) %>%
  arrange(desc(vcr)) %>%
  slice_head(n = 15)

# 2. Asignamos los nombres en español a las 15 posiciones arancelarias
top15_arg_vcr_completo <- top15_arg_vcr %>%
  mutate(nombre_producto = case_when(
    k == "320110" ~ "Extracto de quebracho (320110)",
    k == "150710" ~ "Aceite de soja en bruto (150710)",
    k == "090300" ~ "Yerba mate (090300)",
    k == "330113" ~ "Aceite esencial de limón (330113)",
    k == "150790" ~ "Otros aceites de soja refinados (150790)",
    k == "230250" ~ "Moyuelos y residuos de leguminosas (230250)",
    k == "230400" ~ "Harina y pellets de soja (230400)",
    k == "230800" ~ "Materias vegetales para alimentación animal (230800)",
    k == "230500" ~ "Tortas y residuos de la extracción de maní (230500)",
    k == "030474" ~ "Filetes congelados de merluza (030474)",
    k == "410449" ~ "Cueros bovinos secos/preparados (410449)",
    k == "120242" ~ "Maní crudo sin cáscara (120242)",
    k == "200939" ~ "Jugos de cítricos/agrios (200939)",
    k == "410441" ~ "Cueros bovinos plena flor (410441)",
    k == "285210" ~ "Compuestos inorgánicos de mercurio (285210)",
    TRUE ~ paste("Código HS:", k)
  ))

# 3. Generamos el gráfico de barras horizontales
ggplot(top15_arg_vcr_completo, aes(x = reorder(nombre_producto, vcr), y = vcr)) +
  geom_col(fill = "#2b5c8f", width = 0.7) +
  geom_text(aes(label = round(vcr, 1)), hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Top 15 Productos con Mayor VCR de Argentina (2024)",
    subtitle = "Especialización exportadora relativa según el Índice de Balassa",
    x = "Producto / Posición Arancelaria",
    y = "Índice VCR (Balassa)",
    caption = "Fuente: Elaboración propia en base a BACI (CEPII) e INDEC."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold")
  )
