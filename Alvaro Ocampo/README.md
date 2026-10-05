# Alvaro Miguel Ocampo

**Subtema:** Desindustrialización y especialización comercial.

Trabajo integrador final de la Diplomatura en Programación en R Aplicada a la Economía (FCE-UBA).

## Contenido de la carpeta

- `informe.qmd`: informe en Quarto con texto, código y visualizaciones. Se renderiza a `informe.html`.
- `Ocampo_Alvaro.Rproj`: proyecto de RStudio. Todas las rutas son relativas a esta carpeta.
- `bases/`: bases de datos que usa el informe.
  - `pib_industrial_mundial.csv`: PIB industrial por país, 1970-2023, precios constantes de 2015 (Argendata, Fundar, sobre UNSTATS).
  - `poblacion.csv`: población por país (UNSTATS, National Accounts).
  - `expo_2024_paises.csv`: exportaciones 2024 por producto HS6 de los nueve países del trabajo, con totales por país, producto y mundo (recorte de BACI).
  - `ubicuidad_2024.csv`: cantidad de países que exportan cada producto con VCR mayor a 1 (calculado sobre BACI completa).
- `graficos/`: los dos gráficos del informe, que el propio código exporta en png.

Los dos recortes de BACI se obtuvieron de la base BACI HS22 2024 (CEPII, versión 202601), que pesa más de 350 MB y no está en el repositorio. Se descarga de http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37.

## Paquetes

tidyverse, knitr, scales, ggrepel.
