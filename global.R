# global.R
# Este archivo se ejecuta una sola vez al iniciar la aplicación.
# Úsalo para:
# 1. Cargar todas las librerías necesarias para la app.
# 2. Cargar datos que no cambian (ej. leer un CSV grande).
# 3. Definir funciones de ayuda o variables globales.
# 4. "Source" (cargar) los archivos de la carpeta R/ que contienen tus módulos.

# Carga de librerías principales
library(shiny)
library(readr)
library(dplyr)
library(lubridate)

# Cargar los datos
repatriados_data <- read_csv("data/repatriados_sample.csv")
# Procesar la fecha de corte (si existe la columna fecha_repatriacion)
if ("fecha_repatriacion" %in% names(repatriados_data)) {
  fecha_corte <- repatriados_data %>%
    summarise(max_date = max(fecha_repatriacion, na.rm = TRUE)) %>%
    pull(max_date)
} else {
  # fallback: usar la fecha del sistema
  fecha_corte <- Sys.Date()
}
