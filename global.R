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

# Procesar la fecha de corte
fecha_corte <- repatriados_data %>%
  summarise(max_date = max(fecha_repatriacion, na.rm = TRUE)) %>%
  pull(max_date)

# Formatear la fecha para mostrarla (ej: 05 de septiembre de 2025)
# Sys.setlocale("LC_TIME", "es_ES.UTF-8") # Descomentar si el mes no sale en español en el servidor
fecha_corte_texto <- format(fecha_corte, "%d de %B de %Y")
