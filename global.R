# global.R
# Este archivo se ejecuta una sola vez al iniciar la aplicación.
# Úsalo para:
# 1. Cargar todas las librerías necesarias para la app.
# 2. Cargar datos que no cambian (ej. leer un CSV grande).
# 3. Definir funciones de ayuda o variables globales.
# 4. "Source" (cargar) los archivos de la carpeta R/ que contienen tus módulos.

# Carga de librerías principales
library(shiny)
library(htmlwidgets) # Fundamental para la integración con JavaScript
library(readr)       # Para leer archivos CSV de forma eficiente

# Carga de datos iniciales
# Leemos el archivo CSV y lo almacenamos en un dataframe global.
# Usamos `tryCatch` para manejar el caso en que el archivo no exista.
datos_repatriados <- tryCatch({
  # Se añade `show_col_types = FALSE` para evitar el mensaje en la consola.
  read_csv("data/repatriados_sample.csv", show_col_types = FALSE)
}, error = function(e) {
  # Si hay un error (ej. el archivo no se encuentra), crea un dataframe vacío.
  # Esto previene que la app falle al iniciar si los datos no están.
  data.frame()
})


# (Opcional) Cargar todos los módulos de la carpeta R/ automáticamente
# purrr::walk(list.files("R", full.names = TRUE), source)
