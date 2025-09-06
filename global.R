# global.R
# Este archivo se ejecuta una sola vez al iniciar la aplicación.
# Úsalo para:
# 1. Cargar todas las librerías necesarias para la app.
# 2. Cargar datos que no cambian (ej. leer un CSV grande).
# 3. Definir funciones de ayuda o variables globales.
# 4. "Source" (cargar) los archivos de la carpeta R/ que contienen tus módulos.

# Carga de librerías principales
# Dependencias
library(shiny)
library(readr)
library(dplyr)
library(lubridate)

# global.R
# Este archivo define la función `init_app_data()` que carga los datos
# necesarios y devuelve una lista con `repatriados_data` y `fecha_corte`.
# No debe realizar efectos secundarios al ser `source()`d: el llamador decide
# cuándo ejecutar la inicialización y dónde asignar las variables.

# Cargar utilidades de datos
if (file.exists("R/data_loader.R")) source("R/data_loader.R")

# init_app_data: carga y parsea el dataset, devuelve lista con datos y fecha_corte
init_app_data <- function(path = "data/repatriados_sample.csv") {
  repatriados_data <- tryCatch(get_repatriados_data(path), error = function(e) tibble::tibble())

  # Determinar la columna de fecha y parsear robustamente
  fecha_corte <- tryCatch({
    if (nrow(repatriados_data) == 0 || ncol(repatriados_data) == 0) return(as.Date(NA))
    col_name <- if ("fecha_repatriacion" %in% names(repatriados_data)) "fecha_repatriacion" else names(repatriados_data)[1]
    vec <- repatriados_data[[col_name]]
    parse_try <- function(x) {
      p <- suppressWarnings(as.Date(x))
      if (all(is.na(p))) p <- suppressWarnings(lubridate::ymd(x))
      if (all(is.na(p))) p <- suppressWarnings(lubridate::dmy(x))
      if (all(is.na(p))) p <- suppressWarnings(lubridate::mdy(x))
      if (all(is.na(p)) && requireNamespace("anytime", quietly = TRUE)) p <- suppressWarnings(anytime::anydate(x))
      as.Date(p)
    }
    if (inherits(vec, c("Date", "POSIXt"))) {
      max(vec, na.rm = TRUE)
    } else {
      parsed <- parse_try(vec)
      if (all(is.na(parsed))) return(as.Date(NA))
      max(parsed, na.rm = TRUE)
    }
  }, error = function(e) as.Date(NA))

  list(repatriados_data = repatriados_data, fecha_corte = fecha_corte)
}
