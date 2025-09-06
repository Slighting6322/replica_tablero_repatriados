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
# Cargar los datos (usar data_loader centralizado)
if (file.exists("R/data_loader.R")) source("R/data_loader.R", local = TRUE)
repatriados_data <- get_repatriados_data("data/repatriados_sample.csv")
# Procesar la fecha de corte (si existe la columna fecha_repatriacion)
# Intentar calcular la fecha de corte usando, por prioridad:
# 1) columna llamada 'fecha_repatriacion' si existe
# 2) la primera columna del data.frame
# Se hacen varios intentos de parseo de fechas antes de caer a Sys.Date().
fecha_corte <- tryCatch({
  col_name <- if ("fecha_repatriacion" %in% names(repatriados_data)) {
    "fecha_repatriacion"
  } else if (ncol(repatriados_data) >= 1) {
    names(repatriados_data)[1]
  } else {
    NULL
  }

  if (is.null(col_name)) stop("no date column found")

  vec <- repatriados_data[[col_name]]

  # if already Date/POSIX, handle directly
  if (inherits(vec, c("Date", "POSIXt"))) {
    max(vec, na.rm = TRUE)
  } else {
    # Try several parsers
    parsed <- suppressWarnings(as.Date(vec))
    if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd(vec))
    if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(vec))
    if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(vec))
    # fallback: try parsing with anytime if available
    if (exists("parsed") && all(is.na(parsed)) && requireNamespace("anytime", quietly = TRUE)) {
      parsed <- suppressWarnings(anytime::anydate(vec))
    }
    parsed <- as.Date(parsed)
    if (all(is.na(parsed))) stop("could not parse date column")
    max(parsed, na.rm = TRUE)
  }
}, error = function(e) {
  # No mostrar una fecha alternativa si falla el parseo: devolver NA
  as.Date(NA)
})
