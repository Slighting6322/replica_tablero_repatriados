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
  # Preferir XLSX como fuente canónica para fecha_corte
  xlsx_path <- "data/repatriados.xlsx"
  repatriados_data <- NULL
  fecha_corte <- as.Date(NA)

  # Normalizar nombres de columna para matching (quitar espacios, signos, y pasar a minúsculas)
  clean_name <- function(x) {
    x2 <- iconv(x, to = "ASCII//TRANSLIT")
    x2 <- tolower(x2)
    x2 <- gsub("[^a-z0-9]", "", x2)
    x2
  }

  parse_dates <- function(vec) {
    # si ya son Date/POSIX, convertir
    if (inherits(vec, c("Date", "POSIXt"))) return(as.Date(vec))
    # usar lubridate parse_date_time con varios formatos comunes
    orders <- c("Ymd", "ymd", "dmy", "mdy", "Ymd HMS", "ymd HMS", "dmy HMS", "mdy HMS")
    parsed <- suppressWarnings(lubridate::parse_date_time(vec, orders = orders, tz = "UTC"))
    # fallback a anytime si está disponible
    if (all(is.na(parsed)) && requireNamespace("anytime", quietly = TRUE)) {
      parsed <- suppressWarnings(anytime::anytime(vec))
    }
    as.Date(parsed)
  }

  # 1) Intentar leer XLSX y tomar FECHA DE REPATRIACION (hoja 'Repatriados' esperada)
  if (file.exists(xlsx_path) && requireNamespace("readxl", quietly = TRUE)) {
    df_x <- tryCatch(readxl::read_excel(xlsx_path, sheet = "Repatriados", col_types = "text"), error = function(e) NULL)
    if (!is.null(df_x) && nrow(df_x) > 0) {
      nm <- names(df_x)
      nm_clean <- vapply(nm, clean_name, character(1))

      # buscar posibles variantes para 'fecha de repatriacion'
      target_candidates <- c("fechaderepatriacion", "fecharepatriacion", "fecha")
      match_idx <- which(nm_clean %in% target_candidates)
      # si no hay coincidencias exactas, intentar búsqueda por subcadena
      if (length(match_idx) == 0) {
        match_idx <- grep("fecha.*repatriaci", nm_clean)
      }
      if (length(match_idx) >= 1) {
        # preferir la primera coincidencia encontrada
        col_name <- nm[match_idx[1]]
        vec <- df_x[[col_name]]
        parsed <- parse_dates(vec)
        if (!all(is.na(parsed))) {
          fecha_corte <- suppressWarnings(max(parsed, na.rm = TRUE))
          message(sprintf("[init_app_data] fecha_corte detectada desde '%s': %s", col_name, as.character(fecha_corte)))
        } else {
          message("[init_app_data] Se encontró columna de fecha pero no pudo parsearse con los formatos conocidos.")
        }
      } else {
        message("[init_app_data] No se encontró una columna de fecha en la hoja 'Repatriados' (buscando 'FECHA DE REPATRIACION').")
      }
      # Asignar repatriados_data (todas las columnas leídas como texto)
      repatriados_data <- df_x
    }
  }

  # Requerir que la fecha_corte haya sido obtenida del XLSX; sin XLSX válido, detener la inicialización
  if (is.na(fecha_corte) || is.null(repatriados_data) || nrow(repatriados_data) == 0) {
    stop("[init_app_data] No se encontró un XLSX válido en 'data/repatriados.xlsx' con una columna de FECHA DE REPATRIACION. La inicialización requiere ese archivo.")
  }

  list(repatriados_data = repatriados_data, fecha_corte = fecha_corte)
}
