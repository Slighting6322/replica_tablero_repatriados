## Módulo contenedor de tarjetas KPI (arreglo 2x3)
mod_kpi_cards_grid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "kpi-cards-grid",
    lapply(1:6, function(i) shiny::uiOutput(ns(paste0("card", i))))
  )
}

mod_kpi_cards_grid_server <- function(id, data_path = NULL, external_values = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    datos_reactivo <- shiny::reactive({
      # Si el usuario pasó external_values (reactive), intentar usarlo
      if (!is.null(external_values)) {
        ev <- tryCatch({ external_values() }, error = function(e) NULL)
        if (!is.null(ev)) {
          # aceptar data.frame con Categoria/Valor o una lista con vectores
          if (is.data.frame(ev) && all(c("Categoria","Valor") %in% names(ev))) {
            df <- ev
            df <- df[1:min(6, nrow(df)), , drop = FALSE]
            if (nrow(df) < 6) {
              missing <- 6 - nrow(df)
              df <- rbind(df, data.frame(Categoria = rep("-", missing), Valor = rep("-", missing), stringsAsFactors = FALSE))
            }
            return(df)
          }
          if (is.list(ev) && length(ev) >= 2) {
            # intentar construir data.frame
            cats <- as.character(ev$Categoria %||% ev[[1]])
            vals <- as.character(ev$Valor %||% ev[[2]])
            df <- data.frame(Categoria = cats, Valor = vals, stringsAsFactors = FALSE)
            df <- df[1:min(6, nrow(df)), , drop = FALSE]
            if (nrow(df) < 6) {
              missing <- 6 - nrow(df)
              df <- rbind(df, data.frame(Categoria = rep("-", missing), Valor = rep("-", missing), stringsAsFactors = FALSE))
            }
            return(df)
          }
        }
        # Si external_values no es válido, caer al CSV
      }

      # Si se proporcionó un path válido, intentar leer; si no, usar placeholders
      if (!is.null(data_path) && file.exists(data_path)) {
        datos <- tryCatch({
          df <- read.csv(data_path, stringsAsFactors = FALSE)
          if (!all(c("Categoria", "Valor") %in% names(df))) stop("Faltan columnas requeridas en el archivo de datos de KPIs")
          n <- pmin(6, nrow(df))
          if (n <= 0) return(data.frame(Categoria = rep("-", 6), Valor = rep("-", 6), stringsAsFactors = FALSE))
          df[seq_len(n), , drop = FALSE]
        }, error = function(e) {
          message("mod_kpi_cards_grid_server: error leyendo data_path: ", e$message)
          data.frame(Categoria = rep("-", 6), Valor = rep("-", 6), stringsAsFactors = FALSE)
        })
      } else {
        # No hay archivo de datos; devolver placeholders (las tarjetas pueden ser rellenadas por external_values en montaje)
        datos <- data.frame(Categoria = rep("-", 6), Valor = rep("-", 6), stringsAsFactors = FALSE)
      }
      datos <- datos[1:6, , drop = FALSE]
      datos
    })

    # Renderizar las 6 tarjetas desde datos_reactivo
    for (i in 1:6) {
      local({
        idx <- i
        output[[paste0("card", idx)]] <- shiny::renderUI({
          datos <- datos_reactivo()
          mod_kpi_card_ui(ns(paste0("kpi", idx)), label = datos$Categoria[idx], value = datos$Valor[idx])
        })
      })
    }
  })
}