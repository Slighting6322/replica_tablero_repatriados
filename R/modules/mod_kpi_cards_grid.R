## Módulo contenedor de tarjetas KPI (arreglo 2x3)
mod_kpi_cards_grid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "kpi-cards-grid",
    lapply(1:6, function(i) shiny::uiOutput(ns(paste0("card", i))))
  )
}

mod_kpi_cards_grid_server <- function(id, data_path = "data/datos_tarjeta.csv", external_values = NULL) {
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

      datos <- tryCatch({
        df <- read.csv(data_path, stringsAsFactors = FALSE)
        if (!all(c("Categoria", "Valor") %in% names(df))) stop("Faltan columnas requeridas")
        df
      }, error = function(e) data.frame(Categoria = rep("-", 6), Valor = rep("-", 6), stringsAsFactors = FALSE))
      datos <- datos[1:6, ]
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