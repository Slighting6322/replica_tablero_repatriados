## Módulo contenedor de tarjetas KPI (arreglo 2x3)
mod_kpi_cards_grid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "kpi-cards-grid",
    lapply(1:6, function(i) shiny::uiOutput(ns(paste0("card", i))))
  )
}

mod_kpi_cards_grid_server <- function(id, data_path = "data/datos_tarjeta.csv") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Leer datos
    datos <- tryCatch({
      df <- read.csv(data_path, stringsAsFactors = FALSE)
      if (!all(c("Categoria", "Valor") %in% names(df))) stop("Faltan columnas requeridas")
      df
    }, error = function(e) data.frame(Categoria = rep("-", 6), Valor = rep("-", 6)))
    # Limitar a 6
    datos <- datos[1:6, ]
    for (i in 1:6) {
      local({
        idx <- i
        output[[paste0("card", idx)]] <- shiny::renderUI({
          mod_kpi_card_ui(ns(paste0("kpi", idx)), label = datos$Categoria[idx], value = datos$Valor[idx])
        })
      })
    }
  })
}