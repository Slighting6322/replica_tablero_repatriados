## Módulo de tarjeta KPI individual
mod_kpi_card_ui <- function(id, label = NULL, value = NULL) {
  ns <- shiny::NS(id)
  shiny::div(class = "kpi-card",
    shiny::div(class = "kpi-value", value),
    shiny::div(class = "kpi-label", label)
  )
}

mod_kpi_card_server <- function(id, label, value) {
  shiny::moduleServer(id, function(input, output, session) {
    # No reactivo, solo UI
  })
}