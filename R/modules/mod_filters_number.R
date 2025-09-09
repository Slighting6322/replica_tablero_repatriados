## Módulo: number input filter (frontend only)
mod_filters_number_ui <- function(id, label = NULL, placeholder = NULL) {
  ns <- shiny::NS(id)
  shiny::div(class = "filter-number",
    if (!is.null(label)) shiny::div(class = "filter-label", label),
    shiny::numericInput(ns("num"), label = NULL, value = NA, min = NA, max = NA, width = "100%")
  )
}

mod_filters_number_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({ input$num })
  })
}
