## Módulo: dropdown filter (frontend only placeholder)
mod_filters_dropdown_ui <- function(id, label = NULL, choices = NULL, placeholder = "Seleccionar...") {
  ns <- shiny::NS(id)
  shiny::div(class = "filter-dropdown",
    if (!is.null(label)) shiny::div(class = "filter-label", label),
  shiny::selectInput(ns("select"), label = NULL, choices = if (is.null(choices)) list(placeholder) else choices, selected = NULL, width = "100%")
  )
}

mod_filters_dropdown_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # placeholder: returns reactive value of selection
    shiny::reactive({ input$select })
  })
}
