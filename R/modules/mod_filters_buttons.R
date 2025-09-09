## Módulo: botones (buscar, refrescar)
mod_filters_buttons_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "filter-buttons",
    shiny::actionButton(ns("buscar"), "Buscar", class = "btn-primary"),
    # Use an inline SVG for the refresh icon; keep aria-label for screen readers
    shiny::actionButton(ns("refrescar"), label = shiny::tags$span(
      shiny::tags$svg(xmlns = "http://www.w3.org/2000/svg", width = "14", height = "14", viewBox = "0 0 24 24", fill = "none", stroke = "currentColor", `stroke-width` = "2", `stroke-linecap` = "round", `stroke-linejoin` = "round",
        shiny::tags$path(d = "M23 4v6h-6"),
        shiny::tags$path(d = "M20.49 15A9 9 0 1 1 23 8.5")
      ),
      class = "refresh-icon"
    ), class = "btn-secondary", `aria-label` = "Refrescar")
  )
}

mod_filters_buttons_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(buscar = shiny::reactive({ input$buscar }), refrescar = shiny::reactive({ input$refrescar }))
  })
}
