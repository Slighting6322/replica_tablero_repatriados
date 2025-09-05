## Module: mod_home.R
# Minimal home page module. Shows a heading and a small KPI placeholder.
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "home-module",
        h2("Resumen general"),
        p("KPI: ", textOutput(ns("kpi_total"), inline = TRUE)),
        p("Fecha de corte: ", textOutput(ns("fecha"), inline = TRUE))
    )
  )
}

mod_home_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    # Render a small KPI from global data if available
    output$kpi_total <- renderText({
      if (exists("repatriados_data")) {
        nrow(repatriados_data)
      } else {
        "N/A"
      }
    })

    # Launch fecha submodule
    if (exists("mod_fecha_server")) {
      mod_fecha_server("fecha", fecha_reactivo = fecha_reactivo)
    }
  })
}
