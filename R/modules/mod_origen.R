## Module: mod_origen.R
# Minimal 'origen' page module. Keep layout similar to original origen_destino.html
mod_origen_ui <- function(id) {
  ns <- NS(id)
  tagList(
  div(class = "origen-module",
    h2("Deportaciones desde EEUU"),
    p(class = "page-subtitle", "Fecha de corte: ", textOutput(ns("fecha"), inline = TRUE)),
    div(id = ns("mapa_origen"), "[Aquí irá mapa/gráfica de origen-destino]")
  )
  )
}

mod_origen_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    # iniciar submódulo fecha
    if (exists("mod_fecha_server")) {
      mod_fecha_server("fecha", fecha_reactivo = fecha_reactivo)
    }

    # lógica específica de origen (placeholder)
    # por ejemplo, podríamos exponer un output reactivo o renderUI aquí
  })
}
