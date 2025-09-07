## Module: mod_origen.R
# Minimal 'origen' page module. Keep layout similar to original origen_destino.html
mod_origen_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "origen-module",
        div(class = "origen-placeholder", "")
    )
  )
}

mod_origen_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    # No definir output$fecha_origen aquí; lo maneja el módulo de fecha central
    # Puedes agregar aquí lógica específica de la sección origen si lo necesitas
  })
}
