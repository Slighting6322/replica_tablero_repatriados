## Module: mod_origen.R
# Minimal 'origen' page module. Keep layout similar to original origen_destino.html
mod_origen_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "origen-module",
        # Sección 'origen' mínima: todo lo visible especificado fue removido por petición
        div(class = "origen-placeholder", "")
    )
  )
}

mod_origen_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    # Módulo 'origen' vacío: filtros, fecha y placeholders fueron removidos por petición.
  })
}
