## Module: mod_home.R
# Minimal home page module. Shows a heading and a small KPI placeholder.
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
  div(class = "home-module",
    # Contenido principal de la sección inicio — manteniendo un placeholder vacío
    div(class = "home-placeholder", "")
  )
  )
}

mod_home_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
  # Home no renderiza KPI ni fecha por diseño; mantener vacío para inyección futura
  })
}
