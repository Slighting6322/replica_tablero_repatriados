## Module: mod_fecha.R
# Reusable fecha module
mod_fecha_ui <- function(id, inline = TRUE) {
  ns <- NS(id)
  textOutput(ns("fecha"), inline = inline)
}

mod_fecha_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    # support both reactive and static date inputs
    get_fecha <- function() {
      if (is.reactive(fecha_reactivo)) fecha_reactivo() else fecha_reactivo
    }

    output$fecha <- renderText({
      f <- get_fecha()
      if (is.null(f) || is.na(f)) return("")
      # formato seguro: no forzamos locale globalmente para evitar efectos secundarios
      tryCatch({
        format(as.Date(f), "%d de %B de %Y")
      }, error = function(e) {
        as.character(f)
      })
    })
  })
}
