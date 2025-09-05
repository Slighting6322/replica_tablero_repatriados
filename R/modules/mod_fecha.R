## Module: mod_fecha.R
# UI + server to render a formatted "fecha de corte" string
mod_fecha_ui <- function(id, inline = TRUE) {
  ns <- NS(id)
  textOutput(ns("fecha"), inline = inline)
}

mod_fecha_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    output$fecha <- renderText({
      f <- if (is.reactive(fecha_reactivo)) fecha_reactivo() else fecha_reactivo
      # Intentar formatear en español; si el locale no está disponible, caer al formato por defecto
      res <- tryCatch({
        old <- Sys.getlocale("LC_TIME")
        # Algunos sistemas no tienen "es_ES.UTF-8"; el tryCatch evita fallos
        Sys.setlocale("LC_TIME", "es_ES.UTF-8")
        out <- format(as.Date(f), "%d de %B de %Y")
        # restaurar locale anterior si fue obtenido
        if (!is.na(old)) Sys.setlocale("LC_TIME", old)
        out
      }, error = function(e) {
        format(as.Date(f), "%d de %B de %Y")
      })
      res
    })
  })
}
