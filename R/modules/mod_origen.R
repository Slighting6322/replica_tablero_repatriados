## Module: mod_origen.R
# Minimal 'origen' page module. Keep layout similar to original origen_destino.html
mod_origen_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "origen-module",
        # Sección 'origen' mínima: solo el placeholder y la fecha (si se desea mostrar)
  p(class = "page-subtitle", "Fecha de corte: ",
    # container must handle both ways Shiny may call it:
    # 1) container(content) -> produce <span>content</span>
    # 2) container(outputId, class = ..., ...) -> produce <span id=outputId class=... />
    textOutput(ns("fecha_origen"), container = function(...) {
      args <- list(...)
      nm <- names(args)
      # Called as container(content)
      if (is.null(nm) || all(nm == "")) {
        x <- args[[1]]
        return(shiny::tags$span(x, class = "fecha-origen-inline"))
      }
      # Called with named args like outputId/class
      outputId <- if (!is.null(args$outputId)) args$outputId else args[[1]]
      classArg <- if (!is.null(args$class)) args$class else NULL
      shiny::tags$span(id = outputId, class = paste(classArg, "fecha-origen-inline"))
    })
  ),
        div(class = "origen-placeholder", "")
    )
  )
}

mod_origen_server <- function(id, fecha_reactivo) {
  moduleServer(id, function(input, output, session) {
    # Debug: observar fecha_reactivo dentro de un contexto reactivo y registrar (una sola vez)
    observeEvent(fecha_reactivo(), {
      fr <- fecha_reactivo()
      invisible(NULL)
    }, once = TRUE, ignoreNULL = FALSE)

    # Renderizar la fecha local namespaced para esta sección
      # Bridge reactivo: usar reactiveValues para forzar render cuando cambie fecha_reactivo()
      rv <- reactiveValues(fecha = NULL)

      observeEvent(fecha_reactivo(), {
        fr <- fecha_reactivo()
        rv$fecha <- fr
      }, ignoreNULL = FALSE)

      output$fecha_origen <- renderText({
      tryCatch({
          f <- rv$fecha
  if (is.null(f) || is.na(f)) return("")
  formatted <- format(f, "%d de %B de %Y")
  formatted
      }, error = function(e) {
        message(sprintf("[mod_origen] renderText error: %s", e$message))
        ""
      })
    })
    # Evitar que Shiny suspenda automáticamente este output cuando la sección esté oculta
    tryCatch({
      outputOptions(output, "fecha_origen", suspendWhenHidden = FALSE)
    }, error = function(e) {
      message(sprintf("[mod_origen] unable to set outputOptions: %s", e$message))
    })
  })
}
