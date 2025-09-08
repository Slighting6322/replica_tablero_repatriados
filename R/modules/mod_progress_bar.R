## Module: mod_progress_bar.R
# Barra de progreso reutilizable para mostrar ocupación (personas alojadas vs lugares disponibles)
# Lee por defecto un CSV con dos columnas: actual y total (solo usa la primera fila).
# Parámetros: id (obligatorio), path (opcional), titulo (opcional), mostrar_texto (TRUE/FALSE)

mod_progress_bar_ui <- function(id, titulo = NULL, mostrar_texto = TRUE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "ocupacion-progress-wrapper",
        if (!is.null(titulo)) shiny::h3(class = "ocupacion-progress-title", titulo),
        # Contenedor que se actualizará dinámicamente
        shiny::uiOutput(ns("progress_container"))
    )
  )
}

mod_progress_bar_server <- function(id, path = "data/barra_personas.csv", formato_num = function(x) format(x, big.mark = ",", scientific = FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {

  datos_reactivo <- shiny::reactive({
      df <- tryCatch({
        if (!file.exists(path)) stop("Archivo no encontrado: ", path)
        suppressWarnings(read.csv(path, header = TRUE, check.names = FALSE, strip.white = TRUE))
      }, error = function(e) {
        return(data.frame(.error = e$message, stringsAsFactors = FALSE))
      })

      if (".error" %in% names(df)) return(list(error = df$.error[1]))
      if (nrow(df) == 0 || ncol(df) < 2) return(list(error = "El archivo no tiene suficientes columnas/filas"))

      # Tomar la primera fila y las dos primeras columnas
      actual_raw <- df[[1]][1]
      total_raw  <- df[[2]][1]
      actual <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(actual_raw))))
      total  <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(total_raw))))
      if (is.na(actual) || is.na(total) || total <= 0) {
        return(list(error = "Datos inválidos (NA o total <= 0)"))
      }
      porcentaje <- pmin(100, (actual / total) * 100)
      list(actual = actual, total = total, porcentaje = porcentaje)
    })

  output$progress_container <- shiny::renderUI({
      d <- datos_reactivo()
      if (!is.null(d$error)) {
  return(shiny::div(class = "progress-error", paste("No disponible:", d$error)))
      }
      pct <- round(d$porcentaje, 1)
      actual_fmt <- formato_num(d$actual)
      total_fmt  <- formato_num(d$total)
      # Texto solo con porcentaje para accesibilidad dentro (screen reader) y número principal debajo
      shiny::tagList(
        shiny::div(class = "progress-bar-outer", 
            shiny::div(class = "progress-bar-inner", style = paste0("width:", pct, "%;"), 
                `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100,
                role = "progressbar",
                shiny::span(class = "sr-only", paste0(pct, "%"))
            )
        ),
        shiny::div(class = "progress-numeros",
          shiny::div(class = "progress-col progress-col-actual",
            shiny::span(class = "progress-actual", actual_fmt),
            shiny::div(class = "progress-label", "Personas Alojadas")
          ),
          shiny::div(class = "progress-col progress-col-total",
            shiny::span(class = "progress-total", total_fmt),
            shiny::div(class = "progress-label", "Lugares Disponibles")
          )
        )
      )
    })
  })
}
