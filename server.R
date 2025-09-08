# server.R
# Lógica del servidor con el nuevo enfoque de htmlTemplate.
# Note: global variables (repatriados_data, fecha_corte) are initialized by app.R via init_app_data()

# Cargar todos los módulos R (si existen) para que sus funciones mod_*_server estén disponibles
mods_dir <- "R/modules"
if (dir.exists(mods_dir)) {
  files <- list.files(mods_dir, pattern = "^mod_.*\\.R$", full.names = TRUE)
  for (f in files) {
    tryCatch({
      source(f)
      message("[server] sourced module: ", basename(f))
    }, error = function(e) {
      message("Error sourcing module file ", f, ": ", e$message)
    })
  }
}

# Validar que `fecha_corte` exista y sea una Date válida en el entorno global antes de levantar el server.
if (!exists("fecha_corte")) {
  stop("[server.R] Fecha de corte ('fecha_corte') no encontrada en el entorno global. Asegúrate de ejecutar app.R que inicializa los datos.")
}
if (is.null(fecha_corte) || is.na(fecha_corte) || !inherits(fecha_corte, "Date")) {
  stop(sprintf("[server.R] Fecha de corte inválida: %s (clase: %s). La app requiere una fecha de corte válida en global.R/app.R.",
               paste0(capture.output(str(fecha_corte)), collapse = " "), class(fecha_corte)))
}

server <- function(input, output, session) {

  # Reactive con la fecha de corte (definida en global.R)
  fecha_reactivo <- reactive({
    # No usar fallback silencioso a Sys.Date(): exigir fecha válida y detener si falta.
    if (!exists("fecha_corte")) {
      stop("[server.R] fecha_corte no encontrada en el entorno global. Ejecuta la app via app.R para inicializar datos.")
    }
    if (is.null(fecha_corte) || is.na(fecha_corte) || !inherits(fecha_corte, "Date")) {
      stop("[server.R] fecha_corte inválida: se requiere una Date válida en global.R/app.R. La ejecución se detiene.")
    }
    fecha_corte
  })

  # Renderizar el texto de la fecha de corte (formato en español)
  # Use central mod_fecha for formatted outputs
  if (exists("mod_fecha_server")) {
    tryCatch({
      mod_fecha_server("fecha_home", fecha_reactivo = fecha_reactivo)
      mod_fecha_server("fecha_origen", fecha_reactivo = fecha_reactivo)
      # Forzar que el output no se suspenda aunque la sección esté oculta
      tryCatch({
        outputOptions(output, "fecha_origen-fecha", suspendWhenHidden = FALSE)
      }, error = function(e) {
        message(sprintf("[server] outputOptions error para fecha_origen-fecha: %s", e$message))
      })
      message("[server] mounted mod_fecha for home and origen")
    }, error = function(e) message("Error mounting mod_fecha: ", e$message))
  } else {
    # Fallback: simple renderText for template placeholders
    output$fecha_corte_texto_home <- renderText({
      f <- fecha_reactivo()
      if (is.null(f) || is.na(f)) return("")
      format(f, "%d de %B de %Y")
    })
    output$fecha_corte_texto_origen <- renderText({
      f <- fecha_reactivo()
      if (is.null(f) || is.na(f)) return("")
      format(f, "%d de %B de %Y")
    })
  }

  # Montar servidores de módulos si existen (ids deben coincidir con ui.R: "home1", "origen1")
  if (exists("mod_home_server")) {
    tryCatch(
      mod_home_server("home1", fecha_reactivo = fecha_reactivo),
      error = function(e) message("mod_home_server error: ", e$message)
    )
  }
  # Montar tarjetas KPI si el módulo existe
  if (exists("mod_kpi_cards_grid_server")) {
    tryCatch(
      mod_kpi_cards_grid_server("kpi_grid1"),
      error = function(e) message("mod_kpi_cards_grid_server error: ", e$message)
    )
  }
  # Montar barra de progreso de ocupación si el módulo existe
  if (exists("mod_progress_bar_server")) {
    tryCatch(
      mod_progress_bar_server("ocupacion_bar1"),
      error = function(e) message("mod_progress_bar_server error: ", e$message)
    )
  }
  if (exists("mod_origen_server")) {
    tryCatch(
      mod_origen_server("origen1", fecha_reactivo = fecha_reactivo),
      error = function(e) message("mod_origen_server error: ", e$message)
    )
  }
  # Si tienes un módulo de fecha reutilizable, puedes montarlo también (ejemplo):
  if (exists("mod_fecha_server")) {
    # No usar el mismo id que el placeholder principal; usarlo en submódulos cuando haga falta
    # try(mod_fecha_server("fecha1", fecha_reactivo = fecha_reactivo), silent = TRUE)
  }

  # Aquí puedes añadir otras inicializaciones de servidor / observers globales
}