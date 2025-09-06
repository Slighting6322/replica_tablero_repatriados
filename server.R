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

function(input, output, session) {

  # Reactive con la fecha de corte (definida en global.R)
  fecha_reactivo <- reactive({
    # fecha_corte debe venir de global.R (Date/POSIX). Fallback a Sys.Date() si falta.
    if (exists("fecha_corte") && !is.null(fecha_corte)) fecha_corte else Sys.Date()
  })

  # Renderizar el texto de la fecha de corte (formato en español)
  # Use central mod_fecha for formatted outputs
  if (exists("mod_fecha_server")) {
    tryCatch({
      mod_fecha_server("fecha_home", fecha_reactivo = fecha_reactivo)
      mod_fecha_server("fecha_origen", fecha_reactivo = fecha_reactivo)
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