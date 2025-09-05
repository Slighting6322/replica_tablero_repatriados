# server.R
# Lógica del servidor con el nuevo enfoque de htmlTemplate.

source("global.R", local = TRUE)

# Cargar todos los módulos R (si existen) para que sus funciones mod_*_server estén disponibles
mods_dir <- "R/modules"
if (dir.exists(mods_dir)) {
  files <- list.files(mods_dir, pattern = "^mod_.*\\.R$", full.names = TRUE)
  for (f in files) try(source(f, local = TRUE), silent = TRUE)
}

function(input, output, session) {

  # Reactive con la fecha de corte (definida en global.R)
  fecha_reactivo <- reactive({
    # fecha_corte debe venir de global.R (Date/POSIX). Fallback a Sys.Date() si falta.
    if (exists("fecha_corte") && !is.null(fecha_corte)) fecha_corte else Sys.Date()
  })

  # Renderizar el texto de la fecha de corte (formato en español)
  output$fecha_corte_texto <- renderText({
    # Asegurar formato en español; si el servidor no tiene locale, el nombre del mes puede salir en inglés
    # Sys.setlocale("LC_TIME", "es_ES.UTF-8") # activar si es necesario en tu entorno
    format(fecha_reactivo(), "%d de %B de %Y")
  })

  # Montar servidores de módulos si existen (ids deben coincidir con ui.R: "home1", "origen1")
  if (exists("mod_home_server")) {
    try(mod_home_server("home1", fecha_reactivo = fecha_reactivo), silent = TRUE)
  }
  if (exists("mod_origen_server")) {
    try(mod_origen_server("origen1", fecha_reactivo = fecha_reactivo), silent = TRUE)
  }
  # Si tienes un módulo de fecha reutilizable, puedes montarlo también (ejemplo):
  if (exists("mod_fecha_server")) {
    # No usar el mismo id que el placeholder principal; usarlo en submódulos cuando haga falta
    # try(mod_fecha_server("fecha1", fecha_reactivo = fecha_reactivo), silent = TRUE)
  }

  # Aquí puedes añadir otras inicializaciones de servidor / observers globales
}