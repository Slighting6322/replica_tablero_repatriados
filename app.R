## app.R - Entrypoint para la aplicación Shiny
# Fuente: crea la app a partir de los archivos `global.R`, `ui.R` y `server.R` existentes.

library(shiny)

# Cargar global.R si existe (contiene carga de datos y dependencias)
if (file.exists("global.R")) source("global.R")

# Inicializar datos y exponer variables globales
if (exists("init_app_data")) {
	app_data <- init_app_data()
	# asignar en el entorno global para que ui/server los puedan usar
	assign("repatriados_data", app_data$repatriados_data, envir = .GlobalEnv)
	assign("fecha_corte", app_data$fecha_corte, envir = .GlobalEnv)
} else {
	warning("init_app_data() not found; global variables not initialized")
}

# Cargar UI y servidor
if (file.exists("ui.R")) source("ui.R") else stop("ui.R not found")
if (file.exists("server.R")) source("server.R") else stop("server.R not found")

# 'ui' and 'server' deben quedar definidos por los archivos sourceados
if (!exists("ui") || !exists("server")) stop("ui or server not defined after sourcing files")

shinyApp(ui = ui, server = server)
