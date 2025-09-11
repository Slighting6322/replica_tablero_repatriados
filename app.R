## app.R - Entrypoint para la aplicación Shiny
# Fuente: crea la app a partir de los archivos `global.R`, `ui.R` y `server.R` existentes.

library(shiny)

# Cargar global.R si existe (contiene carga de datos y dependencias)
if (file.exists("global.R")) source("global.R")

# Inicializar datos y exponer variables globales
if (exists("init_app_data")) {
	app_data <- init_app_data()
	# Validar que la fecha de corte sea válida (no NA, no Inf)
	if (is.null(app_data$fecha_corte) || is.na(app_data$fecha_corte) || is.infinite(app_data$fecha_corte)) {
		stop("[app.R] Error: No se pudo determinar una fecha de corte válida a partir del archivo de datos. El proceso se detiene.")
	}
	# asignar en el entorno global para que ui/server los puedan usar
	assign("repatriados_data", app_data$repatriados_data, envir = .GlobalEnv)
	assign("fecha_corte", app_data$fecha_corte, envir = .GlobalEnv)

	# Verificar que la fecha asignada coincide con la calculada directamente del CSV
	# (ayuda a detectar si alguna otra parte del entorno sobrescribe la variable)
	expected_fecha <- tryCatch({
		xlsx_path <- "data/repatriados.xlsx"
		if (file.exists(xlsx_path) && exists("get_fecha_corte", mode = "function")) {
			get_fecha_corte(path = xlsx_path)
		} else {
			NA
		}
	}, error = function(e) NA)

	message(sprintf("[app.R] fecha_corte (asignada) = %s (clase: %s)", as.character(app_data$fecha_corte), class(app_data$fecha_corte)))
	message(sprintf("[app.R] fecha_corte (esperada desde CSV) = %s (clase: %s)", as.character(expected_fecha), class(expected_fecha)))
	if (is.na(expected_fecha) || is.na(app_data$fecha_corte) || as.Date(expected_fecha) != as.Date(app_data$fecha_corte)) {
		stop("[app.R] Inconsistencia: la fecha de corte calculada no coincide con la esperada a partir del CSV. El proceso se detiene para evitar mostrar una fecha equivocada.")
	}
} else {
	warning("init_app_data() not found; global variables not initialized")
}

# Cargar UI y servidor
if (file.exists("ui.R")) source("ui.R") else stop("ui.R not found")
if (file.exists("server.R")) source("server.R") else stop("server.R not found")

# 'ui' and 'server' deben quedar definidos por los archivos sourceados
if (!exists("ui") || !exists("server")) stop("ui or server not defined after sourcing files")

shinyApp(ui = ui, server = server)
