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

# Cargar funciones de helper y data loaders
source("R/data_loader.R")

# Validar que `fecha_corte` exista y sea una Date válida en el entorno global antes de levantar el server.
if (!exists("fecha_corte")) {
  stop("[server.R] Fecha de corte ('fecha_corte') no encontrada en el entorno global. Asegúrate de ejecutar app.R que inicializa los datos.")
}
if (is.null(fecha_corte) || is.na(fecha_corte) || !inherits(fecha_corte, "Date")) {
  stop(sprintf("[server.R] Fecha de corte inválida: %s (clase: %s). La app requiere una fecha de corte válida en global.R/app.R.",
               paste0(capture.output(str(fecha_corte)), collapse = " "), class(fecha_corte)))
}

server <- function(input, output, session) {

  # Helper: normalizar cadenas para comparación robusta
  normalize_str <- function(x) {
    if (is.null(x)) return(NA_character_)
    s <- as.character(x)
    s <- iconv(s, to = "ASCII//TRANSLIT")
    s <- trimws(tolower(s))
    s
  }

  # Helper: deduplicar centros antes de contar/mostrar
  # Prefiere 'id_albergue' si está disponible; si no, deduplica por coordenadas aproximadas
  dedupe_centros <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    # Si hay coordenadas, deduplicar por coordenadas redondeadas (primer registro por coordenada)
    if (all(c("Latitud", "Longitud") %in% names(df))) {
      coords <- paste0(format(round(as.numeric(df$Latitud), 6), nsmall = 6), "_", format(round(as.numeric(df$Longitud), 6), nsmall = 6))
      return(df[!duplicated(coords), , drop = FALSE])
    }
    # Si no hay coordenadas pero existe id_albergue, deduplicar por id
    if ("id_albergue" %in% names(df)) {
      return(df[!duplicated(df$id_albergue), , drop = FALSE])
    }
    # Si no hay claves para deduplicar, devolver tal cual
    df
  }

  # Helper: sanitizar dataframe de centros para asegurar tipos y estructura esperada
  sanitize_centros <- function(df) {
    if (is.null(df)) return(data.frame())
    # Si viene como lista u otro, intentar convertir a data.frame
    if (!is.data.frame(df)) {
      try({ df <- as.data.frame(df, stringsAsFactors = FALSE) }, silent = TRUE)
    }
    if (!is.data.frame(df)) return(data.frame())
    # Convertir columnas tipo list a columnas atómicas (caracter) para evitar pasar listas a leaflet
    for (nm in names(df)) {
      if (is.list(df[[nm]])) {
        df[[nm]] <- sapply(df[[nm]], function(x) {
          if (is.null(x)) return(NA_character_)
          if (length(x) == 0) return(NA_character_)
          paste(unlist(x), collapse = " ")
        }, USE.NAMES = FALSE)
      }
    }
  # Asegurar columnas de Latitud/Longitud numéricas
    if ("Latitud" %in% names(df)) df$Latitud <- suppressWarnings(as.numeric(as.character(df$Latitud)))
    if ("Longitud" %in% names(df)) df$Longitud <- suppressWarnings(as.numeric(as.character(df$Longitud)))
  # Asegurar columnas de Capacidad numéricas si existen
  if ("Capacidad" %in% names(df)) df$Capacidad <- suppressWarnings(as.numeric(as.character(df$Capacidad)))
  if ("capacidad" %in% names(df)) df$capacidad <- suppressWarnings(as.numeric(as.character(df$capacidad)))
    # Quitar filas sin coordenadas si existen columnas Latitud/Longitud
    if (all(c("Latitud","Longitud") %in% names(df))) df <- df[!is.na(df$Latitud) & !is.na(df$Longitud), , drop = FALSE]
    # Forzar data.frame limpio
    as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  }

  # Flag para suprimir la ejecución automática de filtros mientras actualizamos selects programáticamente
  suppress_auto <- shiny::reactiveVal(FALSE)

  # (Debug observer removed to reduce terminal noise)

  # Leer datos de centros de atención
  centros_data <- tryCatch({
    # Preferir catálogo de albergues si existe; mapear columnas a las esperadas por los módulos
    if (file.exists("data/cat_albergues.csv") && exists("get_albergues_data", mode = "function")) {
      df <- tryCatch(get_albergues_data("data/cat_albergues.csv"), error = function(e) {
        message("get_albergues_data error: ", e$message)
        NULL
      })
      if (!is.null(df) && nrow(df) > 0) {
        # Asegurar columnas con los nombres que espera mod_centros_mapa: Latitud, Longitud, Entidad, Municipio, Capacidad, Responsable
        # Mapear descripcion -> Responsable, latitude/longitude ya normalizadas por get_albergues_data
        if ("Descripcion" %in% names(df) && !"Responsable" %in% names(df)) df$Responsable <- df$Descripcion
        # Si no existen Entidad/Municipio, crear columnas vacías para evitar errores en el módulo
        if (!"Entidad" %in% names(df)) df$Entidad <- NA_character_
        if (!"Municipio" %in% names(df)) df$Municipio <- NA_character_
        if (!"Direccion" %in% names(df)) df$Direccion <- NA_character_
        # Asegurar Capacidad/Latitud/Longitud
        if (!"Capacidad" %in% names(df)) df$Capacidad <- NA_real_
        if (!"Latitud" %in% names(df) && "latitude" %in% tolower(names(df))) df$Latitud <- df[[which(tolower(names(df))=="latitude")]]
        if (!"Longitud" %in% names(df) && "longitude" %in% tolower(names(df))) df$Longitud <- df[[which(tolower(names(df))=="longitude")]]
        # Return a data.frame compatible con mod_centros_mapa
        as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        # Fallback a centros_atencion.csv si cat_albergues no es usable
        tryCatch(read.csv("data/centros_atencion.csv", stringsAsFactors = FALSE), error = function(e) {
          message("No se pudo leer centros_atencion.csv: ", e$message)
          NULL
        })
      }
    } else {
      read.csv("data/centros_atencion.csv", stringsAsFactors = FALSE)
    }
  }, error = function(e) {
    message("No se pudo leer datos de centros: ", e$message)
    NULL
  })
  # Montar el módulo del mapa de centros si existe y los datos están disponibles
  if (exists("mod_centros_mapa_server") && !is.null(centros_data)) {
    tryCatch(
      mod_centros_mapa_server("centrosmapa1", data = centros_data),
      error = function(e) message("mod_centros_mapa_server error: ", e$message)
    )
  }
    # Evitar que Shiny suspenda el render de los mapas aunque las secciones estén ocultas
    tryCatch({
      outputOptions(output, "centrosmapa1-mapa_centros", suspendWhenHidden = FALSE)
    }, error = function(e) {})

  # Montar el módulo del mapa de deportaciones (EEUU) en la sección 'origen'
  # Preferir agregación desde el xlsx 'data/repatriados.xlsx' si está disponible;
  # en caso contrario, usar el CSV 'data/deportaciones.csv' como fallback.
  deportaciones_data <- NULL
  try({
    xlsx_path <- "data/repatriados.xlsx"
    if (file.exists(xlsx_path) && exists("agg_repatriados_from_xlsx", mode = "function")) {
      # agg_repatriados_from_xlsx devuelve data.frame con columnas 'Estados' y 'Repatriados'
      dat_x <- tryCatch(agg_repatriados_from_xlsx(path = xlsx_path, sheet = "Repatriados"), error = function(e) {
        message("agg_repatriados_from_xlsx error: ", e$message)
        NULL
      })
      if (!is.null(dat_x) && is.data.frame(dat_x) && nrow(dat_x) > 0) {
        deportaciones_data <- dat_x
        message(sprintf("[server] usando datos agregados desde '%s' (%d estados) para el mapa de deportaciones", xlsx_path, nrow(dat_x)))
      }
    }
  }, silent = TRUE)

  # Fallback CSV si no obtuvimos datos desde xlsx
  # Si no se obtuvieron datos desde xlsx, crear un data.frame vacío compatible
  if (is.null(deportaciones_data)) {
    deportaciones_data <- data.frame(Estados = character(0), Repatriados = integer(0), stringsAsFactors = FALSE)
  }

  if (exists("mod_deportaciones_mapa_server") && !is.null(deportaciones_data)) {
    tryCatch(
      mod_deportaciones_mapa_server("deportacionesmapa1", data = deportaciones_data),
      error = function(e) message("mod_deportaciones_mapa_server error: ", e$message)
    )
    tryCatch({
      outputOptions(output, "deportacionesmapa1-mapa_deportaciones", suspendWhenHidden = FALSE)
    }, error = function(e) {})
  }

  # Montar mapa de repatriaciones (México) usando el mismo módulo si deseado
  # Montar mapa de repatriaciones (México)
  repatriaciones_data <- NULL
  try({
    xlsx_path <- "data/repatriados.xlsx"
    # Intentar agregar desde el xlsx usando la columna ESTADO DE DESTINO
    if (file.exists(xlsx_path) && exists("agg_repatriados_from_xlsx", mode = "function")) {
      dat_x_mx <- tryCatch(agg_repatriados_from_xlsx(path = xlsx_path, sheet = "Repatriados", state_col = "ESTADO DE DESTINO"), error = function(e) {
        message("agg_repatriados_from_xlsx (MX) error: ", e$message)
        NULL
      })
      if (!is.null(dat_x_mx) && is.data.frame(dat_x_mx) && nrow(dat_x_mx) > 0) {
        # Renombrar columnas para compatibilidad con el módulo (Repatriaciones / Estados)
        if ("Repatriados" %in% names(dat_x_mx)) names(dat_x_mx)[names(dat_x_mx) == "Repatriados"] <- "Repatriaciones"
        repatriaciones_data <- dat_x_mx
        message(sprintf("[server] usando datos agregados desde '%s' (%d estados) para el mapa de repatriaciones MX", xlsx_path, nrow(dat_x_mx)))
        # Imprimir en consola los estados de México sin registros (Repatriaciones == 0)
        cnt_col <- if ("Repatriaciones" %in% names(dat_x_mx)) "Repatriaciones" else if ("Repatriados" %in% names(dat_x_mx)) "Repatriados" else NULL
        if (!is.null(cnt_col)) {
          zero_idx <- which(as.integer(dat_x_mx[[cnt_col]]) == 0)
          if (length(zero_idx) > 0) {
            missing_mx <- dat_x_mx$Estados[zero_idx]
            message(sprintf("[server] Estados MX sin registros en '%s' (se usarán 0): %s", xlsx_path, paste(missing_mx, collapse = ", ")))
          }
        }
      }
    }
  }, silent = TRUE)

  # Fallback CSV si no obtuvimos datos desde xlsx
  # Si no se obtuvieron datos desde xlsx, crear un data.frame vacío compatible
  if (is.null(repatriaciones_data)) {
    repatriaciones_data <- data.frame(Estados = character(0), Repatriaciones = integer(0), Repatriados = integer(0), stringsAsFactors = FALSE)
  }

  if (exists("mod_repatriaciones_mx_server") && !is.null(repatriaciones_data)) {
    tryCatch(
      mod_repatriaciones_mx_server("repatriacionesmapa1", data = repatriaciones_data),
      error = function(e) message("mod_repatriaciones_mx_server error: ", e$message)
    )
    tryCatch({
      outputOptions(output, "repatriacionesmapa1-mapa_repatriaciones", suspendWhenHidden = FALSE)
    }, error = function(e) {})
  }

  # --- Montar servidores de los filtros del mapa ---
  # Dropdown debajo del mapa: listar centros por 'Descripcion' (centro de atención)
  entidades_choices <- if (!is.null(centros_data) && "Descripcion" %in% names(centros_data)) unique(centros_data$Descripcion) else NULL
  if (exists("mod_filters_dropdown_server")) {
    tryCatch({
      sel_ent <- mod_filters_dropdown_server("map_dd_entidad")
      # Initialize select choices via update if available (non-blocking)
      if (!is.null(entidades_choices) && length(entidades_choices) > 0) {
        tryCatch({
          current_sel <- NULL
          try({ current_sel <- input[["map_dd_entidad-select"]] }, silent = TRUE)
          choices_map <- c("Seleccionar...", entidades_choices)
          sel_to_set <- if (!is.null(current_sel) && current_sel %in% choices_map) current_sel else "Seleccionar..."
          updateSelectInput(session, "map_dd_entidad-select", choices = choices_map, selected = sel_to_set)
        }, error = function(e) {})
      }
    }, error = function(e) message("mod_filters_dropdown_server error: ", e$message))
  }

  # Montar fila de filtros principal debajo del título 'Filtro de Centros de Atención'
  if (exists("mod_filters_row_server")) {
    tryCatch({
      filtros <- mod_filters_row_server("filtros1")
      session$userData$filtros1 <- filtros
    }, error = function(e) message("mod_filters_row_server error: ", e$message))

    # Inicializar selects una vez cuando `centros_data` esté disponible
    shiny::observeEvent(centros_data, {
      if (is.null(centros_data)) return()
      # Estado (d1)
      if ("Entidad" %in% names(centros_data)) {
        estado_vals <- unique(na.omit(centros_data$Entidad))
        if (!is.null(input[["filtros1-d1-select"]])) {
          tryCatch({
            current_sel_d1 <- NULL
            try({ current_sel_d1 <- input[["filtros1-d1-select"]] }, silent = TRUE)
            choices_d1 <- c("Seleccionar...", estado_vals)
            sel_d1 <- if (!is.null(current_sel_d1) && current_sel_d1 %in% choices_d1) current_sel_d1 else "Seleccionar..."
            updateSelectInput(session, "filtros1-d1-select", choices = choices_d1, selected = sel_d1)
          }, error = function(e) {})
        }
      }
      # Municipio inicial (d2)
      if ("Municipio" %in% names(centros_data)) {
        muni_vals <- sort(unique(na.omit(centros_data$Municipio)))
        if (!is.null(input[["filtros1-d2-select"]])) {
          tryCatch({
            current_sel_d2 <- NULL
            try({ current_sel_d2 <- input[["filtros1-d2-select"]] }, silent = TRUE)
            choices_d2 <- c("Seleccionar...", as.character(muni_vals))
            sel_d2 <- if (!is.null(current_sel_d2) && current_sel_d2 %in% choices_d2) current_sel_d2 else "Seleccionar..."
            updateSelectInput(session, "filtros1-d2-select", choices = choices_d2, selected = sel_d2)
          }, error = function(e) {})
        }
      } else if ("id_municipio" %in% names(centros_data)) {
        muni_vals <- sort(unique(centros_data$id_municipio))
        if (!is.null(input[["filtros1-d2-select"]])) {
          tryCatch({
            current_sel_d2 <- NULL
            try({ current_sel_d2 <- input[["filtros1-d2-select"]] }, silent = TRUE)
            choices_d2 <- c("Seleccionar...", as.character(muni_vals))
            sel_d2 <- if (!is.null(current_sel_d2) && current_sel_d2 %in% choices_d2) current_sel_d2 else "Seleccionar..."
            updateSelectInput(session, "filtros1-d2-select", choices = choices_d2, selected = sel_d2)
          }, error = function(e) {})
        }
      }
    }, ignoreNULL = TRUE)

    # Cuando el usuario cambia el Estado (d1), poblar solo los municipios de ese estado
    shiny::observeEvent(input[["filtros1-d1-select"]], {
      selected_estado <- NULL
      try({ selected_estado <- input[["filtros1-d1-select"]] })
      if (is.null(selected_estado) || selected_estado == "Seleccionar...") return()
      if (!is.null(centros_data) && "Entidad" %in% names(centros_data) && "Municipio" %in% names(centros_data)) {
        muni_for_estado <- sort(unique(na.omit(centros_data$Municipio[normalize_str(centros_data$Entidad) == normalize_str(selected_estado)])))
        choices_d2 <- c("Seleccionar...", as.character(muni_for_estado))
        tryCatch({
          suppress_auto(TRUE)
          updateSelectInput(session, "filtros1-d2-select", choices = choices_d2, selected = "Seleccionar...")
        }, error = function(e) {})
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() suppress_auto(FALSE), 0.2)
        } else {
          # Fallback: limpiar suppression inmediatamente si later no está disponible
          suppress_auto(FALSE)
        }
      }
    }, ignoreInit = TRUE)
  }

  # No hay campo de texto 'Buscar' en la UI del mapa; no se crea reactive.

  # Buttons
  if (exists("mod_filters_buttons_server")) {
    tryCatch({
      btns <- mod_filters_buttons_server("map_btns")
      # Expose reactives in session$userData for future observers
      session$userData$map_btns <- btns
      # Exponer la selección del dropdown si fue inicializada
      if (exists("sel_ent")) session$userData$map_sel_ent <- sel_ent
    }, error = function(e) message("mod_filters_buttons_server error: ", e$message))
  }

  # Observers for map filter buttons: Buscar and Refrescar
  # Use leafletProxy to update markers in the map module output (namespaced id: centrosmapa1-mapa_centros)
  tryCatch({
    if (!is.null(session$userData$map_btns)) {
      # Buscar: filter by selected centro (Descripcion) and update markers
      shiny::observeEvent(session$userData$map_btns$buscar(), {
        sel <- NULL
        try({ sel <- if (!is.null(session$userData$map_sel_ent)) session$userData$map_sel_ent() })
        if (is.null(centros_data)) return()
        if (is.null(sel) || sel == "" || sel == "Seleccionar...") {
          filtered <- centros_data
        } else {
          # Comparacion robusta usando normalizacion sobre Descripcion
          norm_sel <- normalize_str(sel)
          filtered <- centros_data[normalize_str(centros_data$Descripcion) == norm_sel, , drop = FALSE]
        }
        icon_personas <- leaflet::makeIcon(
          iconUrl = "images/iconos_centros.png",
          iconWidth = 20, iconHeight = 20,
          iconAnchorX = 10, iconAnchorY = 20
        )
        tryCatch({
          filtered_sanit <- sanitize_centros(filtered)
          filtered_unique <- dedupe_centros(filtered_sanit)
          # Convertir cualquier columna tipo list a character para evitar pasar listas a leaflet
          if (is.data.frame(filtered_unique) && nrow(filtered_unique) > 0) {
            for (nm in names(filtered_unique)) {
              if (is.list(filtered_unique[[nm]])) {
                filtered_unique[[nm]] <- sapply(filtered_unique[[nm]], function(x) {
                  if (is.null(x)) return(NA_character_)
                  if (length(x) == 0) return(NA_character_)
                  paste(unlist(x), collapse = " ")
                }, USE.NAMES = FALSE)
              }
            }
            # Forzar columnas clave a tipo character y coords a numeric
            for (col in c("Responsable", "Entidad", "Municipio", "Direccion")) {
              if (!col %in% names(filtered_unique)) filtered_unique[[col]] <- NA_character_
              filtered_unique[[col]] <- as.character(filtered_unique[[col]])
            }
            if ("Latitud" %in% names(filtered_unique)) filtered_unique$Latitud <- suppressWarnings(as.numeric(as.character(filtered_unique$Latitud)))
            if ("Longitud" %in% names(filtered_unique)) filtered_unique$Longitud <- suppressWarnings(as.numeric(as.character(filtered_unique$Longitud)))
          }
          # Si no hay filas válidas, limpiar marcadores y salir
          if (!is.data.frame(filtered_unique) || nrow(filtered_unique) == 0) {
            leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>% leaflet::clearMarkers()
          } else {
            leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>%
              leaflet::clearMarkers() %>%
              leaflet::addMarkers(
                data = as.data.frame(filtered_unique, stringsAsFactors = FALSE, check.names = FALSE),
                lng = ~Longitud,
                lat = ~Latitud,
                label = ~Responsable,
                popup = ~paste0("<b>", Entidad, ", ", Municipio, "</b><br>Dirección: ", Direccion, "<br>Capacidad: ", Capacidad, "<br>Responsable: ", Responsable),
                icon = icon_personas
              )
          }
          # Ajustar vista al bounds de los puntos filtrados para hacerlos visibles
          if (is.data.frame(filtered_unique) && nrow(filtered_unique) > 0 && all(c("Latitud","Longitud") %in% names(filtered_unique))) {
            lat_min <- min(filtered_unique$Latitud, na.rm = TRUE); lat_max <- max(filtered_unique$Latitud, na.rm = TRUE)
            lng_min <- min(filtered_unique$Longitud, na.rm = TRUE); lng_max <- max(filtered_unique$Longitud, na.rm = TRUE)
            try({ leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>% leaflet::fitBounds(lng_min, lat_min, lng_max, lat_max) }, silent = TRUE)
            try({ shiny::showNotification(sprintf("Se muestran %d centros", nrow(filtered_unique)), type = "message", duration = 3) }, silent = TRUE)
          }
        }, error = function(e) message("Error updating leaflet via proxy: ", e$message))
      }, ignoreInit = TRUE)

      # Refrescar: reset selection and show all markers
      shiny::observeEvent(session$userData$map_btns$refrescar(), {
  tryCatch({ updateSelectInput(session, "map_dd_entidad-select", selected = "Seleccionar...") }, error = function(e) {})
        if (is.null(centros_data)) return()
        icon_personas <- leaflet::makeIcon(
          iconUrl = "images/iconos_centros.png",
          iconWidth = 20, iconHeight = 20,
          iconAnchorX = 10, iconAnchorY = 20
        )
        tryCatch({
          centros_sanit <- sanitize_centros(centros_data)
          centros_unique <- dedupe_centros(centros_sanit)
          leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>%
            leaflet::clearMarkers() %>%
            leaflet::addMarkers(
              data = centros_unique,
              lng = ~Longitud,
              lat = ~Latitud,
              label = ~Responsable,
              popup = ~paste0("<b>", Entidad, ", ", Municipio, "</b><br>Dirección: ", Direccion, "<br>Capacidad: ", Capacidad, "<br>Responsable: ", Responsable),
              icon = icon_personas
            )
        }, error = function(e) message("Error refreshing leaflet via proxy: ", e$message))
      }, ignoreInit = TRUE)
    }
  }, error = function(e) message("Observers for map buttons not installed: ", e$message))

  # Observers para la fila de filtros (`filtros1`): buscar / refrescar
  tryCatch({
    if (!is.null(session$userData$filtros1)) {
      # Buscar: aplicar filtros compuestos (Estado via Latitud/Longitud string, Municipio via id_municipio, Capacidad via numeric)
      shiny::observeEvent(session$userData$filtros1$buscar(), {
        if (is.null(centros_data)) return()
        sel_estado <- tryCatch({ session$userData$filtros1$sel1() }, error = function(e) NULL)
        sel_municipio <- tryCatch({ session$userData$filtros1$sel2() }, error = function(e) NULL)
        sel_capacidad <- tryCatch({ session$userData$filtros1$num() }, error = function(e) NULL)

        filtered <- centros_data

        # Estado: filtrar por columna 'Entidad' si está disponible
        if (!is.null(sel_estado) && sel_estado != "" && sel_estado != "Seleccionar...") {
          if ("Entidad" %in% names(filtered)) {
            norm_sel_e <- normalize_str(sel_estado)
            filtered <- filtered[normalize_str(filtered$Entidad) == norm_sel_e, , drop = FALSE]
          }
        }

        # Municipio: filtrar por columna 'Municipio' si está disponible; si no, intentar por id_municipio
        if (!is.null(sel_municipio) && sel_municipio != "" && sel_municipio != "Seleccionar...") {
          if ("Municipio" %in% names(filtered)) {
            norm_sel_m <- normalize_str(sel_municipio)
            filtered <- filtered[normalize_str(filtered$Municipio) == norm_sel_m, , drop = FALSE]
          } else if ("id_municipio" %in% names(filtered)) {
            filtered <- filtered[as.character(filtered$id_municipio) == as.character(sel_municipio), , drop = FALSE]
          }
        }

        # Capacidad: asegurarse que la columna sea numérica y filtrar por capacidad > valor si proporcionado
        # Coerce numeric on candidate columns to avoid string/list issues from upstream
        if ("Capacidad" %in% names(filtered)) {
          filtered$Capacidad <- suppressWarnings(as.numeric(as.character(filtered$Capacidad)))
        }
        if ("capacidad" %in% names(filtered)) {
          filtered$capacidad <- suppressWarnings(as.numeric(as.character(filtered$capacidad)))
        }
        if (!is.null(sel_capacidad) && !is.na(suppressWarnings(as.numeric(sel_capacidad)))) {
          cap_num <- as.numeric(sel_capacidad)
          if (!is.na(cap_num)) {
            if ("Capacidad" %in% names(filtered)) {
              filtered <- filtered[!is.na(filtered$Capacidad) & filtered$Capacidad > cap_num, , drop = FALSE]
            } else if ("capacidad" %in% names(filtered)) {
              filtered <- filtered[!is.na(filtered$capacidad) & filtered$capacidad > cap_num, , drop = FALSE]
            }
          }
        }

        # Debug: imprimir en consola resumen para diagnosticar problemas con tipos/filtrado
        tryCatch({
          msg_cols <- paste(names(filtered)[names(filtered) %in% c("Capacidad","capacidad")], collapse = ",")
          cap_preview <- tryCatch({ if ("Capacidad" %in% names(filtered)) paste(head(filtered$Capacidad, 5), collapse = ",") else if ("capacidad" %in% names(filtered)) paste(head(filtered$capacidad,5), collapse = ",") else "(none)" }, error = function(e) "(preview error)")
          message(sprintf("[debug filtros1 buscar] sel_capacidad=%s cap_num=%s cols=%s n_after=%d preview=%s", deparse(sel_capacidad), ifelse(exists('cap_num'), as.character(cap_num), 'NA'), msg_cols, nrow(filtered), cap_preview))
        }, error = function(e) {})

  # (debug message removed)
        icon_personas <- leaflet::makeIcon(
          iconUrl = "images/iconos_centros.png",
          iconWidth = 20, iconHeight = 20,
          iconAnchorX = 10, iconAnchorY = 20
        )
        tryCatch({
          filtered_sanit <- sanitize_centros(filtered)
          filtered_unique <- dedupe_centros(filtered_sanit)
          # Última pasada de seguridad: convertir cualquier columna tipo list a character
          for (nm in names(filtered_unique)) {
            if (is.list(filtered_unique[[nm]])) {
              filtered_unique[[nm]] <- sapply(filtered_unique[[nm]], function(x) {
                if (is.null(x)) return(NA_character_)
                if (length(x) == 0) return(NA_character_)
                paste(unlist(x), collapse = " ")
              }, USE.NAMES = FALSE)
            }
          }
          # Forzar coordenadas numéricas
          if ("Latitud" %in% names(filtered_unique)) filtered_unique$Latitud <- suppressWarnings(as.numeric(as.character(filtered_unique$Latitud)))
          if ("Longitud" %in% names(filtered_unique)) filtered_unique$Longitud <- suppressWarnings(as.numeric(as.character(filtered_unique$Longitud)))

          # Debug: estructura y tipos antes de pasar a leaflet
          tryCatch({
            types <- sapply(filtered_unique, function(x) class(x)[1])
            message(sprintf("[debug filtros1 buscar] filtered_unique rows=%d types=%s", nrow(filtered_unique), paste(names(types), types, sep = ":", collapse = ", ")))
            # Comprobar si hay elementos de tipo list dentro de columnas
            problematic <- vapply(names(filtered_unique), function(nm) {
              any(sapply(filtered_unique[[nm]], is.list))
            }, logical(1))
            if (any(problematic)) {
              probs <- names(problematic)[problematic]
              message(sprintf("[debug filtros1 buscar] columnas con elementos list: %s", paste(probs, collapse = ",")))
            }
            # Imprimir str() compacto para diagnóstico
            st <- paste(utils::capture.output(str(head(filtered_unique, 20))), collapse = " \n ")
            message(sprintf("[debug filtros1 buscar] str(head(filtered_unique,20)): %s", st))
          }, error = function(e) {})

          # Si no hay filas, limpiar marcadores y evitar llamar a addMarkers (evita errores internos de leaflet)
          if (!is.data.frame(filtered_unique) || nrow(filtered_unique) == 0) {
            try({
              leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>% leaflet::clearMarkers()
            }, silent = TRUE)
          } else {
            # Forzar columnas de texto usadas en label/popup a character para evitar listas anidadas
            for (txt in c("Entidad","Municipio","Direccion","Responsable","Descripcion")) {
              if (txt %in% names(filtered_unique)) filtered_unique[[txt]] <- as.character(filtered_unique[[txt]])
            }
            leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>%
              leaflet::clearMarkers() %>%
              leaflet::addMarkers(
                data = filtered_unique,
                lng = ~Longitud,
                lat = ~Latitud,
                label = ~Responsable,
                popup = ~paste0("<b>", Entidad, ", ", Municipio, "</b><br>Dirección: ", Direccion, "<br>Capacidad: ", Capacidad, "<br>Responsable: ", Responsable),
                icon = icon_personas
              )
          }
          if (is.data.frame(filtered_unique) && nrow(filtered_unique) > 0 && all(c("Latitud","Longitud") %in% names(filtered_unique))) {
            lat_min <- min(filtered_unique$Latitud, na.rm = TRUE); lat_max <- max(filtered_unique$Latitud, na.rm = TRUE)
            lng_min <- min(filtered_unique$Longitud, na.rm = TRUE); lng_max <- max(filtered_unique$Longitud, na.rm = TRUE)
            try({ leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>% leaflet::fitBounds(lng_min, lat_min, lng_max, lat_max) }, silent = TRUE)
            try({ shiny::showNotification(sprintf("Se muestran %d centros", nrow(filtered_unique)), type = "message", duration = 3) }, silent = TRUE)
          }
        }, error = function(e) message("Error updating leaflet via proxy (filtros1 buscar): ", e$message))
      }, ignoreInit = TRUE)

      # Aplicar filtros automáticamente al cambiar selecciones (para depuración y UX)
      shiny::observe({
        sel_estado_auto <- tryCatch({ session$userData$filtros1$sel1() }, error = function(e) NULL)
        sel_muni_auto <- tryCatch({ session$userData$filtros1$sel2() }, error = function(e) NULL)
        sel_cap_auto <- tryCatch({ session$userData$filtros1$num() }, error = function(e) NULL)
        # Solo proceder si al menos una selección no es nula
        if (is.null(centros_data)) return()
        # No ejecutar auto mientras estamos actualizando selects programáticamente
        if (isTRUE(suppress_auto())) return()
        if ((is.null(sel_estado_auto) || sel_estado_auto == "Seleccionar...") && (is.null(sel_muni_auto) || sel_muni_auto == "Seleccionar...") && (is.null(sel_cap_auto) || is.na(as.numeric(sel_cap_auto)))) return()
        filtered_auto <- centros_data
        if (!is.null(sel_estado_auto) && sel_estado_auto != "" && sel_estado_auto != "Seleccionar...") {
          if ("Entidad" %in% names(filtered_auto)) {
            norm_sel_ea <- normalize_str(sel_estado_auto)
            filtered_auto <- filtered_auto[normalize_str(filtered_auto$Entidad) == norm_sel_ea, , drop = FALSE]
          }
        }
        if (!is.null(sel_muni_auto) && sel_muni_auto != "" && sel_muni_auto != "Seleccionar...") {
          if ("Municipio" %in% names(filtered_auto)) {
            norm_sel_ma <- normalize_str(sel_muni_auto)
            filtered_auto <- filtered_auto[normalize_str(filtered_auto$Municipio) == norm_sel_ma, , drop = FALSE]
          } else if ("id_municipio" %in% names(filtered_auto)) {
            filtered_auto <- filtered_auto[as.character(filtered_auto$id_municipio) == as.character(sel_muni_auto), , drop = FALSE]
          }
        }
        if (!is.null(sel_cap_auto) && !is.na(as.numeric(sel_cap_auto))) {
          cap_num_auto <- as.numeric(sel_cap_auto)
          if (!is.na(cap_num_auto)) {
            # Forzar numéricos antes de comparar
            if ("Capacidad" %in% names(filtered_auto)) filtered_auto$Capacidad <- suppressWarnings(as.numeric(as.character(filtered_auto$Capacidad)))
            if ("capacidad" %in% names(filtered_auto)) filtered_auto$capacidad <- suppressWarnings(as.numeric(as.character(filtered_auto$capacidad)))
            if ("Capacidad" %in% names(filtered_auto)) {
              filtered_auto <- filtered_auto[!is.na(filtered_auto$Capacidad) & filtered_auto$Capacidad > cap_num_auto, , drop = FALSE]
            } else if ("capacidad" %in% names(filtered_auto)) {
              filtered_auto <- filtered_auto[!is.na(filtered_auto$capacidad) & filtered_auto$capacidad > cap_num_auto, , drop = FALSE]
            }
          }
        }
  # (debug message removed)
        icon_personas_auto <- leaflet::makeIcon(iconUrl = "images/iconos_centros.png", iconWidth = 20, iconHeight = 20, iconAnchorX = 10, iconAnchorY = 20)
        tryCatch({
          filtered_auto_sanit <- sanitize_centros(filtered_auto)
          filtered_auto_unique <- dedupe_centros(filtered_auto_sanit)
          # Seguridad: convertir columnas list a character y asegurar coords
          for (nm in names(filtered_auto_unique)) {
            if (is.list(filtered_auto_unique[[nm]])) {
              filtered_auto_unique[[nm]] <- sapply(filtered_auto_unique[[nm]], function(x) {
                if (is.null(x)) return(NA_character_)
                if (length(x) == 0) return(NA_character_)
                paste(unlist(x), collapse = " ")
              }, USE.NAMES = FALSE)
            }
          }
          if ("Latitud" %in% names(filtered_auto_unique)) filtered_auto_unique$Latitud <- suppressWarnings(as.numeric(as.character(filtered_auto_unique$Latitud)))
          if ("Longitud" %in% names(filtered_auto_unique)) filtered_auto_unique$Longitud <- suppressWarnings(as.numeric(as.character(filtered_auto_unique$Longitud)))
          tryCatch({
            typesa <- sapply(filtered_auto_unique, function(x) class(x)[1])
            message(sprintf("[debug filtros1 auto] rows=%d types=%s", nrow(filtered_auto_unique), paste(names(typesa), typesa, sep=":", collapse=", ")))
          }, error = function(e) {})
          if (!is.data.frame(filtered_auto_unique) || nrow(filtered_auto_unique) == 0) {
            try({ leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>% leaflet::clearMarkers() }, silent = TRUE)
          } else {
            for (txt in c("Entidad","Municipio","Direccion","Responsable","Descripcion")) {
              if (txt %in% names(filtered_auto_unique)) filtered_auto_unique[[txt]] <- as.character(filtered_auto_unique[[txt]])
            }
            leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>%
              leaflet::clearMarkers() %>%
              leaflet::addMarkers(
                data = filtered_auto_unique,
                lng = ~Longitud,
                lat = ~Latitud,
                label = ~Responsable,
                popup = ~paste0("<b>", Entidad, ", ", Municipio, "</b><br>Dirección: ", Direccion, "<br>Capacidad: ", Capacidad, "<br>Responsable: ", Responsable),
                icon = icon_personas_auto
              )
          }
        }, error = function(e) message("Error updating leaflet via proxy (filtros1 auto): ", e$message))
      })

      # Refrescar: limpiar selects y mostrar todos los markers
      shiny::observeEvent(session$userData$filtros1$refrescar(), {
        tryCatch({ updateSelectInput(session, "filtros1-d1-select", selected = "Seleccionar...") }, error = function(e) {})
        tryCatch({ updateSelectInput(session, "filtros1-d2-select", selected = "Seleccionar...") }, error = function(e) {})
        tryCatch({ updateNumericInput(session, "filtros1-n1-num", value = NULL) }, error = function(e) {})
        if (is.null(centros_data)) return()
        icon_personas <- leaflet::makeIcon(
          iconUrl = "images/iconos_centros.png",
          iconWidth = 20, iconHeight = 20,
          iconAnchorX = 10, iconAnchorY = 20
        )
        tryCatch({
          centros_sanit2 <- sanitize_centros(centros_data)
          centros_unique <- dedupe_centros(centros_sanit2)
          leaflet::leafletProxy("centrosmapa1-mapa_centros", session) %>%
            leaflet::clearMarkers() %>%
            leaflet::addMarkers(
              data = centros_unique,
              lng = ~Longitud,
              lat = ~Latitud,
              label = ~Responsable,
              popup = ~paste0("<b>", Entidad, ", ", Municipio, "</b><br>Dirección: ", Direccion, "<br>Capacidad: ", Capacidad, "<br>Responsable: ", Responsable),
              icon = icon_personas
            )
        }, error = function(e) message("Error refreshing leaflet via proxy: ", e$message))
      }, ignoreInit = TRUE)
    }
  }, error = function(e) message("Observers for filtros1 not installed: ", e$message))

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
  # Montar segunda barra de progreso debajo de los filtros del mapa
  # Prepare a reactiveVal to allow updating the second bar from filtered map results
  ocupacion_bar2_values <- shiny::reactiveVal(NULL)
  if (exists("mod_progress_bar_server")) {
    tryCatch(
      mod_progress_bar_server("ocupacion_bar2", external_values = ocupacion_bar2_values),
      error = function(e) message("mod_progress_bar_server (bar2) error: ", e$message)
    )
  }
  # Montar segundo arreglo de tarjetas KPI
  if (exists("mod_kpi_cards_grid_server")) {
    tryCatch(
      mod_kpi_cards_grid_server("kpi_grid2"),
      error = function(e) message("mod_kpi_cards_grid_server (kpi_grid2) error: ", e$message)
    )
  }
  # Si tienes un módulo de fecha reutilizable, puedes montarlo también (ejemplo):
  if (exists("mod_fecha_server")) {
    # No usar el mismo id que el placeholder principal; usarlo en submódulos cuando haga falta
    # try(mod_fecha_server("fecha1", fecha_reactivo = fecha_reactivo), silent = TRUE)
  }

  # Aquí puedes añadir otras inicializaciones de servidor / observers globales
}