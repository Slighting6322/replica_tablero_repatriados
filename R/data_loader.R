# R/data_loader.R
## R/data_loader.R
## Utilities to load and preprocess datasets used by the app.

# Public functions provided:
# get_repatriados_data(path = <path_to_csv>, force = FALSE)
# - get_fecha_corte(path = NULL, data = NULL)
# - agg_repatriados_from_xlsx(path = "data/repatriados.xlsx", sheet = "Repatriados", state_col = NULL)
# - normalize_state_names(x)
# - clear_repatriados_cache()  (no-op unless memoise available)

loader_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# Forward declarations (help static code checkers); real implementations follow below
get_repatriados_data <- function(path = NULL, force = FALSE) {
  # Forward declaration: returns empty tibble if no path provided
  tibble::tibble()
}

# get_repatriados_data: uses memoise::memoise if available to cache in-memory
if (requireNamespace("memoise", quietly = TRUE)) {
  .memo_loader <- memoise::memoise(loader_read_csv)
  get_repatriados_data <- function(path = NULL, force = FALSE) {
    if (is.null(path) || !nzchar(path)) return(tibble::tibble())
    if (isTRUE(force)) {
      tryCatch(memoise::forget(.memo_loader), error = function(e) NULL)
    }
    tryCatch(.memo_loader(path), error = function(e) {
      message("[data_loader] error loading ", path, ": ", e$message)
      tibble::tibble()
    })
  }
  # memoise-based loader present; no explicit cache-clear helper retained
} else {
  get_repatriados_data <- function(path = NULL, force = FALSE) {
    if (is.null(path) || !nzchar(path)) return(tibble::tibble())
    tryCatch(loader_read_csv(path), error = function(e) {
      message("[data_loader] error loading ", path, ": ", e$message)
      tibble::tibble()
    })
  }
  # No explicit cache-clear helper when memoise is not present
}


get_fecha_corte <- function(path = NULL, data = NULL) {
  # Either provide a data.frame/tibble in 'data' or a path to read.
  df <- NULL
  if (!is.null(data)) {
    df <- data
  } else if (!is.null(path)) {
    df <- get_repatriados_data(path)
  } else {
    stop("either 'path' or 'data' must be provided")
  }

  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(as.Date(NA))

  col_name <- if ("fecha_repatriacion" %in% names(df)) "fecha_repatriacion" else names(df)[1]
  vec <- df[[col_name]]
  if (inherits(vec, c("Date", "POSIXt"))) return(as.Date(max(vec, na.rm = TRUE)))

  try_parse <- function(x) {
    p <- suppressWarnings(as.Date(x))
    if (all(is.na(p))) p <- suppressWarnings(lubridate::ymd(x))
    if (all(is.na(p))) p <- suppressWarnings(lubridate::dmy(x))
    if (all(is.na(p))) p <- suppressWarnings(lubridate::mdy(x))
    if (all(is.na(p)) && requireNamespace("anytime", quietly = TRUE)) p <- suppressWarnings(anytime::anydate(x))
    as.Date(p)
  }

  parsed <- try_parse(vec)
  if (all(is.na(parsed))) return(as.Date(NA))
  as.Date(max(parsed, na.rm = TRUE))
}


agg_repatriados_from_xlsx <- function(path = "data/repatriados.xlsx", sheet = "Repatriados", state_col = "ESTADO DE DETENCION") {
  if (!file.exists(path)) stop("xlsx file not found: ", path)
  if (!requireNamespace("readxl", quietly = TRUE)) stop("please install readxl to read xlsx files")

  # Leer todo como texto para evitar warnings por parsing de tipos (fechas, etc.)
  df <- tryCatch(readxl::read_excel(path, sheet = sheet, col_types = "text"), error = function(e) stop("error reading xlsx: ", e$message))
  if (nrow(df) == 0) return(data.frame(Estados = character(0), Repatriados = integer(0), stringsAsFactors = FALSE))

  # Eliminar filas exactamente duplicadas para evitar contar la misma repatriación más de una vez
  df_unique <- df[!duplicated(df), , drop = FALSE]

  col_candidates <- names(df_unique)
  chosen <- NULL
  if (!is.null(state_col) && state_col %in% col_candidates) {
    chosen <- state_col
  } else {
    hits <- grep("estado", tolower(col_candidates), value = TRUE)
    if (length(hits) >= 1) chosen <- hits[1]
  }
  if (is.null(chosen)) stop("state column not found in xlsx (looked for '", state_col, "' or 'estado' in column names)")

  states_vec <- as.character(df_unique[[chosen]])
  states_vec <- trimws(states_vec)
  states_vec[states_vec == "" | is.na(states_vec)] <- "(sin_estado)"

  tb <- as.data.frame(table(states_vec), stringsAsFactors = FALSE)
  names(tb) <- c("Estados", "Repatriados")
  tb$Repatriados <- as.integer(tb$Repatriados)

  # Lista completa de estados de EEUU
  us_states <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
                 "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                 "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                 "Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
                 "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
                 "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
                 "Virginia","Washington","West Virginia","Wisconsin","Wyoming")




    # Lista de estados de México (para mapas MX)
    mx_states <- c("Aguascalientes","Baja California","Baja California Sur","Campeche","Chiapas","Chihuahua","Coahuila","Colima","Durango","Estado de México","Guanajuato","Guerrero","Hidalgo","Jalisco","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas","Ciudad de México")
  
  # Mapa inglés -> español para nombres de estados de EEUU (usado para mostrar en tooltips)
  eng_to_esp <- c(
    "Alabama" = "Alabama",
    "Alaska" = "Alaska",
    "Arizona" = "Arizona",
    "Arkansas" = "Arkansas",
    "California" = "California",
    "Colorado" = "Colorado",
    "Connecticut" = "Connecticut",
    "Delaware" = "Delaware",
    "Florida" = "Florida",
    "Georgia" = "Georgia",
    "Hawaii" = "Hawái",
    "Idaho" = "Idaho",
    "Illinois" = "Illinois",
    "Indiana" = "Indiana",
    "Iowa" = "Iowa",
    "Kansas" = "Kansas",
    "Kentucky" = "Kentucky",
    "Louisiana" = "Luisiana",
    "Maine" = "Maine",
    "Maryland" = "Maryland",
    "Massachusetts" = "Massachusetts",
    "Michigan" = "Michigan",
    "Minnesota" = "Minnesota",
    "Mississippi" = "Mississippi",
    "Missouri" = "Misuri",
    "Montana" = "Montana",
    "Nebraska" = "Nebraska",
    "Nevada" = "Nevada",
    "New Hampshire" = "Nuevo Hampshire",
    "New Jersey" = "Nueva Jersey",
    "New Mexico" = "Nuevo México",
    "New York" = "Nueva York",
    "North Carolina" = "Carolina del Norte",
    "North Dakota" = "Dakota del Norte",
    "Ohio" = "Ohio",
    "Oklahoma" = "Oklahoma",
    "Oregon" = "Oregón",
    "Pennsylvania" = "Pensilvania",
    "Rhode Island" = "Rhode Island",
    "South Carolina" = "Carolina del Sur",
    "South Dakota" = "Dakota del Sur",
    "Tennessee" = "Tennessee",
    "Texas" = "Texas",
    "Utah" = "Utah",
    "Vermont" = "Vermont",
    "Virginia" = "Virginia",
    "Washington" = "Washington",
    "West Virginia" = "Virginia Occidental",
    "Wisconsin" = "Wisconsin",
    "Wyoming" = "Wyoming"
  )

    # Normalizar nombres para comparación robusta
    present_norm <- normalize_state_names(tb$Estados)
    us_norm <- normalize_state_names(us_states)
  
    # Determinar si los datos parecen corresponder a EEUU o a México
    matches_with_us <- sum(present_norm %in% us_norm)
    prop_us <- if (length(present_norm) == 0) 0 else matches_with_us / length(present_norm)
  
    if (prop_us >= 0.4) {
      # Tratar como datos de EEUU
      final_tb <- data.frame(Estados = us_states, Repatriados = integer(length(us_states)), stringsAsFactors = FALSE)
      for (i in seq_along(us_states)) {
        s <- us_states[i]
        s_norm <- normalize_state_names(s)
        match_idx <- which(present_norm == s_norm)
        if (length(match_idx) >= 1) {
          final_tb$Repatriados[i] <- sum(tb$Repatriados[match_idx], na.rm = TRUE)
        } else {
          final_tb$Repatriados[i] <- 0L
        }
      }
      missing_idx <- which(final_tb$Repatriados == 0)
      if (length(missing_idx) > 0) {
        missing <- final_tb$Estados[missing_idx]
        message(sprintf("[agg_repatriados_from_xlsx] Estados de EEUU sin registros en '%s' (se usarán 0): %s", path, paste(missing, collapse = ", ")))
      }
      # Crear columna con versión en español (para mostrar) y dejar 'Estados' en inglés para hacer merges
      final_tb$Estados_es <- sapply(final_tb$Estados, function(s) {
        if (s %in% names(eng_to_esp)) return(eng_to_esp[[s]])
        s_norm <- normalize_state_names(s)
        keys_norm <- normalize_state_names(names(eng_to_esp))
        idx <- which(keys_norm == s_norm)
        if (length(idx) >= 1) return(eng_to_esp[[names(eng_to_esp)[idx[1]]]])
        s_tc <- normalize_state_names(s)
        if (length(s_tc) >= 1) s_tc[1] else s
      }, USE.NAMES = FALSE)
      final_tb
    } else {
      # Tratar como datos de México: construir tabla con los estados MX
      final_tb <- data.frame(Estados = mx_states, Repatriaciones = integer(length(mx_states)), stringsAsFactors = FALSE)
      # tb tiene columnas Estados, Repatriados (conteo detectado)
      # Normalizar nombres y sumar
      tb_norm <- data.frame(Estados = tb$Estados, Repatriados = tb$Repatriados, stringsAsFactors = FALSE)
      tb_norm$norm <- normalize_state_names(tb_norm$Estados)
      mx_norm <- normalize_state_names(mx_states)
      for (i in seq_along(mx_states)) {
        idx <- which(tb_norm$norm == mx_norm[i])
        if (length(idx) >= 1) {
          final_tb$Repatriaciones[i] <- sum(tb_norm$Repatriados[idx], na.rm = TRUE)
        } else {
          final_tb$Repatriaciones[i] <- 0L
        }
      }
      # Añadir Estados_es (esp) y renombrar para compatibilidad con el módulo
      final_tb$Estados_es <- final_tb$Estados
      # Para compatibilidad con módulos que esperan 'Repatriados' como nombre de conteo, añadir esa columna
      final_tb$Repatriados <- final_tb$Repatriaciones
      # Mensaje con estados sin registros
      missing_idx <- which(final_tb$Repatriaciones == 0)
      if (length(missing_idx) > 0) {
        missing <- final_tb$Estados[missing_idx]
        message(sprintf("[agg_repatriados_from_xlsx] Estados de MX sin registros en '%s' (se usarán 0): %s", path, paste(missing, collapse = ", ")))
      }
      final_tb
    }
  # Crear columna con versión en español (para mostrar) y dejar 'Estados' en inglés para hacer merges
  final_tb$Estados_es <- sapply(final_tb$Estados, function(s) {
    # intentar coincidencia directa con la lista inglesa
    if (exists("eng_to_esp") && s %in% names(eng_to_esp)) return(eng_to_esp[[s]])
    # si no, intentar normalizar y buscar por versión normalizada
    if (exists("eng_to_esp")) {
      s_norm <- normalize_state_names(s)
      keys_norm <- normalize_state_names(names(eng_to_esp))
      idx <- which(keys_norm == s_norm)
      if (length(idx) >= 1) return(eng_to_esp[[names(eng_to_esp)[idx[1]]]])
    }
    # fallback: Title Case generico
    s_tc <- normalize_state_names(s)
    if (length(s_tc) >= 1) s_tc[1] else s
  }, USE.NAMES = FALSE)

  final_tb
}


normalize_state_names <- function(x) {
  if (is.null(x)) return(character(0))
  s <- as.character(x)
  s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT")
  s <- trimws(tolower(s))
  s <- gsub("[[:space:]]+", " ", s)
  s <- sapply(strsplit(s, " ", fixed = TRUE), function(words) {
    paste(toupper(substring(words, 1,1)), substring(words, 2), sep = "", collapse = " ")
  }, USE.NAMES = FALSE)
  s
}

# Cargar catálogo de albergues y normalizar columnas para su uso en módulos
get_albergues_data <- function(path = "data/cat_albergues.csv", force = FALSE) {
  loader <- function(p) {
    if (!file.exists(p)) {
      message("[get_albergues_data] file not found: ", p)
      return(tibble::tibble())
    }
    df <- tryCatch(utils::read.csv(p, stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, strip.white = TRUE),
                   error = function(e) stop("error reading albergues csv: ", e$message))
    if (nrow(df) == 0) return(tibble::tibble())

    # Normalizar nombres en minúsculas para matching
    names(df) <- tolower(names(df))

    # Mapear nombres esperados a formato usado por módulos (TitleCase)
    mapping <- list(
      id_albergue = "id_albergue",
      id_municipio = "id_municipio",
      descripcion = "Descripcion",
      direccion = "Direccion",
      telefono = "Telefono",
      latitude = "Latitud",
      longitude = "Longitud",
      capacidad = "Capacidad",
      disponibles = "Disponibles"
    )
    for (old in names(mapping)) {
      if (old %in% names(df)) names(df)[names(df) == old] <- mapping[[old]]
    }

    # Trim y normalizar texto
    char_idx <- vapply(df, is.character, logical(1))
    df[char_idx] <- lapply(df[char_idx], function(x) {
      x2 <- iconv(x, to = "UTF-8")
      trimws(x2)
    })

    # Forzar numéricos en columnas de coordenadas y capacidad
    num_cols <- c("Latitud", "Longitud", "Capacidad", "Disponibles")
    for (nc in num_cols) {
      if (nc %in% names(df)) {
        # eliminar caracteres no numéricos excepto signo menos y punto decimal
        df[[nc]] <- suppressWarnings(as.numeric(gsub("[^0-9\\.-]", "", as.character(df[[nc]]))))
      }
    }

    # Si existe el archivo que mapea municipios/estados, hacer left join por id_albergue
    muni_path <- "data/albergues_municipios.csv"
    if (file.exists(muni_path)) {
      try({
        muni <- utils::read.csv(muni_path, stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, strip.white = TRUE)
        # Asegurar columnas en minúsculas para matching simple
        names(muni) <- tolower(names(muni))
        if ("id_albergue" %in% names(muni)) {
          # Renombrar columnas para evitar confusiones
          if ("municipio" %in% names(muni)) names(muni)[names(muni) == "municipio"] <- "Municipio"
          if ("estado" %in% names(muni)) names(muni)[names(muni) == "estado"] <- "Entidad"
          # Convertir id_albergue en mismo tipo que df
          if ("id_albergue" %in% names(df)) {
            # A veces id_albergue viene como texto; forzar ambos a character para unir
            df$id_albergue <- as.character(df$id_albergue)
            muni$id_albergue <- as.character(muni$id_albergue)
            df <- merge(df, muni, by = "id_albergue", all.x = TRUE, sort = FALSE)
            # Si Entidad ya existía en df, preferir la existente (no sobreescribir) — si la nueva provee mejor información usarla
            if ("Entidad" %in% names(df) && any(is.na(df$Entidad))) {
              # Si tenemos una columna Entidad.x/Entidad.y por merge, limpiar
              # Pero en nuestro caso hemos renombrado muni para que el merge cree una única columna Entidad
            }
          }
        }
      }, silent = TRUE)
    }

    tibble::as_tibble(df)
  }

  # Si memoise está disponible, usar caché similar a get_repatriados_data
  if (requireNamespace("memoise", quietly = TRUE)) {
    if (!exists(".memo_albergues", envir = globalenv())) assign(".memo_albergues", memoise::memoise(loader), envir = globalenv())
    if (isTRUE(force)) {
      tryCatch(memoise::forget(get(".memo_albergues", envir = globalenv())), error = function(e) NULL)
    }
    tryCatch(get(".memo_albergues", envir = globalenv())(path), error = function(e) {
      message("[get_albergues_data] error loading ", path, ": ", e$message)
      tibble::tibble()
    })
  } else {
    tryCatch(loader(path), error = function(e) {
      message("[get_albergues_data] error loading ", path, ": ", e$message)
      tibble::tibble()
    })
  }
}
