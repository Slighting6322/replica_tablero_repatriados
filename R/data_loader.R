# R/data_loader.R
## R/data_loader.R
## Utilities to load and preprocess datasets used by the app.

# Public functions provided:
# - get_repatriados_data(path = "data/repatriados_sample.csv", force = FALSE)
# - get_fecha_corte(path = NULL, data = NULL)
# - agg_repatriados_from_xlsx(path = "data/repatriados.xlsx", sheet = "Repatriados", state_col = NULL)
# - normalize_state_names(x)
# - clear_repatriados_cache()  (no-op unless memoise available)

loader_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# Forward declarations (help static code checkers); real implementations follow below
get_repatriados_data <- function(path = "data/repatriados_sample.csv", force = FALSE) {
  tibble::tibble()
}
clear_repatriados_cache <- function() invisible(NULL)

# get_repatriados_data: uses memoise::memoise if available to cache in-memory
if (requireNamespace("memoise", quietly = TRUE)) {
  .memo_loader <- memoise::memoise(loader_read_csv)
  get_repatriados_data <- function(path = "data/repatriados_sample.csv", force = FALSE) {
    if (isTRUE(force)) {
      tryCatch(memoise::forget(.memo_loader), error = function(e) NULL)
    }
    tryCatch(.memo_loader(path), error = function(e) {
      message("[data_loader] error loading ", path, ": ", e$message)
      tibble::tibble()
    })
  }
  clear_repatriados_cache <- function() {
    tryCatch(memoise::forget(.memo_loader), error = function(e) NULL)
    invisible(TRUE)
  }
} else {
  get_repatriados_data <- function(path = "data/repatriados_sample.csv", force = FALSE) {
    tryCatch(loader_read_csv(path), error = function(e) {
      message("[data_loader] error loading ", path, ": ", e$message)
      tibble::tibble()
    })
  }
  clear_repatriados_cache <- function() invisible(NULL)
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

  df <- tryCatch(readxl::read_excel(path, sheet = sheet), error = function(e) stop("error reading xlsx: ", e$message))
  if (nrow(df) == 0) return(data.frame(Estados = character(0), Repatriados = integer(0), stringsAsFactors = FALSE))

  # No remover duplicados, ya que cada fila es una repatriación
  # df_unique <- df[!duplicated(df), , drop = FALSE]
  col_candidates <- names(df)
  chosen <- NULL
  if (!is.null(state_col) && state_col %in% col_candidates) {
    chosen <- state_col
  } else {
    hits <- grep("estado", tolower(col_candidates), value = TRUE)
    if (length(hits) >= 1) chosen <- hits[1]
  }
  if (is.null(chosen)) stop("state column not found in xlsx (looked for '", state_col, "' or 'estado' in column names)")

  states_vec <- as.character(df[[chosen]])
  states_vec <- trimws(states_vec)
  states_vec[states_vec == "" | is.na(states_vec)] <- "(sin_estado)"

  tb <- as.data.frame(table(states_vec), stringsAsFactors = FALSE)
  names(tb) <- c("Estados", "Repatriados")
  tb$Repatriados <- as.integer(tb$Repatriados)
  tb
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
