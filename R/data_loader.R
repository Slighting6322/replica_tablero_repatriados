# R/data_loader.R
# Utilities to load project data in one place and optionally cache it.

#' Get repatriados data (cached if memoise is installed)
#'
#' This function centralizes loading of the main dataset used across the app.
#' It will use memoise::memoise for in-memory caching when available.
#'
#' @param path Character path to the CSV file. Default: "data/repatriados_sample.csv".
#' @param force Logical, if TRUE forces reload even when cached (only when memoise available).
#' @return A tibble with the dataset, or an empty tibble on error.
get_repatriados_data <- local({
  loader <- function(path) {
    # readr is used for robust CSV parsing
    readr::read_csv(path, show_col_types = FALSE)
  }

  if (requireNamespace("memoise", quietly = TRUE)) {
    memo_loader <- memoise::memoise(loader)

    function(path = "data/repatriados_sample.csv", force = FALSE) {
      if (force) {
        tryCatch(memoise::forget(memo_loader), error = function(e) NULL)
      }
      tryCatch(memo_loader(path), error = function(e) {
        message("[data_loader] error loading ", path, ": ", e$message)
        tibble::tibble()
      })
    }
  } else {
    function(path = "data/repatriados_sample.csv", force = FALSE) {
      tryCatch(loader(path), error = function(e) {
        message("[data_loader] error loading ", path, ": ", e$message)
        tibble::tibble()
      })
    }
  }
})

#' Clear cached repatriados data (no-op if memoise not available)
clear_repatriados_cache <- function() {
  if (requireNamespace("memoise", quietly = TRUE)) {
    # We don't have direct access to the memoised object here; instruct user to call get_repatriados_data(force = TRUE)
    invisible(TRUE)
  } else {
    invisible(NULL)
  }
}


#' Get the latest (max) date from the repatriados dataset
#'
#' This helper accepts either a data frame (data) or a path to a CSV (path).
#' It will try several parsers (base as.Date, lubridate::ymd/dmy/mdy, then anytime::anydate)
#' and return the maximum date (Date) or NA if parsing fails.
#'
get_fecha_corte <- function(path = NULL, data = NULL) {
  df <- NULL
  if (!is.null(data)) {
    df <- data
  } else if (!is.null(path)) {
    df <- get_repatriados_data(path)
  } else {
    stop("either 'path' or 'data' must be provided")
  }

  if (nrow(df) == 0 || ncol(df) == 0) return(as.Date(NA))

  col_name <- if ("fecha_repatriacion" %in% names(df)) {
    "fecha_repatriacion"
  } else {
    names(df)[1]
  }

  vec <- df[[col_name]]
  # If already Date/POSIX, take max
  if (inherits(vec, c("Date", "POSIXt"))) return(as.Date(max(vec, na.rm = TRUE)))

  # Try parsers
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
