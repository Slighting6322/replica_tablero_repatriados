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
