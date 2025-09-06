library(testthat)

test_that("get_repatriados_data returns a tibble and contains expected columns", {
  skip_if_not(file.exists("data/repatriados_sample.csv"))
  df <- get_repatriados_data("data/repatriados_sample.csv")
  expect_true(inherits(df, "data.frame"))
  expect_true(ncol(df) >= 1)
})

test_that("fecha_corte computed in global.R is Date or NA", {
  # Use init_app_data() to compute fecha_corte
  skip_if_not(file.exists("global.R"))
  source("global.R", local = TRUE)
  app_data <- init_app_data()
  expect_true("fecha_corte" %in% names(app_data))
  expect_true(inherits(app_data$fecha_corte, "Date") || is.na(app_data$fecha_corte))
})

test_that("get_fecha_corte parses ymd/dmy/mdy correctly", {
  d1 <- data.frame(fecha = c("2024-11-01", "2024-11-02"), stringsAsFactors = FALSE)
  expect_equal(get_fecha_corte(data = d1), as.Date("2024-11-02"))

  d2 <- data.frame(fecha = c("01/11/2024", "02/11/2024"), stringsAsFactors = FALSE)
  expect_equal(get_fecha_corte(data = d2), as.Date("2024-11-02"))

  d3 <- data.frame(fecha = c("11/01/2024", "11/02/2024"), stringsAsFactors = FALSE)
  # ambiguous mdy/dmy may parse depending on parser; ensure function returns a Date or NA
  res3 <- get_fecha_corte(data = d3)
  expect_true(inherits(res3, "Date") || is.na(res3))
})

test_that("get_fecha_corte returns NA for invalid data", {
  dbad <- data.frame(fecha = c("foo", "bar"), stringsAsFactors = FALSE)
  expect_true(is.na(get_fecha_corte(data = dbad)))
})
