library(testthat)

context("agg_repatriados_from_xlsx")

# Ensure helper is available (tests run with wd = tests/testthat)
if (file.exists("../R/data_loader.R")) {
  try(source("../R/data_loader.R"), silent = TRUE)
}

test_that("agg_repatriados_from_xlsx removes duplicates and aggregates by estado", {
  skip_if_not(requireNamespace("readxl", quietly = TRUE))
  # Create a temporary xlsx file using openxlsx if available, otherwise skip
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    skip("openxlsx needed to create temporary test xlsx")
  }
  tmp <- tempfile(fileext = ".xlsx")
  df <- data.frame(
    NOMBRE = c("A","A","B","C","C","C"),
    APELLIDOS = c("X","X","Y","Z","Z","Z"),
    `EDAD DE REPATRIACION` = c(30,30,25,40,40,40),
    SEXO = c("M","M","F","M","M","M"),
    `ESTADO DE DETENCION` = c("California","California","Texas","New York","New York","New York"),
    stringsAsFactors = FALSE
  )
  openxlsx::write.xlsx(df, file = tmp, sheetName = "Repatriados", overwrite = TRUE)

  res <- agg_repatriados_from_xlsx(path = tmp, sheet = "Repatriados")
  # Expect 2 states: California (1 unique row after dedup), Texas (1), New York (1)
  expect_true(all(c("California", "Texas", "New York") %in% res$Estados))
  # California should count 1 (because two identical rows were deduped)
  cal <- res$Repatriados[res$Estados == "California"]
  ny <- res$Repatriados[res$Estados == "New York"]
  tx <- res$Repatriados[res$Estados == "Texas"]
  expect_equal(cal, 1)
  expect_equal(tx, 1)
  expect_equal(ny, 1)
})
