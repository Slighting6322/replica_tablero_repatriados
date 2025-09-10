## helper-data_loader.R
# This helper is loaded automatically by testthat before tests run.
# It ensures the package / script helpers in R/data_loader.R are available
# to test files which execute with working dir = tests/testthat.

data_loader_path <- "../R/data_loader.R"
if (!file.exists(data_loader_path)) {
  stop("Required file not found for tests: ", data_loader_path)
}
source(data_loader_path)
