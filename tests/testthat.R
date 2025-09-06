library(testthat)
library(pkgload)

# Run tests in this package-like project
testthat::test_dir("tests/testthat", reporter = "summary")
