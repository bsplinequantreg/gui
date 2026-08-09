# tests/testthat/test-gui.R

library(testthat)
library(shiny)
library(shinytest2)
#library(shiny.testthat)
library(BsplineQuantReg)


# ============================================================================
# MOCK AND HELPER FUNCTIONS
# ============================================================================

# Helper to create test data
create_test_data <- function(n = 100, xmin = 0, xmax = 1) {
  set.seed(42)
  x <- seq(xmin, xmax, length.out = n)
  y <- 2 * x + 0.2 * sin(10 * pi * x) + 0.05 * rnorm(n)
  list(x = x, y = y)
}

# Mock for file selection
mock_file_selection <- function(path) {
  # Would need to use mockery or similar package
  # This is a placeholder
}

app_dir <- system.file("shiny", package = "BsplineQuantRegGui")
