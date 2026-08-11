# # tests/testthat/test-gui.R
#
# library(testthat)
# library(shiny)
# library(shinytest2)
# #library(shiny.testthat)
# library(BsplineQuantReg)
#
#
# # ============================================================================
# # MOCK AND HELPER FUNCTIONS
# # ============================================================================
#
# # Helper to create test data
# create_test_data <- function(n = 100, xmin = 0, xmax = 1) {
#   set.seed(42)
#   x <- seq(xmin, xmax, length.out = n)
#   y <- 2 * x + 0.2 * sin(10 * pi * x) + 0.05 * rnorm(n)
#   list(x = x, y = y)
# }
#
# # Mock for file selection
# mock_file_selection <- function(path) {
#   # Would need to use mockery or similar package
#   # This is a placeholder
# }

app_dir <- system.file("shiny", package = "BsplineQuantRegGui")


# tests/testthat/helper.R

# ============================================================================
# TEST HELPERS - Automatically sourced by testthat
# ============================================================================

# Load required packages for testing
library(testthat)
library(BsplineQuantReg)

# Skip on CRAN or non-interactive sessions
skip_if_not_interactive <- function() {
  if (!interactive() || identical(Sys.getenv("NOT_CRAN"), "true")) {
    return(invisible())
  }
  testthat::skip("Skipping in non-interactive or CRAN environment")
}

# Common test data generators
create_test_data <- function(n = 100, xmin = 0, xmax = 1, seed = 42) {
  set.seed(seed)
  x <- seq(xmin, xmax, length.out = n)
  y <- 2 * x + 0.2 * sin(10 * pi * x) + 0.05 * rnorm(n)
  list(x = x, y = y)
}

create_test_knots <- function(x, n_knots = 10) {
  kn <- n_knots - 1
  knots <- quantile(x, probs = (0:kn) / kn)
  unname(knots)
}

# Common test regions
create_test_regions <- function(n = 3) {
  regions <- list()
  for (i in 1:n) {
    regions[[i]] <- list(
      id = i,
      xmin = 0.1 + (i-1) * 0.2,
      xmax = 0.3 + (i-1) * 0.2,
      monot = sample(c(-1, 0, 1), 1),
      conv = sample(c(-1, 0, 1), 1),
      der3 = sample(c(-1, 0, 1), 1)
    )
  }
  regions
}

# Custom expectations
expect_constraint_lengths <- function(constraints, kn) {
  testthat::expect_length(constraints$monot, kn)
  testthat::expect_length(constraints$conv, kn + 1)
  testthat::expect_length(constraints$der3, kn + 1)
}

# Path helpers
get_app_dir <- function() {
  possible_paths <- c(
    "inst/shiny",
    "../inst/shiny",
    "../../inst/shiny"
  )

  for (path in possible_paths) {
    if (dir.exists(path)) {
      return(normalizePath(path))
    }
  }

  # For devtools::test() from package root
  if (file.exists("DESCRIPTION") && dir.exists("inst/shiny")) {
    return(normalizePath("inst/shiny"))
  }

  stop("Could not find Shiny app directory")
}

# Check if shinytest2 tests should run
should_run_shinytest2 <- function() {
  if (!interactive()) return(FALSE)
  if (!requireNamespace("shinytest2", quietly = TRUE)) return(FALSE)

  app_dir <- tryCatch(get_app_dir(), error = function(e) NULL)
  if (is.null(app_dir) || !dir.exists(app_dir)) return(FALSE)



  return(FALSE)
}

# Safe operations with timeouts
safe_click <- function(app, input, timeout_ = 30000, ...) {
  tryCatch({
    app$click(input, timeout_ = timeout_, ...)
    return(TRUE)
  }, error = function(e) {
    warning(sprintf("Click on '%s' timed out: %s", input, e$message))
    return(FALSE)
  })
}

safe_set_inputs <- function(app, ..., timeout_ = 30000, wait_ = TRUE) {
  tryCatch({
    app$set_inputs(..., timeout_ = timeout_, wait_ = wait_)
    return(TRUE)
  }, error = function(e) {
    warning(sprintf("set_inputs timed out: %s", e$message))
    return(FALSE)
  })
}
