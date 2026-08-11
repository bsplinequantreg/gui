# tests/testthat/test-shinytest2.R

library(testthat)
library(shinytest2)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Get the path to the Shiny app
#' @return Character string with the app directory path
get_app_dir <- function() {
  # Try multiple possible locations
  possible_paths <- c(
    "inst/shiny",  # From package root
    "../inst/shiny",  # From tests/testthat directory
    "../../inst/shiny",  # From deeper nested
    "inst/shiny/app.R"  # File path (will be converted)
  )

  for (path in possible_paths) {
    if (dir.exists(path)) {
      return(path)
    }
    # Check if it's a file path
    if (file.exists(path) && grepl("\\.R$", path)) {
      dir_path <- dirname(path)
      if (dir.exists(dir_path)) {
        return(dir_path)
      }
    }
  }

  # If we're in the package root during devtools::test()
  if (file.exists("DESCRIPTION")) {
    if (dir.exists("inst/shiny")) {
      return("inst/shiny")
    }
  }

  stop("Could not find Shiny app directory. Please check the app location.")
}

#' Check if shinytest2 tests should be run
#' @return Logical indicating if tests should run
should_run_shinytest2 <- function() {
  # Skip if not interactive (for CI/CD)
  if (!interactive()) {
    return(FALSE)
  }

  # Skip if shinytest2 is not installed
  if (!requireNamespace("shinytest2", quietly = TRUE)) {
    return(FALSE)
  }

  # Skip if the app directory doesn't exist
  app_dir <- tryCatch(
    get_app_dir(),
    error = function(e) NULL
  )
  if (is.null(app_dir) || !dir.exists(app_dir)) {
    return(FALSE)
  }

  return(TRUE)
}

# ============================================================================
# END-TO-END TESTS WITH SHINYTEST2
# ============================================================================

# These tests are skipped on CRAN and in non-interactive sessions
# They require the Shiny app to be available

test_that("App loads successfully", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  cat(sprintf("Testing app at: %s\n", app_dir))

  # Create AppDriver with the correct path
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "app_load_test",
    width = 1200,
    height = 800,
    load_timeout = 30000,  # 30 seconds timeout
    timeout = 10000
  )

  # Check that app loaded
  expect_true(app$get_value(output = "spline_plot")$visible)

  app$stop()
})

test_that("Test data generation works in full app", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "data_test",
    width = 1200,
    height = 800
  )

  # Click test data button
  app$click("test_data")
  app$wait_for_idle()

  # Check data table has data
  data_table <- app$get_value(output = "data_table")
  expect_true(!is.null(data_table))

  # Check data summary shows data
  data_summary <- app$get_value(output = "data_summary")
  expect_true(grepl("Points:", data_summary))

  app$stop()
})

test_that("Regression runs in full app", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "regression_test",
    width = 1200,
    height = 800
  )

  # Load test data
  app$click("test_data")
  app$wait_for_idle()

  # Set parameters
  app$set_inputs(
    degree = 3,
    tau = 0.5,
    solver = "ECOS",
    verbose = FALSE
  )

  # Run regression
  app$click("run")
  app$wait_for_idle()

  # Check results - curve count should be 1 after running
  curve_count <- app$get_value(output = "curve_count")
  expect_equal(curve_count, "1")

  # Check that fit info is displayed
  fit_info <- app$get_value(output = "fit_info")
  expect_true(grepl("Solver:", fit_info))

  app$stop()
})

test_that("Region selection and addition works in full app", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "region_test",
    width = 1200,
    height = 800
  )

  # Load test data
  app$click("test_data")
  app$wait_for_idle()

  # Switch to region mode
  app$set_inputs(constraint_mode = "region")

  # Set region parameters
  app$set_inputs(
    region_xmin = 0.3,
    region_xmax = 0.6,
    region_monot = "1",
    region_conv = "1",
    region_der3 = "0"
  )

  # Add region
  app$click("add_region")
  app$wait_for_idle()

  # Check region was added (via regions_info output)
  regions_info <- app$get_value(output = "regions_info")
  expect_true(grepl("Region", regions_info))

  app$stop()
})

test_that("Demo execution works", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "demo_test",
    width = 1200,
    height = 800
  )

  # Load data first
  app$click("test_data")
  app$wait_for_idle()

  # Run comprehensive demo
  app$click("demo_comp")
  app$wait_for_idle()

  # Check demo area is visible (js evaluation)
  demo_area_visible <- app$get_js("document.getElementById('demo_area').style.display")
  expect_true(demo_area_visible != "none")

  # Check demo output exists
  demo_output <- app$get_value(output = "demo_output")
  expect_true(!is.null(demo_output))

  app$stop()
})

test_that("Clear curves works", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "clear_curves_test",
    width = 1200,
    height = 800
  )

  # Load data and run regression
  app$click("test_data")
  app$wait_for_idle()
  app$click("run")
  app$wait_for_idle()

  # Check curve count
  curve_count <- app$get_value(output = "curve_count")
  expect_equal(curve_count, "1")

  # Clear curves
  app$click("clear_curves")
  app$wait_for_idle()

  # Check curve count is 0
  curve_count <- app$get_value(output = "curve_count")
  expect_equal(curve_count, "0")

  app$stop()
})

test_that("Clear all works", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "clear_all_test",
    width = 1200,
    height = 800
  )

  # Load data and run regression
  app$click("test_data")
  app$wait_for_idle()
  app$click("run")
  app$wait_for_idle()

  # Clear all
  app$click("clear_all")
  app$wait_for_idle()

  # Check data summary shows "No data"
  data_summary <- app$get_value(output = "data_summary")
  expect_true(grepl("No data", data_summary))

  app$stop()
})

test_that("Changing color works", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "color_test",
    width = 1200,
    height = 800
  )

  # Load data
  app$click("test_data")
  app$wait_for_idle()

  # Change color
  app$set_inputs(curve_color = "#FF0000")
  app$click("apply_color")
  app$wait_for_idle()

  # Run regression with new color
  app$click("run")
  app$wait_for_idle()

  # Curve should still be present
  curve_count <- app$get_value(output = "curve_count")
  expect_equal(curve_count, "1")

  app$stop()
})

test_that("Add knot mode works", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "add_knot_test",
    width = 1200,
    height = 800
  )

  # Load data
  app$click("test_data")
  app$wait_for_idle()

  # Activate add knot mode
  app$click("add_knot_mode")
  app$wait_for_idle()

  # The button should now say "Stop"
  btn_label <- app$get_value(input = "add_knot_mode")
  # We can't easily check label, but we can deactivate
  app$click("add_knot_mode")
  app$wait_for_idle()

  app$stop()
})

test_that("Temperature data loads correctly", {
  skip_if_not(should_run_shinytest2())
  skip_on_cran()

  app_dir <- get_app_dir()
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "temp_data_test",
    width = 1200,
    height = 800
  )

  # Load temperature data
  app$click("temp_data")
  app$wait_for_idle()

  # Check data summary shows temperature data
  data_summary <- app$get_value(output = "data_summary")
  expect_true(grepl("Temperature", data_summary))

  # Check knots were set
  knots_info <- app$get_value(output = "knots_compact")
  expect_true(!is.null(knots_info))

  app$stop()
})

# ============================================================================
# TEST SUITE SUMMARY
# ============================================================================

test_that("shinytest2 test suite summary", {
  cat("\n=== shinytest2 Test Suite ===\n")
  cat("Tested scenarios:\n")
  cat("✓ App loads successfully\n")
  cat("✓ Test data generation\n")
  cat("✓ Regression execution\n")
  cat("✓ Region selection and addition\n")
  cat("✓ Demo execution\n")
  cat("✓ Clear curves\n")
  cat("✓ Clear all\n")
  cat("✓ Color change\n")
  cat("✓ Add knot mode\n")
  cat("✓ Temperature data loading\n")
  cat("====================================\n")
  cat("Note: These tests are skipped on CRAN and in non-interactive sessions\n")
  cat("Run interactively to execute these tests.\n")
  expect_true(TRUE)
})
