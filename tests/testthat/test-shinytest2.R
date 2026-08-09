
# ============================================================================
# END-TO-END TESTS (shinytest2)
# ============================================================================

# Note: These tests require the app to be running or use shinytest2's recording feature

test_that("App loads successfully", {
  # Skip if not running interactively or if app not available
  skip_if_not(interactive())

  app <- AppDriver$new(
    app_dir = system.file("R/run_gui.R", package = "BsplineQuantReg"),
    name = "gui_test",
    width = 1200,
    height = 800
  )

  # Check that app loaded
  expect_true(app$get_value(output = "spline_plot")$visible)

  app$stop()
})

test_that("Test data generation works in full app", {
  skip_if_not(interactive())

  app <- AppDriver$new(
    app_dir = system.file("R/run_gui.R", package = "BsplineQuantReg"),
    name = "data_test"
  )

  # Click test data button
  app$click("test_data")
  app$wait_for_idle()

  # Check data was loaded
  expect_false(is.null(app$get_value(input = "xtab")))

  app$stop()
})

test_that("Regression runs in full app", {
  skip_if_not(interactive())

  app <- AppDriver$new(
    app_dir = system.file("R/run_gui.R", package = "BsplineQuantReg"),
    name = "regression_test"
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

  # Check results
  plot_output <- app$get_value(output = "spline_plot")
  expect_true(!is.null(plot_output))

  # Check curve count
  curve_count <- app$get_value(output = "curve_count")
  expect_equal(curve_count, "1")

  app$stop()
})

test_that("Region selection and addition works in full app", {
  skip_if_not(interactive())

  app <- AppDriver$new(
    app_dir = system.file("R/run_gui.R", package = "BsplineQuantReg"),
    name = "region_test"
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

  # Check region was added (via output)
  regions_info <- app$get_value(output = "regions_info")
  expect_true(grepl("Region", regions_info))

  app$stop()
})

test_that("Demo execution works", {
  skip_if_not(interactive())

  app <- AppDriver$new(
    app_dir = system.file("R/run_gui.R", package = "BsplineQuantReg"),
    name = "demo_test"
  )

  # Load data first
  app$click("test_data")
  app$wait_for_idle()

  # Run comprehensive demo
  app$click("demo_comp")
  app$wait_for_idle()

  # Check demo area is visible
  demo_area <- app$get_value(js = "document.getElementById('demo_area').style.display")
  # This may not work in all cases, but we can check that output exists
  demo_output <- app$get_value(output = "demo_output")
  expect_true(!is.null(demo_output))

  app$stop()
})

test_that("CSV import works", {
  skip_if_not(interactive())
  skip("CSV import requires manual file selection")

  # This test would require providing a CSV file
  app <- AppDriver$new(
    app_dir = system.file("R/run_gui.R", package = "BsplineQuantReg"),
    name = "csv_test"
  )

  # The load_csv button opens a file dialog, which can't be automated easily
  # This would need to be tested manually or with a mock
  app$stop()
})

# ============================================================================
# PERFORMANCE TESTS
# ============================================================================

test_that("Large dataset performance", {
  skip_if_not(interactive())
  skip_on_cran()

  testServer(app, {
    # Create large dataset
    n <- 5000
    session$setInputs(
      n_points = n,
      test_data = 1
    )
    flushReact()

    # Measure time to generate
    time_start <- Sys.time()
    session$setInputs(test_data = 1)
    flushReact()
    time_end <- Sys.time()

    # Should be fast (< 1 second)
    expect_lt(as.numeric(difftime(time_end, time_start, units = "secs")), 1)

    expect_length(values$xtab, n)
  })
})

test_that("Multiple regression performance", {
  skip_if_not(interactive())
  skip_on_cran()

  testServer(app, {
    # Setup
    session$setInputs(test_data = 1)
    flushReact()
    values$knot <- quantile(values$xtab, probs = seq(0, 1, length.out = 10))

    # Run multiple regressions
    time_start <- Sys.time()

    for (tau in seq(0.1, 0.9, by = 0.2)) {
      session$setInputs(tau = tau, run = 1)
      flushReact()
    }

    time_end <- Sys.time()

    # Should be reasonable (< 30 seconds)
    expect_lt(as.numeric(difftime(time_end, time_start, units = "secs")), 30)

    # At least one curve should be present
    expect_true(length(values$curve_lines) > 0)
  })
})
