
# ============================================================================
# EDGE CASE TESTS
# ============================================================================
library(BsplineQuantReg)
app_dir <- system.file("shiny", package = "BsplineQuantRegGui")
app<-app_dir

test_that("Invalid data handling", {
  testServer(app, {
    # Try to run regression with no data
    session$setInputs(run = 1)
    flushReact()

    # Should still have no fit
    expect_null(values$fitted)

    # Try with invalid knots
    values$knot <- c(0, 1)  # minimal knots
    session$setInputs(run = 1)
    flushReact()

    # Should still run with minimal knots
    expect_false(is.null(values$fitted))
  })
})

test_that("Invalid region handling", {
  testServer(app, {
    # Setup
    session$setInputs(
      test_data = 1,
      constraint_mode = "region"
    )
    flushReact()

    # Try to add region with xmin > xmax
    session$setInputs(
      region_xmin = 0.6,
      region_xmax = 0.3,
      add_region = 1
    )
    flushReact()

    # Region should not be added
    expect_length(values$regions, 0)

    # Try with valid region
    session$setInputs(
      region_xmin = 0.3,
      region_xmax = 0.6,
      add_region = 1
    )
    flushReact()

    expect_length(values$regions, 1)
  })
})

test_that("Negative constraints handling", {
  testServer(app, {
    session$setInputs(
      test_data = 1,
      monot = "-1",  # decreasing
      conv = "-1",   # concave
      der3 = "-1",   # negative third derivative
      run = 1
    )
    flushReact()

    # Should run without errors
    expect_false(is.null(values$fitted))
  })
})
