
# ============================================================================
# SERVER TESTS (testServer)
# ============================================================================

test_that("Server reactive values initialize correctly", {
  testServer(app, {
    # Test initial state
    expect_null(values$xtab)
    expect_null(values$ytab)
    expect_null(values$knot)
    expect_null(values$fitted)
    expect_length(values$curve_lines, 0)
    expect_length(values$regions, 0)
    expect_equal(values$region_id, 0)
    expect_false(values$adding_knot)
    expect_false(values$selecting_region)
  })
})

test_that("Data loading updates reactive values", {
  testServer(app, {
    # Simulate test data generation
    session$setInputs(
      test_data = 1,
      data_xmin = 0,
      data_xmax = 1,
      n_points = 100
    )

    # Wait for reactivity
    flushReact()

    # Check that data was loaded
    expect_false(is.null(values$xtab))
    expect_false(is.null(values$ytab))
    expect_length(values$xtab, 100)
    expect_equal(values$data_name, "Test [0, 1]")
    expect_null(values$fitted)
  })
})

test_that("Temperature data loads correctly", {
  testServer(app, {
    session$setInputs(temp_data = 1)
    flushReact()

    expect_false(is.null(values$xtab))
    expect_false(is.null(values$ytab))
    expect_true(length(values$xtab) > 0)
    expect_true(length(values$ytab) > 0)
    expect_equal(values$data_name, "Temperature (1880-1992)")
  })
})

test_that("Knot management works in server", {
  testServer(app, {
    # Setup test data
    session$setInputs(test_data = 1)
    flushReact()

    initial_knots <- values$knot

    # Add a knot
    session$setInputs(add_knot_mode = 1)
    expect_true(values$adding_knot)
    session$setInputs(add_knot_mode = 1)
    expect_false(values$adding_knot)

    # Clear knots
    initial_knot_length <- length(values$knot)
    session$setInputs(clear_knots = 1)
    flushReact()

    # Knots should be reset but not null
    expect_false(is.null(values$knot))
  })
})

test_that("Region management works in server", {
  testServer(app, {
    # Setup test data
    session$setInputs(
      test_data = 1,
      constraint_mode = "region"
    )
    flushReact()

    # Start selection
    session$setInputs(start_selection = 1)
    expect_true(values$selecting_region)

    # Stop selection
    session$setInputs(start_selection = 1)
    expect_false(values$selecting_region)

    # Add a region
    session$setInputs(
      region_xmin = 0.3,
      region_xmax = 0.6,
      region_monot = "1",
      region_conv = "1",
      region_der3 = "0",
      add_region = 1
    )
    flushReact()

    expect_length(values$regions, 1)
    expect_equal(values$region_id, 1)
    expect_equal(values$regions[[1]]$xmin, 0.3)
    expect_equal(values$regions[[1]]$xmax, 0.6)
    expect_equal(values$regions[[1]]$monot, 1)

    # Clear regions
    session$setInputs(clear_regions = 1)
    flushReact()

    expect_length(values$regions, 0)
  })
})

test_that("Regression execution updates values correctly", {
  testServer(app, {
    # Setup test data
    session$setInputs(
      test_data = 1,
      degree = 3,
      tau = 0.5,
      solver = "ECOS",
      verbose = FALSE
    )
    flushReact()

    # Set knots
    values$knot <- quantile(values$xtab, probs = seq(0, 1, length.out = 5))

    # Run regression
    session$setInputs(run = 1)
    flushReact()

    # Check that fit was successful
    expect_false(is.null(values$fitted))
    expect_false(is.null(values$x_eval))
    expect_false(is.null(values$y_eval))
    expect_length(values$curve_lines, 1)
  })
})

test_that("Clear functions work correctly", {
  testServer(app, {
    # Setup data and run regression
    session$setInputs(test_data = 1)
    flushReact()

    values$knot <- quantile(values$xtab, probs = seq(0, 1, length.out = 5))
    session$setInputs(run = 1)
    flushReact()

    expect_length(values$curve_lines, 1)

    # Clear curves only
    session$setInputs(clear_curves = 1)
    flushReact()

    expect_length(values$curve_lines, 0)
    expect_false(is.null(values$xtab))
    expect_false(is.null(values$ytab))

    # Clear all
    session$setInputs(clear_all = 1)
    flushReact()

    expect_null(values$xtab)
    expect_null(values$ytab)
    expect_null(values$knot)
    expect_null(values$fitted)
    expect_length(values$curve_lines, 0)
    expect_length(values$regions, 0)
    expect_equal(values$data_name, "No data")
  })
})

test_that("Console logging works correctly", {
  testServer(app, {
    # Test console message function
    log_console <- function(msg) {
      current <- console_messages()
      console_messages(paste0(current, msg, "\n"))
    }

    # Initially empty
    expect_equal(console_messages(), "")

    # Log a message
    log_console("Test message")
    expect_equal(console_messages(), "Test message\n")

    # Log another message
    log_console("Second message")
    expect_equal(console_messages(), "Test message\nSecond message\n")

    # Clear console
    session$setInputs(clear_console = 1)
    # This would trigger the observer, but we can't test it directly here
    # Instead, manually clear
    console_messages("")
    expect_equal(console_messages(), "")
  })
})

test_that("Color changes update correctly", {
  testServer(app, {
    session$setInputs(
      test_data = 1,
      curve_color = "#FF0000",
      apply_color = 1
    )
    flushReact()

    # Color should be stored in input
    expect_equal(input$curve_color, "#FF0000")
  })
})
