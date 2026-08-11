# tests/testthat/test-server.R

library(testthat)

# ============================================================================
# HELPER FUNCTIONS (matching app's server logic)
# ============================================================================

#' Create a mock server environment for testing
#' This mimics the app's reactive values and server logic
create_mock_server <- function() {
  # Use a closure to maintain state
  values <- list(
    xtab = NULL,
    ytab = NULL,
    knot = NULL,
    manual_knots = list(),
    adding_knot = FALSE,
    fitted = NULL,
    x_eval = NULL,
    y_eval = NULL,
    curve_lines = list(),
    regions = list(),
    data_name = "No data available",
    region_id = 0,
    selected_region_id = NULL,
    selecting_region = FALSE
  )

  # Return a list of functions that operate on the values
  list(
    # Get current state
    get_state = function() {
      return(values)
    },

    # Get a specific value
    get_value = function(name) {
      if (name %in% names(values)) {
        return(values[[name]])
      }
      return(NULL)
    },

    # Set a value
    set_value = function(name, value) {
      if (name %in% names(values)) {
        values[[name]] <<- value
        return(TRUE)
      }
      return(FALSE)
    },

    # Data generation
    generate_test_data = function(n = 200, xmin = 0, xmax = 1, seed = 42) {
      set.seed(seed)
      x <- seq(xmin, xmax, length.out = n)
      y <- 2 * x + 0.2 * sin(10 * pi * x) + 0.05 * rnorm(n)
      values$xtab <<- x
      values$ytab <<- y
      # Match the app's formatting: "Test [xmin, xmax]" with spaces
      values$data_name <<- paste("Test [", xmin, ",", xmax, "]", sep = " ")
      values$fitted <<- NULL
      values$curve_lines <<- list()
      values$regions <<- list()
      return(TRUE)
    },

    # Knot management
    set_knots = function(knots) {
      values$knot <<- knots
      return(TRUE)
    },

    add_manual_knot = function(x) {
      if (!is.null(values$xtab)) {
        if (x > min(values$xtab) && x < max(values$xtab)) {
          # Check if knot already exists (with tolerance)
          if (!any(abs(values$knot - x) < 1e-6)) {
            values$manual_knots <<- c(values$manual_knots, x)
            values$knot <<- sort(c(values$knot, x))
            return(TRUE)
          } else {
            # Knot already exists
            return(FALSE)
          }
        }
      }
      return(FALSE)
    },

    clear_knots = function() {
      values$manual_knots <<- list()
      if (!is.null(values$xtab)) {
        kn <- 9  # default knots_count - 1
        values$knot <<- quantile(values$xtab, probs = (0:kn) / kn)
      }
      return(TRUE)
    },

    # Region management
    add_region = function(xmin, xmax, monot = 0, conv = 0, der3 = 0) {
      if (is.null(xmin) || is.null(xmax) || xmin >= xmax) {
        return(FALSE)
      }
      values$region_id <<- values$region_id + 1
      region <- list(
        id = values$region_id,
        xmin = xmin,
        xmax = xmax,
        monot = monot,
        conv = conv,
        der3 = der3
      )
      values$regions <<- c(values$regions, list(region))
      return(TRUE)
    },

    delete_region = function(id) {
      if (is.null(id) || is.na(id) || id < 0) {
        return(FALSE)
      }
      exists <- any(sapply(values$regions, function(r) r$id == id))
      if (!exists) {
        return(FALSE)
      }
      values$regions <<- values$regions[!sapply(values$regions, function(r) r$id == id)]
      if (!is.null(values$selected_region_id) && values$selected_region_id == id) {
        values$selected_region_id <<- NULL
      }
      return(TRUE)
    },

    clear_regions = function() {
      values$regions <<- list()
      values$region_id <<- 0
      values$selected_region_id <<- NULL
      return(TRUE)
    },

    select_region = function(id) {
      exists <- any(sapply(values$regions, function(r) r$id == id))
      if (exists) {
        values$selected_region_id <<- id
        return(TRUE)
      }
      return(FALSE)
    },

    # Curve management
    add_curve = function(x, y, color = "blue") {
      values$curve_lines <<- c(values$curve_lines, list(list(
        x = x,
        y = y,
        color = color
      )))
      return(TRUE)
    },

    clear_curves = function() {
      values$curve_lines <<- list()
      return(TRUE)
    },

    clear_all = function() {
      values$xtab <<- NULL
      values$ytab <<- NULL
      values$knot <<- NULL
      values$fitted <<- NULL
      values$curve_lines <<- list()
      values$regions <<- list()
      values$region_id <<- 0
      values$selected_region_id <<- NULL
      values$data_name <<- "No data"
      return(TRUE)
    },

    # Toggle modes
    toggle_adding_knot = function() {
      values$adding_knot <<- !values$adding_knot
      return(values$adding_knot)
    },

    toggle_selecting_region = function() {
      values$selecting_region <<- !values$selecting_region
      return(values$selecting_region)
    }
  )
}

# ============================================================================
# SERVER TESTS
# ============================================================================

test_that("Server reactive values initialize correctly", {
  server <- create_mock_server()
  state <- server$get_state()

  expect_null(state$xtab)
  expect_null(state$ytab)
  expect_null(state$knot)
  expect_null(state$fitted)
  expect_length(state$curve_lines, 0)
  expect_length(state$regions, 0)
  expect_equal(state$region_id, 0)
  expect_equal(state$data_name, "No data available")
  expect_false(state$adding_knot)
  expect_false(state$selecting_region)
})

test_that("Data loading updates reactive values", {
  server <- create_mock_server()

  # Generate test data
  expect_true(server$generate_test_data(n = 100, xmin = 0, xmax = 1))

  state <- server$get_state()
  expect_length(state$xtab, 100)
  expect_length(state$ytab, 100)
  # Match the actual formatting from the app
  expect_equal(state$data_name, "Test [ 0 , 1 ]")
  expect_null(state$fitted)
  expect_length(state$curve_lines, 0)

  # Test with different parameters
  expect_true(server$generate_test_data(n = 50, xmin = -1, xmax = 1))
  state <- server$get_state()
  expect_length(state$xtab, 50)
  expect_equal(min(state$xtab), -1)
  expect_equal(max(state$xtab), 1)
  expect_equal(state$data_name, "Test [ -1 , 1 ]")
})

test_that("Knot management works in server", {
  server <- create_mock_server()

  # Generate data first
  server$generate_test_data(n = 100, xmin = 0, xmax = 1)

  # Set initial knots (excluding 0.5 so we can add it)
  test_knots <- c(0, 0.25, 0.75, 1)
  expect_true(server$set_knots(test_knots))

  state <- server$get_state()
  expect_equal(state$knot, test_knots)
  expect_false(0.5 %in% state$knot)  # 0.5 should not be in knots yet

  # Add manual knot (0.5 should be valid and not duplicate)
  expect_true(server$add_manual_knot(0.5))

  state <- server$get_state()
  expect_true(0.5 %in% state$knot)
  expect_equal(length(state$knot), 5)  # Should have 5 knots now

  # Try to add duplicate
  expect_false(server$add_manual_knot(0.5))

  # Try to add knot outside range
  expect_false(server$add_manual_knot(-0.1))
  expect_false(server$add_manual_knot(1.1))

  # Clear knots
  expect_true(server$clear_knots())
  state <- server$get_state()
  expect_false(is.null(state$knot))
  # After clear, knots should be quantiles
  expect_true(length(state$knot) > 0)
})

test_that("Region management works in server", {
  server <- create_mock_server()

  # Add regions
  expect_true(server$add_region(0.3, 0.6, 1, 0, 0))
  state <- server$get_state()
  expect_length(state$regions, 1)
  expect_equal(state$region_id, 1)
  expect_equal(state$regions[[1]]$xmin, 0.3)
  expect_equal(state$regions[[1]]$xmax, 0.6)
  expect_equal(state$regions[[1]]$monot, 1)

  # Add more regions
  expect_true(server$add_region(0.5, 0.7, -1, 1, 0))
  expect_true(server$add_region(0.2, 0.4, 0, 0, 1))
  state <- server$get_state()
  expect_length(state$regions, 3)
  expect_equal(state$region_id, 3)

  # Select region
  expect_true(server$select_region(2))
  state <- server$get_state()
  expect_equal(state$selected_region_id, 2)

  # Delete region
  expect_true(server$delete_region(2))
  state <- server$get_state()
  expect_length(state$regions, 2)
  expect_null(state$selected_region_id)

  # Clear regions
  expect_true(server$clear_regions())
  state <- server$get_state()
  expect_length(state$regions, 0)
  expect_equal(state$region_id, 0)
})

test_that("Curve management works in server", {
  server <- create_mock_server()

  # Add curves
  x <- seq(0, 1, length.out = 100)
  y <- runif(100)

  expect_true(server$add_curve(x, y, "blue"))
  state <- server$get_state()
  expect_length(state$curve_lines, 1)
  expect_equal(state$curve_lines[[1]]$color, "blue")
  expect_equal(state$curve_lines[[1]]$x, x)
  expect_equal(state$curve_lines[[1]]$y, y)

  expect_true(server$add_curve(x, y * 2, "red"))
  state <- server$get_state()
  expect_length(state$curve_lines, 2)
  expect_equal(state$curve_lines[[2]]$color, "red")

  # Clear curves
  expect_true(server$clear_curves())
  state <- server$get_state()
  expect_length(state$curve_lines, 0)
})

test_that("Clear all works correctly", {
  server <- create_mock_server()

  # Setup some data
  server$generate_test_data()
  server$set_knots(c(0, 0.25, 0.5, 0.75, 1))
  server$add_region(0.3, 0.6)
  server$add_curve(seq(0, 1, length.out = 100), runif(100))

  state <- server$get_state()
  expect_false(is.null(state$xtab))
  expect_false(is.null(state$ytab))
  expect_false(is.null(state$knot))
  expect_length(state$regions, 1)
  expect_length(state$curve_lines, 1)

  # Clear all
  expect_true(server$clear_all())
  state <- server$get_state()

  expect_null(state$xtab)
  expect_null(state$ytab)
  expect_null(state$knot)
  expect_null(state$fitted)
  expect_length(state$curve_lines, 0)
  expect_length(state$regions, 0)
  expect_equal(state$region_id, 0)
  expect_equal(state$data_name, "No data")
})

test_that("Adding knot mode toggles correctly", {
  server <- create_mock_server()

  # Initial state
  expect_false(server$get_state()$adding_knot)

  # Toggle on
  expect_true(server$toggle_adding_knot())
  expect_true(server$get_state()$adding_knot)

  # Toggle off
  expect_false(server$toggle_adding_knot())
  expect_false(server$get_state()$adding_knot)
})

test_that("Selecting region mode toggles correctly", {
  server <- create_mock_server()

  # Initial state
  expect_false(server$get_state()$selecting_region)

  # Toggle on
  expect_true(server$toggle_selecting_region())
  expect_true(server$get_state()$selecting_region)

  # Toggle off
  expect_false(server$toggle_selecting_region())
  expect_false(server$get_state()$selecting_region)
})

test_that("Region validation prevents invalid regions", {
  server <- create_mock_server()

  # Valid region
  expect_true(server$add_region(0.3, 0.6))

  # Invalid: xmin > xmax
  expect_false(server$add_region(0.6, 0.3))

  # Invalid: xmin == xmax
  expect_false(server$add_region(0.5, 0.5))

  # Invalid: NULL values
  expect_false(server$add_region(NULL, 0.5))
  expect_false(server$add_region(0.5, NULL))

  # Count should remain 1
  state <- server$get_state()
  expect_length(state$regions, 1)
})

test_that("Delete region with invalid ID fails", {
  server <- create_mock_server()

  # Add a region
  server$add_region(0.3, 0.6)
  expect_length(server$get_state()$regions, 1)

  # Delete with invalid ID
  expect_false(server$delete_region(NULL))
  expect_false(server$delete_region(NA))
  expect_false(server$delete_region(-1))
  expect_false(server$delete_region(99))

  # Should still have the region
  expect_length(server$get_state()$regions, 1)
})

test_that("Manual knot addition validates input", {
  server <- create_mock_server()

  # Generate data first
  server$generate_test_data(n = 100, xmin = 0, xmax = 1)

  # Set initial knots (without 0.4)
  server$set_knots(c(0, 0.25, 0.5, 0.75, 1))

  # Try to add a knot that already exists
  expect_false(server$add_manual_knot(0.5))  # 0.5 already exists

  # Try to add a knot outside range
  expect_false(server$add_manual_knot(-0.1))
  expect_false(server$add_manual_knot(1.1))

  # Add a new valid knot
  expect_true(server$add_manual_knot(0.4))
  state <- server$get_state()
  expect_true(0.4 %in% state$knot)

  # Try to add duplicate
  expect_false(server$add_manual_knot(0.4))
})

test_that("Get value and set value work correctly", {
  server <- create_mock_server()

  # Get non-existent value
  expect_null(server$get_value("nonexistent"))

  # Set value
  expect_true(server$set_value("data_name", "Test data"))
  expect_equal(server$get_value("data_name"), "Test data")

  # Set non-existent value
  expect_false(server$set_value("nonexistent", "value"))
})

# ============================================================================
# INTEGRATION TESTS WITH MOCK DATA
# ============================================================================

test_that("Full workflow with server logic works", {
  server <- create_mock_server()

  # 1. Load data
  server$generate_test_data(n = 100, xmin = 0, xmax = 1)
  state <- server$get_state()
  expect_false(is.null(state$xtab))
  expect_length(state$xtab, 100)

  # 2. Set knots
  knots <- quantile(state$xtab, probs = seq(0, 1, length.out = 6))
  server$set_knots(knots)
  state <- server$get_state()
  expect_length(state$knot, 6)

  # 3. Add regions
  server$add_region(0.3, 0.5, 1, 0, 0)
  server$add_region(0.6, 0.8, -1, 1, 0)
  state <- server$get_state()
  expect_length(state$regions, 2)

  # 4. Select region
  server$select_region(1)
  state <- server$get_state()
  expect_equal(state$selected_region_id, 1)

  # 5. Add curves
  server$add_curve(seq(0, 1, length.out = 50), runif(50), "blue")
  state <- server$get_state()
  expect_length(state$curve_lines, 1)

  # 6. Clear curves
  server$clear_curves()
  state <- server$get_state()
  expect_length(state$curve_lines, 0)

  # 7. Clear all
  server$clear_all()
  state <- server$get_state()
  expect_null(state$xtab)
  expect_null(state$ytab)
  expect_null(state$knot)
  expect_length(state$regions, 0)
  expect_length(state$curve_lines, 0)
  expect_equal(state$data_name, "No data")
})

# ============================================================================
# TEST SUITE SUMMARY
# ============================================================================

test_that("Server test suite summary", {
  cat("\n=== Server Test Suite ===\n")
  cat("Tested scenarios:\n")
  cat("✓ Reactive values initialization\n")
  cat("✓ Data loading\n")
  cat("✓ Knot management\n")
  cat("✓ Region management\n")
  cat("✓ Curve management\n")
  cat("✓ Clear all functionality\n")
  cat("✓ Adding knot mode toggle\n")
  cat("✓ Selecting region mode toggle\n")
  cat("✓ Region validation\n")
  cat("✓ Delete region with invalid ID\n")
  cat("✓ Manual knot validation\n")
  cat("✓ Get/set value functions\n")
  cat("✓ Full workflow integration\n")
  cat("====================================\n")
  expect_true(TRUE)
})
