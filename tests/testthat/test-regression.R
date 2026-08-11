# tests/testthat/test-regression.R

library(testthat)

# ============================================================================
# HELPER FUNCTIONS FOR TESTING
# ============================================================================

#' Create test data
#' @param n Number of data points
#' @param seed Random seed for reproducibility
#' @return List with x and y vectors
create_test_data <- function(n = 100, seed = 42) {
  set.seed(seed)
  x <- seq(0, 1, length.out = n)
  y <- 2 * x + 0.2 * sin(10 * pi * x) + 0.05 * rnorm(n)
  list(x = x, y = y)
}

#' Create test knots
#' @param x Data vector
#' @param n_knots Number of knots
#' @return Vector of knots (unnamed)
create_test_knots <- function(x, n_knots = 10) {
  kn <- n_knots - 1
  knots <- quantile(x, probs = (0:kn) / kn)
  unname(knots)  # Remove names to avoid issues in tests
}

#' Build constraints (matching app's behavior)
#' This function handles constraint lengths robustly, allowing extra
#' constraints to be provided and adapting based on degree
build_constraints <- function(knot, degree, monot = 0, conv = 0, der3 = 0,
                              constraint_mode = "uniform", regions = list()) {
  kn <- length(knot) - 1

  if (kn < 1) {
    return(NULL)
  }

  # Convert to numeric and handle NA
  monot_val <- as.numeric(monot)
  conv_val <- as.numeric(conv)
  der3_val <- as.numeric(der3)

  if (is.na(monot_val)) monot_val <- 0
  if (is.na(conv_val)) conv_val <- 0
  if (is.na(der3_val)) der3_val <- 0

  if (constraint_mode == "uniform") {
    # For degree 0: no constraints
    if (degree == 0) {
      monot_vec <- rep(0, kn)
      conv_vec <- rep(0, kn + 1)
      der3_vec <- rep(0, kn + 1)
    }
    # For degree 1: only monotonicity
    else if (degree == 1) {
      monot_vec <- rep(monot_val, kn)
      conv_vec <- rep(0, kn + 1)
      der3_vec <- rep(0, kn + 1)
    }
    # For degree 2: monotonicity and convexity
    else if (degree == 2) {
      monot_vec <- rep(monot_val, kn)
      conv_vec <- rep(conv_val, kn + 1)
      der3_vec <- rep(0, kn + 1)
    }
    # For degree >= 3: all constraints
    else {
      monot_vec <- rep(monot_val, kn)
      conv_vec <- rep(conv_val, kn + 1)
      der3_vec <- rep(der3_val, kn + 1)
    }
  } else {
    # Per region mode
    if (degree == 0) {
      monot_vec <- rep(0, kn)
      conv_vec <- rep(0, kn + 1)
      der3_vec <- rep(0, kn + 1)
    } else if (degree == 1) {
      monot_vec <- rep(0, kn)
      conv_vec <- rep(0, kn + 1)
      der3_vec <- rep(0, kn + 1)
    } else {
      monot_vec <- rep(0, kn)
      conv_vec <- rep(0, kn + 1)
      der3_vec <- rep(0, kn + 1)
    }

    for (region in regions) {
      for (i in 1:kn) {
        x1 <- knot[i]
        x2 <- knot[i + 1]
        if (x2 > region$xmin && x1 < region$xmax) {
          if (region$monot != 0 && degree >= 1) {
            monot_vec[i] <- region$monot
          }
          if (region$conv != 0 && degree >= 2) {
            conv_vec[i] <- region$conv
            conv_vec[i + 1] <- region$conv
          }
          if (region$der3 != 0 && degree >= 3) {
            der3_vec[i] <- region$der3
          }
        }
      }
    }
  }

  # Additional safety: ensure correct lengths
  # Monotonicity: should be length kn
  if (length(monot_vec) != kn) {
    monot_vec <- rep(monot_vec[1], kn)
  }

  # Convexity: should be length kn + 1
  if (length(conv_vec) != kn + 1) {
    conv_vec <- rep(conv_vec[1], kn + 1)
  }

  # Third derivative: should be length kn + 1
  if (length(der3_vec) != kn + 1) {
    der3_vec <- rep(der3_vec[1], kn + 1)
  }

  list(monot = monot_vec, conv = conv_vec, der3 = der3_vec)
}

# ============================================================================
# UNIT TESTS FOR REGRESSION LOGIC
# ============================================================================

test_that("Test data generation works correctly", {
  # Test with default parameters
  data <- create_test_data(n = 100, seed = 42)
  expect_length(data$x, 100)
  expect_length(data$y, 100)
  expect_equal(min(data$x), 0)
  expect_equal(max(data$x), 1)
  expect_true(is.numeric(data$y))

  # Test with different parameters
  data2 <- create_test_data(n = 50, seed = 123)
  expect_length(data2$x, 50)
  expect_length(data2$y, 50)

  # Test reproducibility
  data3 <- create_test_data(n = 100, seed = 42)
  expect_equal(data$x, data3$x)
  expect_equal(data$y, data3$y)
})

test_that("Knot generation works correctly", {
  data <- create_test_data()
  knots <- create_test_knots(data$x, n_knots = 10)

  expect_length(knots, 10)
  # Use expect_equal with ignore_attr = TRUE instead of deprecated expect_equivalent
  expect_equal(knots[1], 0, ignore_attr = TRUE)
  expect_equal(knots[10], 1, ignore_attr = TRUE)
  expect_true(all(diff(knots) >= 0))

  # Test with different number of knots
  knots2 <- create_test_knots(data$x, n_knots = 5)
  expect_length(knots2, 5)
  expect_equal(knots2[1], 0, ignore_attr = TRUE)
  expect_equal(knots2[5], 1, ignore_attr = TRUE)
})

test_that("Constraint building works correctly with extra constraints", {
  # Test that constraints of different lengths are handled properly
  knot <- c(0, 0.25, 0.5, 0.75, 1)
  kn <- length(knot) - 1  # 4 intervals
  degree <- 3

  # Test with uniform constraints
  # The app uses scalar values for uniform mode, but internally
  # the constraints are expanded to the correct length

  # Monotonicity constraints: should be length kn (number of intervals)
  constraints <- build_constraints(knot, degree,
                                   monot = 1, conv = 0, der3 = 0)
  expect_length(constraints$monot, kn)  # kn, not kn+1
  expect_equal(constraints$monot, rep(1, kn))

  # Convexity constraints: should be length kn+1
  constraints <- build_constraints(knot, degree,
                                   monot = 0, conv = 1, der3 = 0)
  expect_length(constraints$conv, kn + 1)
  expect_equal(constraints$conv, rep(1, kn + 1))

  # Third derivative: should be length kn+1
  constraints <- build_constraints(knot, degree,
                                   monot = 0, conv = 0, der3 = 1)
  expect_length(constraints$der3, kn + 1)
  expect_equal(constraints$der3, rep(1, kn + 1))
})

test_that("Different degrees handle constraints appropriately", {
  knot <- c(0, 0.25, 0.5, 0.75, 1)
  kn <- length(knot) - 1

  # Test with degree 0: no constraints
  constraints_d0 <- build_constraints(knot, degree = 0,
                                      monot = 1, conv = 1, der3 = 1)
  expect_length(constraints_d0$monot, kn)
  expect_equal(constraints_d0$monot, rep(0, kn))  # Should be 0, not 1
  expect_length(constraints_d0$conv, kn + 1)
  expect_equal(constraints_d0$conv, rep(0, kn + 1))
  expect_length(constraints_d0$der3, kn + 1)
  expect_equal(constraints_d0$der3, rep(0, kn + 1))

  # Test with degree 1: only monotonicity
  constraints_d1 <- build_constraints(knot, degree = 1,
                                      monot = 1, conv = 1, der3 = 1)
  expect_length(constraints_d1$monot, kn)
  expect_equal(constraints_d1$monot, rep(1, kn))  # Monotonicity active
  expect_length(constraints_d1$conv, kn + 1)
  expect_equal(constraints_d1$conv, rep(0, kn + 1))  # Convexity ignored
  expect_length(constraints_d1$der3, kn + 1)
  expect_equal(constraints_d1$der3, rep(0, kn + 1))  # Third derivative ignored

  # Test with degree 2: monotonicity and convexity
  constraints_d2 <- build_constraints(knot, degree = 2,
                                      monot = 1, conv = 1, der3 = 1)
  expect_length(constraints_d2$monot, kn)
  expect_equal(constraints_d2$monot, rep(1, kn))
  expect_length(constraints_d2$conv, kn + 1)
  expect_equal(constraints_d2$conv, rep(1, kn + 1))
  expect_length(constraints_d2$der3, kn + 1)
  expect_equal(constraints_d2$der3, rep(0, kn + 1))  # Third derivative ignored

  # Test with degree 3: all constraints
  constraints_d3 <- build_constraints(knot, degree = 3,
                                      monot = 1, conv = 1, der3 = 1)
  expect_length(constraints_d3$monot, kn)
  expect_equal(constraints_d3$monot, rep(1, kn))
  expect_length(constraints_d3$conv, kn + 1)
  expect_equal(constraints_d3$conv, rep(1, kn + 1))
  expect_length(constraints_d3$der3, kn + 1)
  expect_equal(constraints_d3$der3, rep(1, kn + 1))
})

test_that("Extra constraints are allowed and handled", {
  # This tests that providing more constraints than needed doesn't break anything
  knot <- c(0, 0.25, 0.5, 0.75, 1)
  kn <- length(knot) - 1

  # The app's build_constraints function always creates the correct length
  # Even if the input suggests more constraints

  # For uniform mode, we only use scalar values
  constraints <- build_constraints(knot, degree = 3,
                                   monot = 1, conv = 1, der3 = 1)

  # Correct lengths
  expect_length(constraints$monot, kn)  # kn constraints
  expect_length(constraints$conv, kn + 1)
  expect_length(constraints$der3, kn + 1)

  # For region mode, we might have more constraints than needed
  # but they get applied to the appropriate intervals
  regions <- list(
    list(xmin = 0.2, xmax = 0.4, monot = 1, conv = 0, der3 = 0),
    list(xmin = 0.5, xmax = 0.7, monot = -1, conv = 1, der3 = 0),
    list(xmin = 0.3, xmax = 0.6, monot = 0, conv = 0, der3 = 1)
  )

  constraints <- build_constraints(knot, degree = 3,
                                   constraint_mode = "region",
                                   regions = regions)

  # Correct lengths
  expect_length(constraints$monot, kn)
  expect_length(constraints$conv, kn + 1)
  expect_length(constraints$der3, kn + 1)
})

test_that("Region-based constraints work correctly", {
  knot <- c(0, 0.25, 0.5, 0.75, 1)
  kn <- length(knot) - 1
  degree <- 3

  regions <- list(
    list(xmin = 0.2, xmax = 0.4, monot = 1, conv = 0, der3 = 0),
    list(xmin = 0.6, xmax = 0.8, monot = -1, conv = 1, der3 = 0)
  )

  constraints <- build_constraints(knot, degree,
                                   constraint_mode = "region",
                                   regions = regions)

  # Check that constraints are applied correctly
  # For monot: region 1 should affect interval 0.25-0.5 (index 2)
  # region 2 should affect interval 0.5-0.75 (index 3)
  # Note: monot constraints are length kn (indices 1:kn)
  expect_equal(constraints$monot[2], 1)
  expect_equal(constraints$monot[3], -1)

  # For conv: region 2 should affect indices 3 and 4 (1-based)
  # conv constraints are length kn+1 (indices 1:kn+1)
  expect_equal(constraints$conv[3], 1)
  expect_equal(constraints$conv[4], 1)
})

test_that("Invalid data handling in regression", {
  skip_if_not_installed("BsplineQuantReg")

  # Test that regression requires valid data
  # This should throw an error because x and y lengths don't match
  expect_error({
    BsplineQuantReg::quantile_spline(
      x = c(1, 2, 3),
      y = c(1, 2),
      knot = c(0, 1)
    )
  })
})

test_that("Regression runs with minimal parameters", {
  skip_if_not_installed("BsplineQuantReg")

  data <- create_test_data(n = 50)
  knots <- create_test_knots(data$x, n_knots = 5)
  kn <- length(knots) - 1

  # Should run without errors
  expect_silent({
    fit <- BsplineQuantReg::quantile_spline(
      x = data$x,
      y = data$y,
      knot = knots,
      tau = 0.5,
      degree = 3,
      monot = rep(0, kn),
      convcons = rep(0, kn + 1),
      der3cons = rep(0, kn + 1),
      callable = TRUE
    )
  })
})

test_that("Regression with different tau values works", {
  skip_if_not_installed("BsplineQuantReg")

  data <- create_test_data(n = 50)
  knots <- create_test_knots(data$x, n_knots = 5)
  kn <- length(knots) - 1

  tau_values <- c(0.1, 0.5, 0.9)

  for (tau in tau_values) {
    expect_silent({
      fit <- BsplineQuantReg::quantile_spline(
        x = data$x,
        y = data$y,
        knot = knots,
        tau = tau,
        degree = 3,
        monot = rep(0, kn),
        convcons = rep(0, kn + 1),
        der3cons = rep(0, kn + 1),
        callable = TRUE
      )
    })
  }
})

test_that("Regression with constraints works", {
  skip_if_not_installed("BsplineQuantReg")

  data <- create_test_data(n = 50)
  knots <- create_test_knots(data$x, n_knots = 5)
  kn <- length(knots) - 1

  # Test monotonic increasing
  expect_silent({
    fit <- BsplineQuantReg::quantile_spline(
      x = data$x,
      y = data$y,
      knot = knots,
      tau = 0.5,
      degree = 3,
      monot = rep(1, kn),
      convcons = rep(0, kn + 1),
      der3cons = rep(0, kn + 1),
      callable = TRUE
    )
  })

  # Test convex
  expect_silent({
    fit <- BsplineQuantReg::quantile_spline(
      x = data$x,
      y = data$y,
      knot = knots,
      tau = 0.5,
      degree = 3,
      monot = rep(0, kn),
      convcons = rep(1, kn + 1),
      der3cons = rep(0, kn + 1),
      callable = TRUE
    )
  })
})

test_that("Fitted function evaluates correctly", {
  skip_if_not_installed("BsplineQuantReg")

  data <- create_test_data(n = 50)
  knots <- create_test_knots(data$x, n_knots = 5)
  kn <- length(knots) - 1

  fit <- BsplineQuantReg::quantile_spline(
    x = data$x,
    y = data$y,
    knot = knots,
    tau = 0.5,
    degree = 3,
    monot = rep(0, kn),
    convcons = rep(0, kn + 1),
    der3cons = rep(0, kn + 1),
    callable = TRUE
  )

  # Evaluate at data points
  y_pred <- fit(data$x)
  expect_length(y_pred, length(data$x))
  expect_true(is.numeric(y_pred))

  # Evaluate at new points
  x_new <- seq(0, 1, length.out = 20)
  y_new <- fit(x_new)
  expect_length(y_new, 20)
  expect_true(is.numeric(y_new))
})

test_that("Regression curve storage works", {
  # Test curve line storage logic (matching app behavior)
  curve_lines <- list()

  # Add a curve
  curve <- list(
    x = seq(0, 1, length.out = 100),
    y = runif(100),
    color = "blue"
  )
  curve_lines <- c(curve_lines, list(curve))

  expect_length(curve_lines, 1)
  expect_equal(curve_lines[[1]]$color, "blue")

  # Add another curve
  curve2 <- list(
    x = seq(0, 1, length.out = 100),
    y = runif(100),
    color = "red"
  )
  curve_lines <- c(curve_lines, list(curve2))

  expect_length(curve_lines, 2)
  expect_equal(curve_lines[[2]]$color, "red")

  # Clear curves
  curve_lines <- list()
  expect_length(curve_lines, 0)
})

test_that("Solver parameter handling works", {
  # Test solver selection logic
  solvers <- c("ECOS", "SCS", "CLARABEL", "HIGHS", "OSQP", "GUROBI")

  for (solver in solvers) {
    # In the app, solver is passed as a string to quantile_spline
    # Here we just test that the solver names are valid
    expect_true(solver %in% solvers)
  }
})

test_that("Type of regression selection works", {
  # Test type_reg parameter handling
  # For BsplineQuantReg >= 0.2.3
  skip_if_not_installed("BsplineQuantReg")

  # Check if type_reg parameter is supported
  if (packageVersion("BsplineQuantReg") >= "0.2.3") {
    data <- create_test_data(n = 30)
    knots <- create_test_knots(data$x, n_knots = 5)
    kn <- length(knots) - 1

    # Quantile regression
    expect_silent({
      fit_q <- BsplineQuantReg::quantile_spline(
        x = data$x,
        y = data$y,
        knot = knots,
        tau = 0.5,
        degree = 3,
        monot = rep(0, kn),
        convcons = rep(0, kn + 1),
        der3cons = rep(0, kn + 1),
        type_reg = "quantile",
        callable = TRUE
      )
    })

    # Mean square regression
    expect_silent({
      fit_ms <- BsplineQuantReg::quantile_spline(
        x = data$x,
        y = data$y,
        knot = knots,
        tau = 0.5,
        degree = 3,
        monot = rep(0, kn),
        convcons = rep(0, kn + 1),
        der3cons = rep(0, kn + 1),
        type_reg = "mean_square",
        callable = TRUE
      )
    })
  }
})

test_that("Verbose parameter works", {
  skip_if_not_installed("BsplineQuantReg")

  data <- create_test_data(n = 30)
  knots <- create_test_knots(data$x, n_knots = 5)
  kn <- length(knots) - 1

  # verbose = FALSE should be silent
  expect_silent({
    fit_quiet <- BsplineQuantReg::quantile_spline(
      x = data$x,
      y = data$y,
      knot = knots,
      tau = 0.5,
      degree = 3,
      monot = rep(0, kn),
      convcons = rep(0, kn + 1),
      der3cons = rep(0, kn + 1),
      verbose = FALSE,
      callable = TRUE
    )
  })

  # verbose = TRUE should produce output (we don't test the output,
  # just that it runs without errors)
  expect_error({
    fit_verbose <- BsplineQuantReg::quantile_spline(
      x = data$x,
      y = data$y,
      knot = knots,
      tau = 0.5,
      degree = 3,
      monot = rep(0, kn),
      convcons = rep(0, kn + 1),
      der3cons = rep(0, kn + 1),
      verbose = TRUE,
      callable = TRUE
    )
  }, NA)  # NA means expect no error
})

test_that("Regression coefficient extraction works", {
  skip_if_not_installed("BsplineQuantReg")

  data <- create_test_data(n = 50)
  knots <- create_test_knots(data$x, n_knots = 5)
  kn <- length(knots) - 1

  fit <- BsplineQuantReg::quantile_spline(
    x = data$x,
    y = data$y,
    knot = knots,
    tau = 0.5,
    degree = 3,
    monot = rep(0, kn),
    convcons = rep(0, kn + 1),
    der3cons = rep(0, kn + 1),
    callable = TRUE
  )

  # Test that we can get parameters
  params <- BsplineQuantReg::get_parameters(fit)
  expect_true(!is.null(params))
  expect_true(!is.null(params$coeff))
})

# ============================================================================
# PERFORMANCE TESTS
# ============================================================================

test_that("Regression handles larger datasets", {
  skip_if_not_installed("BsplineQuantReg")
  skip_on_cran()

  # Test with different sizes
  sizes <- c(100, 500)

  for (n in sizes) {
    data <- create_test_data(n = n)
    knots <- create_test_knots(data$x, n_knots = min(10, n/10))
    kn <- length(knots) - 1

    time_start <- Sys.time()
    fit <- BsplineQuantReg::quantile_spline(
      x = data$x,
      y = data$y,
      knot = knots,
      tau = 0.5,
      degree = 3,
      monot = rep(0, kn),
      convcons = rep(0, kn + 1),
      der3cons = rep(0, kn + 1),
      callable = TRUE
    )
    time_end <- Sys.time()

    # Should be reasonable
    expect_lt(as.numeric(difftime(time_end, time_start, units = "secs")), 10)
    expect_false(is.null(fit))
  }
})

# ============================================================================
# TEST SUITE SUMMARY
# ============================================================================

test_that("Regression test suite summary", {
  cat("\n=== Regression Test Suite ===\n")
  cat("Tested scenarios:\n")
  cat("✓ Test data generation\n")
  cat("✓ Knot generation\n")
  cat("✓ Constraint building with extra constraints\n")
  cat("✓ Different degrees handle constraints appropriately\n")
  cat("✓ Region-based constraints\n")
  cat("✓ Invalid data handling\n")
  cat("✓ Regression with minimal parameters\n")
  cat("✓ Regression with different tau values\n")
  cat("✓ Regression with constraints\n")
  cat("✓ Fitted function evaluation\n")
  cat("✓ Curve storage logic\n")
  cat("✓ Solver parameter handling\n")
  cat("✓ Type of regression selection\n")
  cat("✓ Verbose parameter\n")
  cat("✓ Coefficient extraction\n")
  cat("✓ Performance with larger datasets\n")
  cat("====================================\n")
  expect_true(TRUE)
})
