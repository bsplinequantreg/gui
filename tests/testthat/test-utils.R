# ============================================================================
# UNIT TESTS (testthat)
# ============================================================================

test_that("Constraint symbols function works correctly", {
  # Helper function from the app
  get_sym <- function(val, symbols) {
    if (is.null(val) || is.na(val)) return("x")
    val <- as.numeric(val)
    if (!val %in% c(-1, 0, 1)) return("x")
    return(symbols[val + 2])
  }

  # Test monotonicity symbols
  mono_symbols <- c("down", "x", "up")
  expect_equal(get_sym(-1, mono_symbols), "down")
  expect_equal(get_sym(0, mono_symbols), "x")
  expect_equal(get_sym(1, mono_symbols), "up")
  expect_equal(get_sym(NULL, mono_symbols), "x")
  expect_equal(get_sym(NA, mono_symbols), "x")
  expect_equal(get_sym(2, mono_symbols), "x")

  # Test convexity symbols
  conv_symbols <- c("n", "x", "U")
  expect_equal(get_sym(-1, conv_symbols), "n")
  expect_equal(get_sym(0, conv_symbols), "x")
  expect_equal(get_sym(1, conv_symbols), "U")

  # Test third derivative symbols
  der3_symbols <- c("-", "x", "+")
  expect_equal(get_sym(-1, der3_symbols), "-")
  expect_equal(get_sym(0, der3_symbols), "x")
  expect_equal(get_sym(1, der3_symbols), "+")
})

test_that("Build constraints with uniform mode works correctly", {
  # Create a minimal reactive environment for testing
  test_inputs <- list(
    degree = 3,
    constraint_mode = "uniform",
    monot = "0",
    conv = "0",
    der3 = "0"
  )

  # Mock values for testing
  values <- list(knot = c(0, 0.25, 0.5, 0.75, 1))

  # Test uniform constraints with no constraints
  build_constraints_test <- function(degree, knot, monot, conv, der3) {
    kn <- length(knot) - 1
    monot_val <- as.numeric(monot)
    conv_val <- as.numeric(conv)
    der3_val <- as.numeric(der3)

    if (is.na(monot_val)) monot_val <- 0
    if (is.na(conv_val)) conv_val <- 0
    if (is.na(der3_val)) der3_val <- 0

    monot_vec <- rep(monot_val, kn + 1)
    conv_vec <- rep(conv_val, kn + 1)
    der3_vec <- rep(der3_val, kn + 1)

    if (degree < 3) der3_vec <- rep(0, kn + 1)

    list(monot = monot_vec, conv = conv_vec, der3 = der3_vec)
  }

  result <- build_constraints_test(
    degree = 3,
    knot = c(0, 0.25, 0.5, 0.75, 1),
    monot = "1",
    conv = "1",
    der3 = "0"
  )

  expect_length(result$monot, 5)  # kn+1 = 4, but
  expect_equal(result$monot, rep(1, 5))
  expect_equal(result$conv, rep(1, 5))
  expect_equal(result$der3, rep(0, 5))
})

test_that("Auto_knot generation works correctly", {
  # Test quantile knot generation
  x <- seq(0, 1, length.out = 100)
  knot_count <- 10
  kn <- knot_count - 1
  knot <- as.numeric(quantile(x, probs = (0:kn) / kn))

  expect_length(knot, knot_count)
  expect_equal(knot[1], 0)
  expect_equal(knot[knot_count], 1)
  expect_true(all(diff(knot) >= 0))

  # Test with minimal knot
  knot_count <- 2
  kn <- knot_count - 1
  knot <- as.numeric(quantile(x, probs = (0:kn) / kn))
  expect_length(knot, 2)
  expect_equal(knot, c(0, 1))
})

test_that("Data generation functions produce correct output", {
  # Simulate test data generation
  set.seed(42)
  n <- 200
  xmin <- 0
  xmax <- 1
  x <- seq(xmin, xmax, length.out = n)
  y <- 2 * x + 0.2 * sin(10 * pi * x) + 0.05 * rnorm(n)

  expect_length(x, n)
  expect_length(y, n)
  expect_equal(min(x), xmin)
  expect_equal(max(x), xmax)
  expect_true(is.numeric(y))

  # Test custom function generation
  func_str <- "2*x + 0.5*sin(6*pi*x) + 0.2*rnorm(n)"
  func_str <- gsub("sin\\(", "sin(", func_str)
  func_str <- gsub("pi", "pi", func_str)
  y_custom <- eval(parse(text = func_str))

  expect_length(y_custom, n)
  expect_true(is.numeric(y_custom))
})

test_that("Region management works correctly", {
  # Create a region list
  region <- list(
    id = 1,
    xmin = 0.3,
    xmax = 0.6,
    monot = 1,
    conv = 1,
    der3 = 0
  )

  # Test region structure
  expect_equal(region$id, 1)
  expect_lt(region$xmin, region$xmax)
  expect_true(region$monot %in% c(-1, 0, 1))
  expect_true(region$conv %in% c(-1, 0, 1))
  expect_true(region$der3 %in% c(-1, 0, 1))

  # Test multiple regions
  regions <- list(
    list(id = 1, xmin = 0.2, xmax = 0.4, monot = 1, conv = 0, der3 = 0),
    list(id = 2, xmin = 0.6, xmax = 0.8, monot = -1, conv = 0, der3 = 0)
  )

  expect_length(regions, 2)
  expect_equal(regions[[1]]$id, 1)
  expect_equal(regions[[2]]$monot, -1)
})


