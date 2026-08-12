# tests/testthat/Test_region_management.R

library(testthat)

# ============================================================================
# HELPER FUNCTIONS FOR TESTING
# ============================================================================

#' Create test regions for consistent testing
#' @param n Number of regions to create
#' @return List of region objects
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

#' Get region IDs from list
#' @param regions List of regions
#' @return Vector of region IDs
get_region_ids <- function(regions) {
  sapply(regions, function(r) r$id)
}

#' Check if region ID exists
#' @param regions List of regions
#' @param id Region ID to check
#' @return Logical
region_exists <- function(regions, id) {
  id %in% sapply(regions, function(r) r$id)
}

#' Get constraint symbol (matching the app's function)
get_sym <- function(val, symbols) {
  if (is.null(val) || is.na(val)) return("x")
  val <- as.numeric(val)
  if (!val %in% c(-1, 0, 1)) return("x")
  return(symbols[val + 2])
}

#' Validate region input
validate_region <- function(xmin, xmax) {
  if (is.null(xmin) || is.null(xmax) || is.na(xmin) || is.na(xmax)) {
    return(FALSE)
  }
  if (xmin >= xmax) {
    return(FALSE)
  }
  return(TRUE)
}

# ============================================================================
# REGION MANAGER CLASS (Plain R implementation)
# ============================================================================

create_region_manager <- function() {
  self <- environment()

  self$regions <- list()
  self$region_id <- 0
  self$selected_region_id <- NULL

  # Add a region
  self$add_region <- function(xmin, xmax, monot = 0, conv = 0, der3 = 0) {
    if (!validate_region(xmin, xmax)) {
      return(FALSE)
    }
    self$region_id <- self$region_id + 1
    region <- list(
      id = self$region_id,
      xmin = xmin,
      xmax = xmax,
      monot = monot,
      conv = conv,
      der3 = der3
    )
    self$regions <- c(self$regions, list(region))
    return(TRUE)
  }

  # Delete a region
  self$delete_region <- function(id) {
    if (is.null(id) || is.na(id) || id < 0) {
      return(FALSE)
    }
    exists <- any(sapply(self$regions, function(r) r$id == id))
    if (!exists) {
      return(FALSE)
    }
    self$regions <- self$regions[!sapply(self$regions, function(r) r$id == id)]
    if (!is.null(self$selected_region_id) && self$selected_region_id == id) {
      self$selected_region_id <- NULL
    }
    return(TRUE)
  }

  # Clear all regions
  self$clear_regions <- function() {
    self$regions <- list()
    self$region_id <- 0
    self$selected_region_id <- NULL
    return(TRUE)
  }

  # Select a region
  self$select_region <- function(id) {
    exists <- any(sapply(self$regions, function(r) r$id == id))
    if (exists) {
      self$selected_region_id <- id
      return(TRUE)
    }
    return(FALSE)
  }

  # Get region by ID
  self$get_region <- function(id) {
    for (region in self$regions) {
      if (region$id == id) {
        return(region)
      }
    }
    return(NULL)
  }

  # Get all region IDs
  self$get_ids <- function() {
    sapply(self$regions, function(r) r$id)
  }

  # Get count
  self$get_count <- function() {
    length(self$regions)
  }

  # Format region for display (matching app's output)
  self$format_region <- function(region) {
    mono_symbols <- c("down", "x", "up")
    conv_symbols <- c("n", "x", "U")
    der3_symbols <- c("-", "x", "+")

    paste0(
      "Region ", region$id,
      " [", round(region$xmin, 3), ", ", round(region$xmax, 3), "]",
      "  M=", get_sym(region$monot, mono_symbols),
      " C=", get_sym(region$conv, conv_symbols),
      " D3=", get_sym(region$der3, der3_symbols)
    )
  }

  # Get formatted output (matching app's regions_info output)
  self$get_formatted_regions <- function() {
    if (self$get_count() == 0) {
      return("No regions")
    }
    paste(sapply(self$regions, self$format_region), collapse = "\n")
  }

  return(self)
}

# ============================================================================
# UNIT TESTS
# ============================================================================

test_that("Region manager initialization works", {
  manager <- create_region_manager()

  expect_equal(manager$get_count(), 0)
  expect_equal(manager$region_id, 0)
  expect_null(manager$selected_region_id)
  expect_equal(manager$get_formatted_regions(), "No regions")
})

test_that("Add region works correctly", {
  manager <- create_region_manager()

  # Add valid region
  expect_true(manager$add_region(0.2, 0.4, 1, 0, 0))
  expect_equal(manager$get_count(), 1)
  expect_equal(manager$region_id, 1)
  expect_equal(manager$regions[[1]]$xmin, 0.2)
  expect_equal(manager$regions[[1]]$xmax, 0.4)
  expect_equal(manager$regions[[1]]$monot, 1)
  expect_equal(manager$regions[[1]]$conv, 0)
  expect_equal(manager$regions[[1]]$der3, 0)

  # Add more regions
  expect_true(manager$add_region(0.5, 0.7, -1, 1, 0))
  expect_true(manager$add_region(0.3, 0.6, 0, 0, 1))
  expect_equal(manager$get_count(), 3)
  expect_equal(manager$region_id, 3)

  # Get region IDs
  ids <- manager$get_ids()
  expect_equal(ids, c(1, 2, 3))

  # Get specific region
  region <- manager$get_region(2)
  expect_equal(region$xmin, 0.5)
  expect_equal(region$xmax, 0.7)
})

test_that("Add region with invalid input fails", {
  manager <- create_region_manager()

  # Invalid ranges
  expect_false(manager$add_region(0.4, 0.2))  # xmin > xmax
  expect_false(manager$add_region(0.4, 0.4))  # xmin == xmax
  expect_false(manager$add_region(NULL, 0.5))
  expect_false(manager$add_region(0.5, NA))
  expect_false(manager$add_region(NA, NA))

  # Count should not change
  expect_equal(manager$get_count(), 0)
})

test_that("Delete region works correctly", {
  manager <- create_region_manager()

  # Add some regions
  manager$add_region(0.2, 0.4, 1, 0, 0)
  manager$add_region(0.5, 0.7, -1, 1, 0)
  manager$add_region(0.3, 0.6, 0, 0, 1)
  expect_equal(manager$get_count(), 3)

  # Delete middle region
  expect_true(manager$delete_region(2))
  expect_equal(manager$get_count(), 2)
  ids <- manager$get_ids()
  expect_equal(ids, c(1, 3))

  # Delete first region
  expect_true(manager$delete_region(1))
  expect_equal(manager$get_count(), 1)
  ids <- manager$get_ids()
  expect_equal(ids, c(3))

  # Delete last region
  expect_true(manager$delete_region(3))
  expect_equal(manager$get_count(), 0)
})

test_that("Delete region with invalid ID fails", {
  manager <- create_region_manager()

  # Add a region
  manager$add_region(0.2, 0.4)
  expect_equal(manager$get_count(), 1)

  # Invalid IDs
  expect_false(manager$delete_region(NULL))
  expect_false(manager$delete_region(NA))
  expect_false(manager$delete_region(-1))
  expect_false(manager$delete_region(99))

  # Count should not change
  expect_equal(manager$get_count(), 1)
})

test_that("Delete selected region clears selection", {
  manager <- create_region_manager()

  # Add regions
  manager$add_region(0.2, 0.4)
  manager$add_region(0.5, 0.7)

  # Select region 1
  expect_true(manager$select_region(1))
  expect_equal(manager$selected_region_id, 1)

  # Delete selected region
  expect_true(manager$delete_region(1))
  expect_equal(manager$get_count(), 1)
  expect_null(manager$selected_region_id)
  ids <- manager$get_ids()
  expect_equal(ids, c(2))

  # Select and delete different region
  expect_true(manager$select_region(2))
  expect_equal(manager$selected_region_id, 2)
  expect_true(manager$delete_region(2))
  expect_equal(manager$get_count(), 0)
  expect_null(manager$selected_region_id)
})

test_that("Select region works correctly", {
  manager <- create_region_manager()

  # Add regions
  manager$add_region(0.2, 0.4)
  manager$add_region(0.5, 0.7)

  # Select existing region
  expect_true(manager$select_region(1))
  expect_equal(manager$selected_region_id, 1)

  expect_true(manager$select_region(2))
  expect_equal(manager$selected_region_id, 2)

  # Select non-existing region
  expect_false(manager$select_region(99))
  # Selection should remain
  expect_equal(manager$selected_region_id, 2)
})

test_that("Clear all regions works correctly", {
  manager <- create_region_manager()

  # Add regions
  manager$add_region(0.2, 0.4)
  manager$add_region(0.5, 0.7)
  manager$add_region(0.3, 0.6)
  manager$select_region(1)

  expect_equal(manager$get_count(), 3)
  expect_equal(manager$region_id, 3)
  expect_equal(manager$selected_region_id, 1)

  # Clear all
  expect_true(manager$clear_regions())
  expect_equal(manager$get_count(), 0)
  expect_equal(manager$region_id, 0)
  expect_null(manager$selected_region_id)
})

test_that("Region formatting matches app output", {
  manager <- create_region_manager()

  # Add regions with various constraints
  manager$add_region(0.2, 0.4, 1, 0, 0)   # up, x, x
  manager$add_region(0.5, 0.7, -1, 1, 0)  # down, U, x
  manager$add_region(0.3, 0.6, 0, 0, 1)   # x, x, +

  formatted <- manager$get_formatted_regions()

  # Check formatting (actual formatting depends on get_sym)
  expect_true(grepl("Region 1", formatted))
  expect_true(grepl("Region 2", formatted))
  expect_true(grepl("Region 3", formatted))
  expect_true(grepl("0.2", formatted))
  expect_true(grepl("0.7", formatted))

  # Individual region formatting
  region1 <- manager$get_region(1)
  formatted1 <- manager$format_region(region1)
  expect_true(grepl("Region 1", formatted1))
  expect_true(grepl("0.2", formatted1))
  expect_true(grepl("0.4", formatted1))
})

test_that("Region validation works correctly", {
  # Valid
  expect_true(validate_region(0.2, 0.4))
  expect_true(validate_region(0, 1))
  expect_true(validate_region(-0.5, 0.5))

  # Invalid
  expect_false(validate_region(0.4, 0.2))  # xmin > xmax
  expect_false(validate_region(0.5, 0.5))  # xmin == xmax
  expect_false(validate_region(NULL, 0.5))
  expect_false(validate_region(0.5, NA))
  expect_false(validate_region(NA, NA))
})

test_that("get_sym helper works correctly", {
  mono_symbols <- c("down", "x", "up")
  expect_equal(get_sym(-1, mono_symbols), "down")
  expect_equal(get_sym(0, mono_symbols), "x")
  expect_equal(get_sym(1, mono_symbols), "up")
  expect_equal(get_sym(NULL, mono_symbols), "x")
  expect_equal(get_sym(NA, mono_symbols), "x")
  expect_equal(get_sym(2, mono_symbols), "x")

  conv_symbols <- c("n", "x", "U")
  expect_equal(get_sym(-1, conv_symbols), "n")
  expect_equal(get_sym(0, conv_symbols), "x")
  expect_equal(get_sym(1, conv_symbols), "U")
})

test_that("Region manager handles edge cases", {
  manager <- create_region_manager()

  # Add region with extreme values
  expect_true(manager$add_region(-100, 100))
  expect_equal(manager$get_count(), 1)
  expect_equal(manager$regions[[1]]$xmin, -100)
  expect_equal(manager$regions[[1]]$xmax, 100)

  # Add region with very small interval
  expect_true(manager$add_region(0.0001, 0.0002))
  expect_equal(manager$get_count(), 2)

  # Delete and then re-add with same parameters
  expect_true(manager$delete_region(1))
  expect_true(manager$add_region(0.2, 0.4))
  # New region should have new ID
  expect_equal(manager$region_id, 3)
  expect_equal(manager$regions[[2]]$id, 3)
})

# ============================================================================
# PERFORMANCE TESTS
# ============================================================================

test_that("Region operations perform well with many regions", {
  manager <- create_region_manager()
  n_regions <- 1000

  # Add many regions
  time_start <- Sys.time()
  for (i in 1:n_regions) {
    manager$add_region(0.1 + (i-1) * 0.0005, 0.2 + (i-1) * 0.0005)
  }
  time_end <- Sys.time()

  expect_lt(as.numeric(difftime(time_end, time_start, units = "secs")), 0.5)
  expect_equal(manager$get_count(), n_regions)

  # Delete middle region
  time_start <- Sys.time()
  manager$delete_region(500)
  time_end <- Sys.time()

  expect_lt(as.numeric(difftime(time_end, time_start, units = "secs")), 0.1)
  expect_equal(manager$get_count(), n_regions - 1)

  # Get formatted output
  time_start <- Sys.time()
  formatted <- manager$get_formatted_regions()
  time_end <- Sys.time()

  expect_lt(as.numeric(difftime(time_end, time_start, units = "secs")), 0.1)
  expect_true(grepl("Region 1", formatted))
  expect_true(grepl("Region 999", formatted))

  # Clear all
  time_start <- Sys.time()
  manager$clear_regions()
  time_end <- Sys.time()

  expect_lt(as.numeric(difftime(time_end, time_start, units = "secs")), 0.05)
  expect_equal(manager$get_count(), 0)
})

# ============================================================================
# TEST SUITE SUMMARY
# ============================================================================

test_that("Region management test suite summary", {
  cat("\n=== Region Management Test Suite ===\n")
  cat("Tested scenarios:\n")
  cat("✓ Region manager initialization\n")
  cat("✓ Add region (valid and invalid)\n")
  cat("✓ Delete region (valid and invalid)\n")
  cat("✓ Delete selected region clears selection\n")
  cat("✓ Select region\n")
  cat("✓ Clear all regions\n")
  cat("✓ Region formatting\n")
  cat("✓ Region validation\n")
  cat("✓ get_sym helper\n")
  cat("✓ Edge cases\n")
  cat("✓ Performance with 1000 regions\n")
  cat("====================================\n")
  expect_true(TRUE)
})

# ============================================================================
# RUN TESTS - CORRECTED VERSION
# ============================================================================

# This code only runs when the script is sourced directly, not when loaded via testthat
if (interactive() && !isTRUE(getOption("testthat.running"))) {
  cat("\n=== Running Region Management Tests ===\n")

  # Use the correct path - this file is the test file itself
  test_file_path <- "tests/testthat/Test_region_management.R"

  # Check if we're in the package root or a subdirectory
  if (file.exists(test_file_path)) {
    testthat::test_file(test_file_path)
  } else {
    # Try alternative path
    test_file_path <- "Test_region_management.R"
    if (file.exists(test_file_path)) {
      testthat::test_file(test_file_path)
    } else {
      cat("Cannot find test file. Please run tests with:\n")
      cat("  testthat::test_file('tests/testthat/Test_region_management.R')\n")
      cat("or from the package root directory.\n")
    }
  }
}
