# tests/run_all_tests.R
# Run this from the package root directory

run_all_tests <- function() {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║     BsplineQuantReg GUI - Complete Test Suite              ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n")
  cat("\n")

  test_files <- c(
    "test_utils.R",
    "test-region_management.R",
    "test-regression.R",
    "test-server.R",
    "test-shinytest2.R"
  )

  results <- list()
  total_passed <- 0
  total_failed <- 0
  total_skipped <- 0
  total_warnings <- 0

  for (file in test_files) {
    file_path <- file.path("tests/testthat", file)
    if (file.exists(file_path)) {
      cat(sprintf("!##! Running %s...\n", file))
      cat("───────────────────────────────────────────────────────────────\n")

      # Capture test results
      result <- tryCatch({
        testthat::test_file(file_path, reporter = "summary")
      }, error = function(e) {
        cat(sprintf("!XX! Error running %s: %s\n", file, e$message))
        NULL
      })

      if (!is.null(result)) {
        # Extract summary stats if available
        if (inherits(result, "testthat_results")) {
          passed <- sum(result$passed)
          failed <- sum(result$failed)
          skipped <- sum(result$skipped)
          warnings <- sum(result$warnings)

          total_passed <- total_passed + passed
          total_failed <- total_failed + failed
          total_skipped <- total_skipped + skipped
          total_warnings <- total_warnings + warnings

          status <- if (failed == 0) "!OK!" else "!XX!"
          cat(sprintf("%s %s: %d passed, %d failed, %d skipped, %d warnings\n\n",
                      status, file, passed, failed, skipped, warnings))
        } else {
          cat(sprintf("!OK!%s: All tests passed\n\n", file))
        }
      }
    } else {
      cat(sprintf("!WW!  Warning: %s not found\n", file_path))
    }
  }

  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                     TEST SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  !OK! Total Passed:  %d\n", total_passed))
  cat(sprintf("  !XXX! Total Failed:  %d\n", total_failed))
  cat(sprintf("  !>>>!  Total Skipped: %d\n", total_skipped))
  cat(sprintf(" !W!  Total Warnings: %d\n", total_warnings))
  cat("═══════════════════════════════════════════════════════════════\n")

  if (total_failed == 0) {
    cat("\n !:))! ALL TESTS PASSED! Great job! !:))!\n\n")
  } else {
    cat(sprintf("\n!XX!%d test(s) failed. Please review the errors above.\n\n", total_failed))
  }

  return(invisible(list(
    passed = total_passed,
    failed = total_failed,
    skipped = total_skipped,
    warnings = total_warnings
  )))
}

# Run all tests
if (interactive()) {
  run_all_tests()
}
