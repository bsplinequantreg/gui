

# ============================================================================
# INTEGRATION WITH CI/CD
# ============================================================================

# For continuous integration, use:
# - GitHub Actions with R CMD check
# - Use shinyTest for Shiny-specific testing
# - Set up headless Chrome for shinytest2

# Example GitHub Actions workflow:
#
# name: R-CMD-check
# on: [push, pull_request]
# jobs:
#   test:
#     runs-on: ubuntu-latest
#     steps:
#       - uses: actions/checkout@v4
#       - uses: r-lib/actions/setup-r@v2
#       - uses: r-lib/actions/setup-pandoc@v2
#       - name: Install dependencies
#         run: |
#           install.packages(c("testthat", "shinytest2", "shiny", "DT", "plotly"))
#       - name: Run tests
#         run: |
#           devtools::test()
