#' Launch the 'BsplineQuantReg' Shiny Interface
#' Opens an interactive Shiny application for quantile regression
#' using B-splines with shape constraints.
#' @return Launches a Shiny application in the default browser.
#' @export
#' @import BsplineQuantReg
#' @importFrom shiny shinyApp runApp fluidPage sidebarLayout mainPanel
#' @importFrom shinyjs useShinyjs
#' @importFrom plotly plotlyOutput renderPlotly plot_ly
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom shinythemes shinytheme
#' @importFrom colourpicker colourInput
#' @param rstudio Boolean if TRUE launches the shiny browser of rstudio.
#' @param brow Boolean if TRUE launches the default browser in the system.
#' @param host address of the server. Default is '127.0.0.1'
#' @param port port to reach the app on the server default is 3674
#' @examples
#' if (interactive()) {
#'   run_gui()
#' }
#' @export

run_gui <- function(brow = TRUE, rstudio = FALSE, host = '127.0.0.1', port = 3674) {

app_dir <- "./inst/shiny/"


if (!brow && !rstudio) {
  shiny::runApp(app_dir, launch.browser = FALSE, host = host, port = port)
} else if (brow) {
  shiny::runApp(app_dir, launch.browser = TRUE, host = host, port = port)
} else {
  shiny::runApp(app_dir)
}
}
