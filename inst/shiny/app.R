# BsplineQuantReg Shiny Interface
# Author: Alexandre Abbes
# version 0.1.1
# Stable versionwith graphical region selection
#
# Run with:
# shiny::runApp("R/run_gui.R")


library(BsplineQuantReg)


library(shiny)
library(shinyjs)
library(shinythemes)
library(ECOSolveR)
library(DT)
library(plotly)
library(colourpicker)
library(png)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  tags$style(
    HTML(
      "
    .shiny-notification-success {
      background-color: #d4edda;
      border-color: #c3e6cb;
      color: #155724;
    }
    .region-box {
      border: 2px solid #ff9800;
      background-color: rgba(255, 152, 0, 0.15);
      border-radius: 5px;
      padding: 8px;
      margin: 4px 0;
    }
  "
    )
  ),

  titlePanel(
    h2(
      "BsplineQuantReg - Quantile Regression with B-Splines under Shape Constraints",
      align = "center",
      style = "color: #2c3e50;"
    ),
    windowTitle = "BsplineQuantRegGui"
  ),
  #themeSelector(),
  div(
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    actionButton("toggle_theme", "Themes", class = "btn-sm btn-outline-secondary", style = "border-radius: 20px; padding: 5px 15px;")
  ),

  # Et la zone pour le themeSelector (cachée par défaut)
  div(
    id = "theme_selector_area",
    style = "display: none; position: absolute; top: 50px; right: 20px; z-index: 1000;
           background: white; padding: 15px; border-radius: 8px;
           box-shadow: 0 4px 12px rgba(0,0,0,0.15); width: 250px;",
    h5("Select Theme:"),
    themeSelector()
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "background-color: #f8f9fa; border-radius: 5px;",

      # ============ 1. DATA ============
      h3("1. Data", class = "text-primary"),

      div(
        style = "display: flex; flex-wrap: wrap; gap: 5px;",
        actionButton("test_data", "Test", class = "btn-sm btn-success", style = " background-color:#000000;"),
        actionButton("temp_data", "Temp", class = "btn-sm btn-warning", style = "background-color:#FF0000;"),
        actionButton("load_csv", "CSV", class = "btn-sm btn-info", style = "background-color:#10AA10;")
      ),
      br(),

      h5("Interval:"),
      h6(fluidRow(
        column(
          4,
          numericInput("data_xmin", "X min:", value = 0, step = 0.05),
          style = "padding-right: 1px;"
        ),
        column(
          4,
          numericInput("data_xmax", "X max:", value = 1, step = 0.05),
          style = "padding-right: 1px;"
        ),
        column(
          4,
          numericInput(
            "n_points",
            "n:",
            value = 100,
            min = 10,
            max = 1000
          ),
          style = "padding-right: 1px;"
        )
      )),

      p("Custom function:"),
      fluidRow(column(
        12,
        textInput("custom_func", NULL, value = "2*x + 0.5*sin(6*pi*x) + 0.2*rnorm(n)")
      ) ),

      actionButton("generate_custom", "Generate", class = "btn-sm btn-primary"),

      hr(),

      # ============ 2. SPLINE ============
      h3("2. Spline", class = "text-primary"),

      fluidRow(column(
        6, numericInput(
          "degree",
          "Degree:",
          value = 3,
          min = 1,
          max = 4
        )
      ), column(
        6,
        numericInput(
          "knots_count",
          "Auto knots:",
          value = 10,
          min = 2,
          max = 30
        )
      )),

      fluidRow(column(
        4,
        actionButton(
          "add_knot_mode",
          "Add knot",
          class = "btn-sm btn-primary",
          style = "background-color:#FFDD00; color:#000000"
        )
      ), column(
        4,
        actionButton("clear_knots", "Clear knots", class = "btn-sm btn-danger")
      )),
      br(),

      fluidRow(sliderInput(
        "tau",
        "Tau:",
        min = 0.05,
        max = 0.95,
        value = 0.5
      )),
      br(),
      fluidRow(h6(column(6, selectInput("solver","Solver:",
          choices = c("ECOS", "SCS","CLARABEL", "HIGHS", "OSQP", "GUROBI") ) ),
        column(6,selectInput("type_reg","Type of Regr.",
                      choices=c('quantile','mean_square') ) ))),
      fluidRow(h6(column(
        6, checkboxInput("verbose", "Verbose", FALSE)
        )
      )),

      hr(),

      #  5. Demos" :

      h3("5. Demos", class = "text-primary"),
      div(
        style = "display: flex; flex-wrap: wrap; gap: 5px;",
        actionButton(
          "demo_comp",
          "Comprehensive",
          class = "btn-sm btn-info",
          style = "flex:1;"
        ),
        actionButton(
          "demo_monot",
          "Monotonicity Basic",
          class = "btn-sm btn-info",
          style = "flex:1;"
        ),
        actionButton("demo_log", "Logistic", class = "btn-sm btn-info", style = "flex:1;"),
        actionButton(
          "demo_temp",
          "Temperature",
          class = "btn-sm btn-info",
          style = "flex:1;"
        ),
        actionButton(
          "demo_temp2",
          "Temperature2",
          class = "btn-sm btn-info",
          style = "flex:1;"
        ),
        actionButton("demo_conv", "Convexity", class = "btn-sm btn-info", style = "flex:1;"),
        actionButton("demo_degrees", "Degrees", class = "btn-sm btn-info", style = "flex:1;"),
        actionButton(
          "demo_der3",
          "Third derivative",
          class = "btn-sm btn-info",
          style = "flex:1;"
        ),
        actionButton(
          "demo_derivative",
          "Derivative",
          class = "btn-sm btn-info",
          style = "flex:1;"
        )
      ),



      div(
        style = "font-size: 11px; color: #666; text-align: center;",
        p("BsplineQuantRegGui v0.1.0"),
        p("BsplineQuantReg 0.2.2"),
        p("GPL3 (c) Abbes, 2026"),
        a("GitHub", href = "https://github.com/alexandreabbes/BsplineQuantReg", target = "_blank")
      )
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        tabPanel(
          "Visualization",
          br(),
          fluidRow(
            column(9, plotlyOutput("spline_plot", height = "500px")),

            column(
              3,
              h3("3. Constraints", class = "text-primary"),
              radioButtons(
                "constraint_mode",
                "Mode:",
                choices = c("Uniform" = "uniform", "Per region" = "region"),
                selected = "uniform",
                inline = TRUE
              ),

              conditionalPanel(
                condition = "input.constraint_mode == 'uniform'",
                radioButtons(
                  "monot",
                  "Monotonicity:",
                  choices = c("x" = "0", "up" = "1", "down" = "-1"),
                  selected = "0",
                  inline = TRUE
                ),
                radioButtons(
                  "conv",
                  "Convexity:",
                  choices = c("x" = "0", "U" = "1", "n" = "-1"),
                  selected = "0",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.degree >= 3",
                  radioButtons(
                    "der3",
                    "Third Derivative:",
                    choices = c("x" = "0", "+" = "1", "-" = "-1"),
                    selected = "0",
                    inline = TRUE
                  )
                )
              ),

              conditionalPanel(
                condition = "input.constraint_mode == 'region'",
                div(style = "font-size: 13px; color: #555; margin-bottom: 10px;", "1. Click 'Select'"),
                div(style = "font-size: 13px; color: #555; margin-bottom: 10px;", "2. Select a region on the plot"),
                div(style = "font-size: 13px; color: #555; margin-bottom: 10px;", "3. X min/max fields are updated"),

                fluidRow(column(
                  6,
                  actionButton(
                    "start_selection",
                    "Select",
                    class = "btn-sm btn-warning",
                    style = "width:100%;"
                  )
                ), column(
                  6,
                  actionButton(
                    "clear_regions",
                    "Cancel region",
                    class = "btn-sm btn-danger",
                    style = "width:100%;"
                  )
                )),
                br(),

                fluidRow(column(
                  6, numericInput("region_xmin", "X min:", value = 0.3, step = 0.05)
                ), column(
                  6, numericInput("region_xmax", "X max:", value = 0.6, step = 0.05)
                )),

                radioButtons(
                  "region_monot",
                  "Monotonicity:",
                  choices = c("x" = "0", "up" = "1", "down" = "-1"),
                  selected = "0",
                  inline = TRUE
                ),
                radioButtons(
                  "region_conv",
                  "Convexity:",
                  choices = c("x" = "0", "U" = "1", "n" = "-1"),
                  selected = "0",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.degree >= 3",
                  radioButtons(
                    "region_der3",
                    "Third Derivative:",
                    choices = c("x" = "0", "+" = "1", "-" = "-1"),
                    selected = "0",
                    inline = TRUE
                  )
                ),


                fluidRow(column(
                  6,
                  actionButton("add_region", "Add region", class = "btn-sm btn-primary", style = "width:100%;")
                ), column(
                  6,
                  actionButton("update_region", "Update", class = "btn-sm btn-info", style = "width:100%;")
                )),
                br(),
                div(id = "regions_list", style = "max-height: 120px; overflow-y: auto;")
              ),

              # ============ 4. EXECUTION ============

              h3("4. Execution"),
              h5("Color:"),
              fluidRow(
                column(
                  6,
                  colourpicker::colourInput("curve_color", NULL, value = "blue")
                ),
                column(6, actionButton("apply_color", "Apply", class = "btn-sm"))
              ),

              p("Curves:", textOutput("curve_count", inline = TRUE)),
              fluidRow(
              actionButton("run", "Run", class = "btn-success btn-lg"),
              actionButton("clear_curves", "Clear last curve", class = "btn-sm btn-warning")),
              br(),
              actionButton("clear_all", "Clear all", class = "btn-sm btn-danger")


            )
          ),

          br(),


          ##========== INFORMATIONS ============
          fluidRow(
            # Information générale
            column(
              6,
              h5("Information"),
              verbatimTextOutput("fit_info")
            ),

            # Noeuds et coefficients
            column(
              6,
              h5("List of knots"),
              verbatimTextOutput("knots_compact", placeholder = TRUE),
              h5("Coefficients on the Bspline Basis"),
              verbatimTextOutput("coeff_list", placeholder = TRUE)
            )
          ),
        ),  # fin tabPane



        tabPanel(
          "Data",
          br(),
          fluidRow(
            column(6, h4("Summary"), verbatimTextOutput("data_summary"))
            #,
            #column(6, h4("Knots"), verbatimTextOutput("knots_info"))
          ),
          br(),
          DTOutput("data_table")
        ),
        tabPanel(
          "Regions",
          br(),
          h4("Defined Regions"),
          verbatimTextOutput("regions_info"),
          br(),
          fluidRow(column(
            6, h5("Active regions"), uiOutput("regions_list_ui")
          ), column(
            6,
            h5("Instructions"),
            p("1. Mode 'Per region'"),
            p("2. 'Select' a rectangle on the plot"),
            p("3. Select constraints"),
            p("4. 'Add region'")
          ))
        ),
        tabPanel(
          "R Code",
          br(),
          h4("R Code to reproduce the analysis:"),
          verbatimTextOutput("r_code")
        ),
        tabPanel("Console", br(), fluidRow(column(
          12,
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            h4("Console Output", style = "margin: 0;"),
            actionButton(
              "clear_console",
              "Clear Console",
              icon = icon("eraser"),
              class = "btn-sm btn-danger"
            )
          ),
          div(
            style = "background-color: #1e1e1e; color: #d4d4d4; padding: 15px;
                 border-radius: 5px; font-family: 'Courier New', monospace;
                 font-size: 13px; height: 400px; overflow-y: auto;
                 white-space: pre-wrap; word-wrap: break-word;",
            verbatimTextOutput("console_output")
          )
        ))),
        tabPanel('Demo',br(),
                 div(
                   id = "demo_area",
                   style = "display: none; margin-top: 10px;",
                   hr(),
                   h4("Demo Results:"),
                   div(
                     style = "overflow: auto; width: 100%; max-height: 1800px;",
                     plotOutput("demo_plot", height = 800)  # Hauteur par défaut, sera modifiée dynamiquement
                   ),
                   br(),
                   verbatimTextOutput("demo_output")
                 )
                 )
      )
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  #Theme selector
  # Dans le server, ajoutez :
  observeEvent(input$toggle_theme, {
    # Basculer l'affichage de la zone de thème
    toggle("theme_selector_area", anim = TRUE)
  })

  # ============ REACTIVE VALUES ============
  values <- reactiveValues(
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
    selecting_region = FALSE,
    deriv= list(),
    version= packageVersion("BsplineQuantReg")
  )

  # ============ CONSTRAINT SYMBOL FUNCTION ============
  get_sym <- function(val, symbols) {
    if (is.null(val) || is.na(val))
      return("x")
    val <- as.numeric(val)
    if (!val %in% c(-1, 0, 1))
      return("x")
    return(symbols[val + 2])
  }

  # ============ FIELD UPDATE FUNCTION ============
  update_region_fields <- function(xmin, xmax) {
    if (is.null(xmin) ||
        is.null(xmax) || is.na(xmin) || is.na(xmax))
      return()
    if (xmin >= xmax) {
      showNotification("X min must be less than X max", type = "warning")
      return()
    }
    updateNumericInput(session, "region_xmin", value = round(xmin, 3))
    updateNumericInput(session, "region_xmax", value = round(xmax, 3))
  }

  # ============ CONSOLE ============

  console_messages <- reactiveVal("")




  log_console <- function(msg) {
    current <- console_messages()
    console_messages(paste0(current, msg, "\n"))
    cat(msg, "\n")  # Pour voir dans la console R aussi
  }

  # Observer pour vider la console
  observeEvent(input$clear_console, {
    console_messages("")
  })

  # Afficher la console
  output$console_output <- renderText({
    console_messages()
  })

  # ============ DATA GENERATION ============

  observeEvent(input$test_data, {
    withProgress(message = "Generating...", {
      set.seed(42)
      n <- 200
      xmin <- input$data_xmin
      xmax <- input$data_xmax
      x <- as.vector(seq(xmin, xmax, length.out = n))
      y <- as.vector(2 * x + 0.2 * sin(10 * pi * x) + 0.05 * rnorm(n))
      values$xtab <- x
      values$ytab <- y
      values$data_name <- paste("Test [", xmin, ",", xmax, "]")
      values$fitted <- NULL
      values$curve_lines <- list()
      values$regions <- list()
      showNotification("Test data generated", type = "message")
    })
  })

  observeEvent(input$temp_data, {
    withProgress(message = "Loading...", {
      temp_data <- c(
        -0.32,
        -0.32,
        -0.40,
        -0.39,
        -0.65,
        -0.43,
        -0.40,
        -0.52,
        -0.30,
        -0.12,-0.40,
        -0.42,
        -0.39,
        -0.45,
        -0.35,
        -0.36,
        -0.19,
        -0.14,
        -0.37,
        -0.22,
        0.00,
        -0.08,
        -0.24,
        -0.36,
        -0.49,
        -0.27,
        -0.19,
        -0.43,
        -0.29,
        -0.30,-0.29,
        -0.29,
        -0.28,
        -0.23,
        -0.04,
        -0.02,
        -0.24,
        -0.42,
        -0.35,
        -0.16,-0.17,
        -0.09,
        -0.13,
        -0.16,
        -0.14,
        -0.14,
        0.10,
        -0.03,
        0.03,
        -0.18,-0.06,
        0.04,
        0.02,
        -0.13,
        0.03,
        -0.06,
        0.02,
        0.13,
        0.13,
        -0.03,
        0.15,
        0.12,
        0.10,
        0.04,
        0.11,
        -0.04,
        0.01,
        0.13,
        -0.01,
        -0.06,-0.14,
        -0.02,
        0.04,
        0.14,
        -0.07,
        -0.06,
        -0.17,
        0.10,
        0.10,
        0.05,-0.01,
        0.08,
        0.02,
        0.02,
        -0.26,
        -0.16,
        -0.09,
        -0.02,
        -0.12,
        0.03,
        0.04,
        -0.11,
        -0.07,
        0.19,
        -0.07,
        -0.05,
        -0.22,
        0.16,
        0.09,
        0.14,
        0.28,
        0.39,
        0.07,
        0.29,
        0.11,
        0.11,
        0.16,
        0.32,
        0.35,
        0.25,
        0.47,
        0.41,
        0.13
      )
      years <- 1880:1992
      x <- (years - 1880) / (1992 - 1880)
      y <- temp_data
      values$xtab <- x
      values$ytab <- y
      values$data_name <- "Temperature (1880-1992)"
      values$fitted <- NULL
      values$curve_lines <- list()
      values$regions <- list()
      year_knots <- c(1880, 1889, 1900, 1910, 1930, 1940, 1965, 1992)
      knot <- (year_knots - 1880) / (1992 - 1880)
      values$knot <- knot
      showNotification("Temperature data loaded", type = "message")
    })
  })

  observeEvent(input$generate_custom, {
    tryCatch({
      n <- input$n_points
      xmin <- input$data_xmin
      xmax <- input$data_xmax
      x <- as.vector(seq(xmin, xmax, length.out = n))
      func_str <- gsub("sin\\(", "sin(", input$custom_func)
      func_str <- gsub("cos\\(", "cos(", func_str)
      func_str <- gsub("pi", "pi", func_str)
      func_str <- gsub("randn\\(", "rnorm(", func_str)
      y <- eval(parse(text = func_str))
      values$xtab <- x
      values$ytab <- y
      values$data_name <- "Custom function"
      values$fitted <- NULL
      values$curve_lines <- list()
      values$regions <- list()
      showNotification("Data generated", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # ============ KNOTS ============

  observe({
    if (!is.null(values$xtab) && length(values$manual_knots) == 0)
    {
      if (!is.na(input$knots_count))
      {
        kn <- max((input$knots_count), 2) - 1

        #if (is.na(knots_count)){knots_count=2}
        values$knot <- as.numeric(quantile(values$xtab, probs = ((0:(kn)) / (kn))))
      }
      else{
        kn = 1
        values$knot = c(0, 1)
      }
    }
  })

  output$knots_compact <- renderPrint({
    if (!is.null(values$knot) && length(values$knot) > 0) {
      k <- round(values$knot, 3)
      if (length(k) <= 8) {
        cat(paste(k, collapse = ", "))
      } else {
        cat(paste(c(head(k, 4), "...", tail(k, 4)), collapse = ", "))
      }
    } else {
      cat("(none)")
    }
  })
  output$knots_compact <- renderPrint({
    if (!is.null(values$knot) && length(values$knot) > 0) {
      k <- round(values$knot, 3)
      if (length(k) <= 8) {
        cat(paste(k, collapse = ", "))
      } else {
        cat(paste(c(head(k, 4), "...", tail(k, 4)), collapse = ", "))
      }
    } else {
      cat("(none)")
    }
  })
  observeEvent(input$add_knot_mode, {
    values$adding_knot <- !values$adding_knot
    if (values$adding_knot) {
      showNotification("Add knot mode: click on the plot", type = "message")
      updateActionButton(session, "add_knot_mode", label = "Stop")
    } else {
      updateActionButton(session, "add_knot_mode", label = "Add knot")
    }
  })

  observeEvent(event_data("plotly_click", source = "plot"), {
    if (values$adding_knot) {
      click <- event_data("plotly_click", source = "plot")
      if (!is.null(click) && !is.null(values$xtab)) {
        x <- click$x
        if (x > min(values$xtab) && x < max(values$xtab)) {
          if (!any(abs(values$knot - x) < 1e-6)) {
            values$manual_knots <- c(values$manual_knots, x)
            values$knot <- sort(c(values$knot, x))
            showNotification(paste("Knot added at x =", round(x, 3)), type = "message")
          } else {
            showNotification("This knot already exists", type = "warning")
          }
        } else {
          showNotification("Knot must be inside the interval", type = "warning")
        }
      }
    }
  })

  observeEvent(input$clear_knots, {
    values$manual_knots <- list()
    if (!is.null(values$xtab)) {
      kn <- max(input$knots_count, 2) - 1
      values$knot <- as.numeric(quantile(values$xtab, probs = (0:(kn)) / (kn)))
    }
    showNotification("knot reset", type = "message")
  })

  # ============ SELECTION MANAGEMENT ============

  observeEvent(input$start_selection, {
    values$selecting_region <- !values$selecting_region
    if (values$selecting_region) {
      showNotification("Select a region on the plot (rectangle)", type = "message")
      updateActionButton(session, "start_selection", label = "Stop")
    } else {
      updateActionButton(session, "start_selection", label = "Select")
    }
  })

  observeEvent(event_data("plotly_selected", source = "plot"), {
    if (input$constraint_mode == "region" && values$selecting_region) {
      selected <- event_data("plotly_selected", source = "plot")
      if (!is.null(selected) && nrow(selected) > 0) {
        x_vals <- selected$x
        if (length(x_vals) >= 2) {
          xmin <- min(x_vals, na.rm = TRUE)
          xmax <- max(x_vals, na.rm = TRUE)
          updateNumericInput(session, "region_xmin", value = round(xmin, 3))
          updateNumericInput(session, "region_xmax", value = round(xmax, 3))
          values$selecting_region <- FALSE
          updateActionButton(session, "start_selection", label = "Select")
          showNotification(paste(
            "Region selected: [",
            round(xmin, 3),
            ", ",
            round(xmax, 3),
            "]"
          ),
          type = "message")
        }
      }
    }
  })

  # ============ REGIONS ============

  observeEvent(input$add_region, {
    req(values$xtab, values$knot)
    xmin <- input$region_xmin
    xmax <- input$region_xmax
    if (xmin >= xmax) {
      showNotification("X min < X max", type = "warning")
      return()
    }
    values$region_id <- values$region_id + 1
    region <- list(
      id = values$region_id,
      xmin = xmin,
      xmax = xmax,
      monot = as.numeric(input$region_monot),
      conv = as.numeric(input$region_conv),
      der3 = as.numeric(input$region_der3)
    )
    values$regions <- c(values$regions, list(region))
    values$selected_region_id <- NULL
    showNotification(paste("Region added: [", round(xmin, 3), ", ", round(xmax, 3), "]"), type = "message")
  })

  observeEvent(input$update_region, {
    if (!is.null(values$selected_region_id)) {
      idx <- which(sapply(values$regions, function(r)
        r$id == values$selected_region_id))
      if (length(idx) > 0) {
        values$regions[[idx]]$xmin <- input$region_xmin
        values$regions[[idx]]$xmax <- input$region_xmax
        values$regions[[idx]]$monot <- as.numeric(input$region_monot)
        values$regions[[idx]]$conv <- as.numeric(input$region_conv)
        values$regions[[idx]]$der3 <- as.numeric(input$region_der3)
        showNotification("Region updated", type = "message")
      }
    } else {
      showNotification("Select a region first", type = "warning")
    }
  })

  observeEvent(input$clear_regions, {
    values$regions <- list()
    values$region_id <- 0
    values$selected_region_id <- NULL
    showNotification("Regions cleared", type = "message")
  })

  observeEvent(input$delete_region, {
    id <- as.numeric(input$delete_region)
    if (is.na(id)) {
      showNotification("Invalid ID", type = "warning")
      return()
    }
    values$regions <- values$regions[!sapply(values$regions, function(r)
      r$id == id)]
    if (!is.null(values$selected_region_id) &&
        values$selected_region_id == id) {
      values$selected_region_id <- NULL
    }
    showNotification(paste("Region", id, "deleted"), type = "message")
  }, ignoreNULL = TRUE)

  # ============ CONSTRAINT CONSTRUCTION ============

  build_constraints <- function() {
    degree <- input$degree
    kn <- length(values$knot) - 1

    if (kn < 1) {
      showNotification("Not enough knots!", type = "warning")
      return(NULL)
    }

    safe_repeat <- function(val, len) {
      if (length(val) == 1) {
        return(rep(as.numeric(val), len))
      }
      v <- as.numeric(val)
      if (length(v) > len)
        return(v[1:len])
      if (length(v) < len)
        return(c(v, rep(0, len - length(v))))
      return(v)
    }

    if (input$constraint_mode == "uniform") {
      monot <- input$monot
      conv <- input$conv
      der3 <- input$der3
    } else {
      monot <- rep(0, kn + 1) #each time give the maximum of knots
      conv <- rep(0, kn + 1) # so that for all degrees
      der3 <- rep(0, kn + 1)  # the number of constraints is satisfied
      for (region in values$regions) {
        for (i in 1:kn) {
          x1 <- values$knot[i]
          x2 <- values$knot[i + 1]
          if (x2 > region$xmin && x1 < region$xmax) {
            if (region$monot != 0)
              monot[i] <- region$monot
            if (region$conv != 0) {
              conv[i] <- region$conv
              conv[i + 1] <- region$conv
            }
            if (region$der3 != 0 && degree >= 3) {
              der3[i] <- region$der3
            }
          }
        }
      }
    }
    # Dans build_constraints(), pour les contraintes uniformes
    if (input$constraint_mode == "uniform") {
      # S'assurer que les valeurs sont numériques
      monot_val <- as.numeric(input$monot)
      conv_val <- as.numeric(input$conv)
      der3_val <- as.numeric(input$der3)

      # Remplacer NA par 0
      if (is.na(monot_val))
        monot_val <- 0
      if (is.na(conv_val))
        conv_val <- 0
      if (is.na(der3_val))
        der3_val <- 0

      monot <- rep(monot_val, kn + 1)
      conv <- rep(conv_val, kn + 1) # débordre pour le degre 2
      der3 <- rep(der3_val, kn + 1) # débordre pour le degre 3
    }
    if (degree < 3)
      der3 <- rep(0, kn + 1)

    list(monot = monot,
         conv = conv,
         der3 = der3)
  }

  # ============ CSV IMPORT ============

  observeEvent(input$load_csv, {
    file_path <- file.choose()
    if (is.na(file_path))
      return()

    tryCatch({
      df <- read.csv(file_path, header = TRUE)

      if (ncol(df) < 2) {
        showNotification("File must have at least 2 columns!", type = "error")
        return()
      }

      x_col <- df[, 1]
      y_col <- df[, 2]

      valid <- !is.na(x_col) & !is.na(y_col)
      x_col <- x_col[valid]
      y_col <- y_col[valid]

      if (length(x_col) < 3) {
        showNotification("Not enough data (minimum 3 points)", type = "error")
        return()
      }

      values$xtab <- as.vector(x_col)
      values$ytab <- as.vector(y_col)
      values$data_name <- basename(file_path)
      values$fitted <- NULL
      values$curve_lines <- list()
      values$regions <- list()

      updateNumericInput(session, "data_xmin", value = min(values$xtab))
      updateNumericInput(session, "data_xmax", value = max(values$xtab))

      values$manual_knots <- list()
      kn <- max(input$knots_count, 2) - 1
      values$knot <- as.numeric(quantile(values$xtab, probs = (0:(kn)) / (kn)))

      showNotification(paste(
        "File loaded:",
        basename(file_path),
        "-",
        length(x_col),
        "points"
      ),
      type = "success")

    }, error = function(e) {
      showNotification(paste("Read error:", e$message), type = "error")
    })
  })

  # # ============ EXCEL IMPORT ============
  #
  # observeEvent(input$load_excel, {
  #   if (!requireNamespace("readxl", quietly = TRUE)) {
  #     showNotification(
  #       "Install 'readxl' to read Excel files: install.packages('readxl')",
  #       type = "error",
  #       duration = 10
  #     )
  #     return()
  #   }
  #
  #   file_path <- file.choose()
  #   if (is.na(file_path))
  #     return()
  #   tryCatch({
  #     df <- readxl::read_excel(file_path)
  #     df <- as.data.frame(df)
  #
  #     if (ncol(df) < 2) {
  #       showNotification("File must have at least 2 columns!", type = "error")
  #       return()
  #     }
  #
  #     x_col <- df[, 1]
  #     y_col <- df[, 2]
  #
  #     valid <- !is.na(x_col) & !is.na(y_col)
  #     x_col <- x_col[valid]
  #     y_col <- y_col[valid]
  #
  #     if (length(x_col) < 3) {
  #       showNotification("Not enough data (minimum 3 points)", type = "error")
  #       return()
  #     }
  #
  #     values$xtab <- as.vector(x_col)
  #     values$ytab <- as.vector(y_col)
  #     values$data_name <- basename(file_path)
  #     values$fitted <- NULL
  #     values$curve_lines <- list()
  #     values$regions <- list()
  #
  #     updateNumericInput(session, "data_xmin", value = min(values$xtab))
  #     updateNumericInput(session, "data_xmax", value = max(values$xtab))
  #
  #     values$manual_knots <- list()
  #     kn <- max(input$knot_count, 2) - 1
  #     values$knot <- as.numeric(quantile(values$xtab, probs = (0:(kn)) / (kn)))
  #
  #     showNotification(paste(
  #       "File loaded:",
  #       basename(file_path),
  #       "-",
  #       length(x_col),
  #       "points"
  #     ),
  #     type = "success")
  #
  #   }, error = function(e) {
  #     showNotification(paste("Read error:", e$message), type = "error")
  #   } )} )


  # ============ REGRESSION ============

  observeEvent(input$run, {
    req(values$xtab, values$ytab, values$knot)

    if (length(values$knot) < 2) {
      showNotification("Need at least 2 knots!", type = "error")
      return()
    }

    if (length(values$xtab) != length(values$ytab)) {
      showNotification("x and y have different lengths!", type = "error")
      return()
    }

    withProgress(message = "Regression...", {
      constraints <- build_constraints()
      if (is.null(constraints)) return()

      log_console("=== Starting Regression ===")
      log_console(paste("Degree:", input$degree))
      log_console(paste("Solver:", input$solver))
      log_console(paste("Verbose:", input$verbose))

      # Vérifier la version de BsplineQuantReg
      version <- values$version

      # Paramètres communs
      args <- list(
        as.vector(values$xtab),
        as.vector(values$ytab),
        knot=as.vector(values$knot),
        tau = input$tau,
        degree = as.numeric(input$degree),
        monot = constraints$monot,
        convcons = constraints$conv,
        der3cons = constraints$der3,
        solver = as.logical(input$solver),
        verbose = input$verbose,
        callable = TRUE
      )

      # Ajouter le paramètre type_reg si la version est >= 0.2.3
      if (version <"0.2.3" && input$type_reg=="mean_square"){
      showNotification("BsplineQuantReg Version 0.2.3 \n
      needed for mean square regression support!\n
      Falling back to quantile regression.", type = "warning")
      }

      if (version >= "0.2.3") {
        args$type_reg <- input$type_reg
      }


      console_text <- character()

      # Rediriger stdout
      sink(tempfile())

      # Capturer les messages
      withCallingHandlers({
        output <- capture.output({
          fitted <- do.call(quantile_spline, args)
        })
      }, message = function(m) {
        console_text <<- c(console_text, m$message)
      })

      # Restaurer stdout
      sink()

      # Ajouter la sortie standard
      console_all <- output
      version >= "0.2.3"

    log_console(console_all)


      # Traiter le résultat
      if (!is.null(fitted)) {
        x_eval <- seq(min(values$xtab), max(values$xtab), length.out = 300)
        y_eval <- fitted(x_eval)
        values$fitted <- fitted
        values$x_eval <- x_eval
        values$y_eval <- y_eval
        color <- input$curve_color
        values$curve_lines <- c(list(list(
          x = x_eval,
          y = y_eval,
          color = color
        )),values$curve_lines )
        showNotification("Regression successful!", type = "message")
      } else {
        log_console("=== Regression failed ===", "error")
      }
    })
      clean_ansi <- function(text) {
        text <- gsub("gG3;", "", text)
        text <- gsub("G3;", "", text)
        text <- trimws(text)
        return(text)
      }

      #log_console(clean_ansi(console_text))

      for (line in console_text) {
        if (nchar(line) > 0) {
          line = clean_ansi(line)
          log_console(paste(line))
        }
      }

      #
      # log_console(paste("BsplineQuantReg version:", packageVersion("BsplineQuantReg")))
      # if (!is.null(fitted)) {
      #   x_eval <- seq(min(values$xtab), max(values$xtab), length.out = 300)
      #   y_eval <- BsplineQuantReg::spline_eval(fitted,x_eval)
      #   values$fitted <- fitted
      #   values$x_eval <- x_eval
      #   values$y_eval <- y_eval
      #   color <- input$curve_color
      #   values$curve_lines <- c(values$curve_lines, list(list(
      #     x = x_eval,
      #     y = y_eval,
      #     color = color
      #   )))
      #   showNotification("Regression successful!", type = "message")
      #   #lapply(console_text, function(line) if(nchar(line)>0) log_console(line))
      #
      #
      # }
    })



  # ============ VISUALIZATION ============

  output$spline_plot <- renderPlotly({
    req(values$xtab)

    p <- plot_ly(source = "plot")

    # Data
    p <- p %>% add_trace(
      x = values$xtab,
      y = values$ytab,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = "gray",
        size = 6,
        opacity = 0.5
      ),
      name = "Data"
    )

    # Knots
    if (!is.null(values$knot)) {
      y_range <- range(values$ytab)
      y_pos <- y_range[2] - 0.1 * diff(y_range)
      p <- p %>% add_trace(
        x = values$knot,
        y = rep(y_pos, length(values$knot)),
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "red",
          symbol = "triangle-down",
          size = 10
        ),
        name = "Knots"
      )
    }

    # Regions
    if (input$constraint_mode == "region" &&
        length(values$regions) > 0) {
      y_range <- range(values$ytab)
      for (region in values$regions) {
        is_selected <- !is.null(values$selected_region_id) &&
          values$selected_region_id == region$id
        border_color <- if (is_selected)
          "#ff0000"
        else
          "rgba(255, 152, 0, 0.8)"
        fill_color <- if (is_selected)
          "rgba(255, 0, 0, 0.15)"
        else
          "rgba(255, 152, 0, 0.15)"
        p <- p %>% add_trace(
          x = c(
            region$xmin,
            region$xmax,
            region$xmax,
            region$xmin,
            region$xmin
          ),
          y = c(y_range[1], y_range[1], y_range[2], y_range[2], y_range[1]),
          type = "scatter",
          mode = "lines",
          fill = "toself",
          fillcolor = fill_color,
          line = list(color = border_color, width = ifelse(is_selected, 3, 1)),
          name = paste0("Region ", region$id),
          hoverinfo = "text",
          text = paste0(
            "Region ",
            region$id,
            "\n",
            "[",
            round(region$xmin, 3),
            ", ",
            round(region$xmax, 3),
            "]\n",
            "M: ",
            get_sym(region$monot, c("down", "x", "up")),
            "\n",
            "C: ",
            get_sym(region$conv, c("n", "x", "U")),
            "\n",
            "D3: ",
            get_sym(region$der3, c("-", "x", "+"))
          )
        )
      }
    }

    # Curves
    for (curve in values$curve_lines) {
      p <- p %>% add_trace(
        x = curve$x,
        y = curve$y,
        type = "scatter",
        mode = "lines",
        line = list(color = curve$color, width = 2),
        name = paste0("tau=", input$tau)
      )
    }

    # Annotation
    p <- p %>% layout(
      annotations = list(
        x = 0.02,
        y = 0.98,
        text = paste("Knots:", length(values$knot)),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 12, color = "red")
      ),
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      hovermode = "closest",
      legend = list(orientation = "h", y = -0.1),
      dragmode = if (values$selecting_region &&
                     input$constraint_mode == "region")
        "select"
      else
        "zoom"
    )

    p <- p %>% config(
      scrollZoom = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("sendDataToCloud", "resetViews")
    )

    p
  })

  # ============ REGION SELECTION BY CLICK ============

  observeEvent(event_data("plotly_click", source = "plot"), {
    if (!values$selecting_region && input$constraint_mode == "region") {
      click <- event_data("plotly_click", source = "plot")
      if (!is.null(click)) {
        x <- click$x
        for (region in values$regions) {
          if (x >= region$xmin && x <= region$xmax) {
            values$selected_region_id <- region$id
            update_region_fields(region$xmin, region$xmax)
            updateRadioButtons(session, "region_monot", selected = as.character(region$monot))
            updateRadioButtons(session, "region_conv", selected = as.character(region$conv))
            updateRadioButtons(session, "region_der3", selected = as.character(region$der3))
            showNotification(paste("Region", region$id, "selected"), type = "message")
            break
          }
        }
      }
    }

  })
  # ============ OUTPUTS ============
  # = INFO = COEFF
  output$coeff_list<- renderPrint( {
    tryCatch({
      if (is.null(values$fitted)) {
        return('No regression')
      } else
      {
        # Extraire les informations
          list_coeff <- (get_parameters(values$fitted))$coeff
          status <- (get_parameters(values$fitted))$result$status
          value <- (get_parameters(values$fitted))$result$value
          cat("Status of convergence: ",status,"\n")
          cat("Value of the loss Function (min): ", round(value,4),"\n" )
          cat("Dimension of the basis: ",
              length(list_coeff %||% numeric(0)), "\n")
          cat("Values: \n")
          cat(list_coeff)
          }},
      error = function(e) {
        cat("Error displaying fit info:", e$message)
      })
  })

    #===INFO==GENERAL
  output$fit_info <- renderPrint(
      {tryCatch({
      cat("Solver: ",input$solver,"\n")
      cat("Degree:", if (!is.na(input$degree))
          input$degree
          else
            "unknown", "   ")
      cat("Tau:", input$tau, "   ")
      cat("Knots:", if (!is.na(input$degree)){length(input$knot)}
            else{"Unknown" }, "\n")
          }

    , error = function(e) {
      cat("Error displaying info:", e$message)}
      )
    constraints <- build_constraints()
    cat("Constraints:\n",
      "Monotone",paste(c(constraints$monot), collapse = ",") ,'\n',
        "Convex",paste(c(constraints$conv),collapse = ",") , "\n",
        "Third derivative",paste( c(constraints$der3),collapse = ",") , "\n"
  )
  } )

  #### bspline coefficients
  output$bspline_coeff<-renderPrint(
    {if (is.null(values$fitted)){return("No regression")}
    else{
      return(
        show_pp(values$fitted,local=input$local,verbose=input$verbose)
        )

    }
      }
    )




  # = Curve cournt =
  output$curve_count <- renderText({
    length(values$curve_lines)
  })

  # regions
  output$regions_list_ui <- renderUI({
    if (length(values$regions) == 0) {
      return(p("No regions", style = "color: #999;"))
    }
    tags$div(lapply(values$regions, function(r) {
      is_selected <- !is.null(values$selected_region_id) &&
        values$selected_region_id == r$id
      tags$div(
        class = "region-box",
        style = if (is_selected)
          "border: 3px solid #ff0000; background-color: rgba(255,0,0,0.1);",
        tags$div(
          style = "display: flex; justify-content: space-between;",
          tags$span(
            style = "font-weight: bold;",
            paste0(
              "Region ",
              r$id,
              " [",
              round(r$xmin, 3),
              ", ",
              round(r$xmax, 3),
              "]"
            )
          ),
          actionButton(
            paste0("del_", r$id),
            "x",
            class = "btn-sm btn-danger",
            style = "padding: 0px 6px;",
            onclick = paste0("Shiny.setInputValue('delete_region', ", r$id, ")")
          )
        ),
        tags$div(
          style = "font-size: 12px; color: #555;",
          paste0(
            "M: ",
            get_sym(r$monot, c("down", "x", "up")),
            " | C: ",
            get_sym(r$conv, c("n", "x", "U")),
            " | D3: ",
            get_sym(r$der3, c("-", "x", "+"))
          )
        )
      )
    }))
  })

  output$data_summary <- renderPrint({
    if (is.null(values$xtab)) {
      cat("No data")
    } else {
      cat("Source:", values$data_name, "\n")
      cat("Points:", length(values$xtab), "\n")
      cat("x: [", min(values$xtab), ",", max(values$xtab), "]\n")
      cat("y: [", min(values$ytab), ",", max(values$ytab), "]")
    }
  })

  output$knots_info <- renderPrint({
    if (is.null(values$knot)) {
      cat("No knots")
    } else {
      cat("Knots:", length(values$knot), "\n")
      print(round(values$knot, 4))
    }
  })

  output$data_table <- renderDT({
    if (is.null(values$xtab))
      return(NULL)
    datatable(data.frame(
      x = round(values$xtab, 4),
      y = round(values$ytab, 4)
    ),
    options = list(pageLength = 10, scrollX = TRUE))
  })

  output$regions_info <- renderPrint({
    if (length(values$regions) == 0) {
      cat("No regions")
    } else {
      for (r in values$regions) {
        cat(
          r$id,
          ": [",
          round(r$xmin, 3),
          ", ",
          round(r$xmax, 3),
          "]  ",
          "M=",
          get_sym(r$monot, c("down", "x", "up")),
          " C=",
          get_sym(r$conv, c("n", "x", "U")),
          " D3=",
          get_sym(r$der3, c("-", "x", "+")),
          "\n",
          sep = ""
        )
      }
    }
  })

  # ============ R CODE ============

  output$r_code <- renderText({
    if (is.null(values$fitted)) {
      return("# Run a regression first")
    }
    constraints <- build_constraints()
    if (is.null(constraints)) {
      return("# Error: constraints not defined")
    }


    paste0(
      "library(BsplineQuantReg)\n\n",
      "x <- c(",
      paste(round(values$xtab, 4), collapse = ", "),
      ")\n",
      "y <- c(",
      paste(round(values$ytab, 4), collapse = ", "),
      ")\n",
      "knot <- c(",
      paste(round(values$knot, 4), collapse = ", "),
      ")\n\n",
      "fitted <- quantile_spline(x, y, knot,\n",
      "                       tau = ",
      input$tau,
      ",\n",
      "                       degree = ",
      input$degree,
      ",\n",
      "                       monot = c(",
      paste(constraints$monot, collapse = ", "),
      "),\n",
      "                       convcons = c(",
      paste(constraints$conv, collapse = ", "),
      "),\n",
      "                       der3cons = c(",
      paste(constraints$der3, collapse = ", "),
      "),\n",
      "                       solver = '",
      input$solver,
      "',\n",
      "                       callable = TRUE)\n\n",
      "x_eval <- seq(min(x), max(x), length.out = 300)\n",
      "y_eval <- fitted(x_eval)\n\n",
      "plot(x, y, pch = 16, cex = 0.5, col = 'gray',main='fitted spline')\n",
      "lines(x_eval, y_eval, col = '",
      input$curve_color,
      "', lwd = 2)\n"

    )
  })

  # ============ ACTIONS ============

  observeEvent(input$apply_color, {
    showNotification(paste("Color:", input$curve_color), type = "message")
  })

  observeEvent(input$clear_curves, {
    values$curve_lines[1] <- NULL
    showNotification("Last Curve cleared", type = "message")
  })

  observeEvent(input$clear_all, {
    values$xtab <- NULL
    values$ytab <- NULL
    values$knot <- NULL
    values$fitted <- NULL
    values$curve_lines <- list()
    values$regions <- list()
    values$region_id <- 0
    values$selected_region_id <- NULL
    values$data_name <- "No data"
    showNotification("All cleared", type = "message")
  })

  # ============ DEMOS DU PACKAGE ============

  demo_results <- reactiveValues(plot = NULL,
                                 output = NULL,
                                 height = 800)
  demo_heights <- list(
    comprehensive = 800,
    monotonicity = 800,
    logistic = 800,
    temperature = 800,
    temperature2 = 800,
    convexity = 800,
    degrees_comparison = 800,
    demo_der3 = 800,
    derivative2 = 1800
  )
  execute_demo <- function(demo_name) {
    showNotification(paste("Running demo:", demo_name), type = "message")

    withProgress(message = paste("Running", demo_name, "..."), {
      # Récupérer la hauteur depuis la liste
      demo_height <- demo_heights[[demo_name]] %||% 800
      demo_width <- if (demo_name == "derivative2")
        1500
      else
        800
      # Créer un fichier temporaire pour l'image
      temp_file <- tempfile(fileext = ".png")

      # Ouvrir un device PNG

      png(temp_file,
          width = demo_width,
          height = demo_height,
          res = 120)

      # Créer un environnement avec la variable degree
      demo_env <- new.env()
      demo_env$degree <- input$degree
      demo_env$par <- graphics::par
      demo_results$height <- demo_height

      # Capturer la sortie
      output_text <- capture.output({
        tryCatch({
          with(demo_env, {
            source(
              system.file("demo", paste0(demo_name, ".R"), package = "BsplineQuantReg"),
              local = TRUE,
              echo = FALSE
            )
          })
        }, error = function(e) {
          cat("Error:", e$message, "\n")
        })
      })

      # Fermer le device
      dev.off()

      # Lire l'image
      if (file.exists(temp_file)) {
        img <- png::readPNG(temp_file)
        demo_results$plot <- grid::rasterGrob(img, interpolate = TRUE)
        unlink(temp_file)
      } else {
        demo_results$plot <- NULL
      }

      demo_results$output <- output_text

      runjs('document.getElementById("demo_area").style.display = "block";')
    })
  }

  # Exécuter les démos
  observeEvent(input$demo_comp, {
    execute_demo("comprehensive")
  })
  observeEvent(input$demo_monot, {
    execute_demo("monotonicity_basic")
  })
  observeEvent(input$demo_log, {
    execute_demo("logistic")
  })
  observeEvent(input$demo_temp, {
    execute_demo("temperature")
  })
  observeEvent(input$demo_temp2, {
    execute_demo("temperature2")
  })
  observeEvent(input$demo_conv, {
    execute_demo("convexity")
  })
  observeEvent(input$demo_degrees, {
    execute_demo("degrees_comparison")
  })
  observeEvent(input$demo_der3, {
    execute_demo("demo_der3")
  })
  observeEvent(input$demo_derivative, {
    execute_demo("derivative2")
  })

  # Afficher les résultats
  output$demo_plot <- renderPlot({
    if (!is.null(demo_results$plot)) {
      grid::grid.draw(demo_results$plot)
    }
  })

  output$demo_output <- renderPrint({
    if (!is.null(demo_results$output)) {
      cat(paste(demo_results$output, collapse = "\n"))
    }
  })

}

# Run app
shinyApp(ui = ui, server = server)
