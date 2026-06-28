library(shiny)
library(caret)
library(ggplot2)
library(patchwork)
library(pROC)
library(ROCR)
library(dplyr)
library(tidyr)

# Define unified corporate CSS styles aligning with package palettes
custom_css <- "
  body { background-color: #F8F9FA; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
  .navbar { background-color: #111E6C !important; }
  .navbar-default .navbar-brand { color: #FFFFFF !important; font-weight: bold; }
  .navbar-default .navbar-nav > li > a { color: #FFFFFF !important; }
  .well { background-color: #FFFFFF !important; border: 1px solid #E3E6F0 !important; border-radius: 0.35rem !important; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15) !important; }
  .nav-tabs > li.active > a { background-color: #FFFFFF !important; border-bottom-color: transparent !important; font-weight: bold; color: #111E6C !important; }
  h3, h4 { color: #111E6C; font-weight: bold; }
  .shiny-output-error-validation { color: #FF4500; font-weight: bold; }
"

# -------------------------------------------------------------------------
# USER INTERFACE LAYOUT
# -------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(custom_css))),

  navbarPage(
    title = "ClassificationEnsembles Executive Studio",
    id = "main_nav",

    tabPanel(
      title = "STUDIO DASHBOARD",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Pipeline Configuration"),
          hr(),
          fileInput("data_file", "Upload Production Dataset (.csv)", accept = ".csv"),
          uiOutput("target_selector"),
          uiOutput("stratify_selector"),

          hr(),
          h4("Hyperparameter Bounds"),
          sliderInput("cv_folds", "Cross-Validation Folds", min = 2, max = 10, value = 5, step = 1),
          sliderInput("train_pct", "Training Allocation Split", min = 0.5, max = 0.9, value = 0.7, step = 0.05),
          numericInput("vif_threshold", "Max Collinearity VIF Bound", value = 5, min = 2, max = 20),

          hr(),
          h4("Aesthetic Style & Balance"),
          selectInput("sampling_method", "Imbalance Sampling Rule", choices = c("none", "down", "up", "smote")),
          selectInput("palette_style", "UI Theme Palette Style", choices = c("modern", "standard", "viridis")),

          actionButton("run_pipeline", "EXECUTE ENSEMBLE STUDIOS", class = "btn-primary btn-block", style = "margin-top: 15px; font-weight: bold;")
        ),

        mainPanel(
          width = 9,
          tabsetPanel(
            id = "studio_tabs",

            tabPanel("LEADERBOARD SUMMARY",
                     br(),
                     h3("Model Optimization Leaderboard"),
                     p("Architectures ordered by Macro-Averaged Area Under the Curve (Macro-AUC)."),
                     tableOutput("leaderboard_table"),
                     br(),
                     h4("Feature Space Correlation Health Heatmap"),
                     plotOutput("corr_plot", height = "400px")
            ),

            tabPanel("PERFORMANCE INTERVALS",
                     br(),
                     plotOutput("kpi_intervals_plot", height = "350px"),
                     br(),
                     plotOutput("risk_controls_plot", height = "350px")
            ),

            tabPanel("OVA ROC CANVAS",
                     br(),
                     h3("Faceted Multi-Class Receiver Operating Characteristics Canvas"),
                     p("Programmatic One-vs-All (OVA) curves evaluating boundary partitions."),
                     plotOutput("roc_canvas_plot", height = "600px")
            ),

            tabPanel("RELIABILITY & CONFUSION MATRIX",
                     br(),
                     fluidRow(
                       column(6,
                              h4("Probability Calibration Diagram"),
                              plotOutput("calibration_plot", height = "400px")
                       ),
                       column(6,
                              h4("Faceted Confusion Matrices Audit"),
                              uiOutput("model_cm_selector"),
                              verbatimTextOutput("confusion_matrix_text")
                       )
                     )
            ),

            tabPanel("EXECUTIVE DATA PROFILES",
                     br(),
                     h4("Profile 1: Technical Feature Metadata (Data Dictionary)"),
                     tableOutput("dict_table"),
                     br(),
                     h4("Profile 2: Multi-Collinearity Filter Audit (VIF Summary)"),
                     tableOutput("vif_table"),
                     br(),
                     h4("Profile 3: Production Sample Rows (Data Head)"),
                     tableOutput("head_table")
            )
          )
        )
      )
    )
  )
)

# -------------------------------------------------------------------------
# BACKEND EXECUTION SERVER
# -------------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive values data container to store model state arrays
  pipeline_state <- reactiveValues(object = NULL)

  raw_data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, stringsAsFactors = TRUE)
  })

  output$target_selector <- renderUI({
    req(raw_data())
    selectInput("target_col", "Dependent Target Variable", choices = colnames(raw_data()))
  })

  output$stratify_selector <- renderUI({
    req(raw_data())
    selectInput("stratify_col", "Stratification Feature Column (Optional)", choices = c("", colnames(raw_data())))
  })

  observeEvent(input$run_pipeline, {
    req(raw_data(), input$target_col)

    # Show user execution feedback notification
    id <- showNotification("Executing High-Velocity Classification Pipelines...", duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id), add = TRUE)

    fast_config <- ClassificationFastConfig()

    strat_val <- if(input$stratify_col == "") NULL else input$stratify_col

    # Run the core un-abbreviated engine directly
    res <- tryCatch({
      Classification(
        dataset         = raw_data(),
        target_col      = input$target_col,
        cv_folds        = input$cv_folds,
        train_pct       = input$train_pct,
        stratify_col    = strat_val,
        vif_threshold   = input$vif_threshold,
        sampling_method = input$sampling_method,
        palette_style   = input$palette_style,
        config          = fast_config,
        verbose         = FALSE
      )
    }, error = function(e) {
      showNotification(paste("Pipeline Error:", e$message), type = "error")
      NULL
    })

    pipeline_state$object <- res
  })

  # --- UI SELECTOR ROUTING ---
  output$model_cm_selector <- renderUI({
    req(pipeline_state$object)
    selectInput("cm_model_choice", "Select Model Confusion Matrix:", choices = names(pipeline_state$object$confusion_matrices))
  })

  # --- CONSOLE/TABLE DATA OUTPUT GENERATION ---
  output$leaderboard_table <- renderTable({
    req(pipeline_state$object)
    pipeline_state$object$performance_report[, c("Model", "Macro_AUC", "Accuracy", "Class_Error", "F1_Score", "KS_Statistic", "Duration")]
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$dict_table <- renderTable({
    req(pipeline_state$object)
    pipeline_state$object$data_dictionary
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$vif_table <- renderTable({
    req(pipeline_state$object)
    if(nrow(pipeline_state$object$vif_report) == 0) {
      return(data.frame(Status = "No attributes were filtered under current configurations."))
    }
    pipeline_state$object$vif_report
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$head_table <- renderTable({
    req(pipeline_state$object)
    head(pipeline_state$object$data_head, 5)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$confusion_matrix_text <- renderPrint({
    req(pipeline_state$object, input$cm_model_choice)
    pipeline_state$object$confusion_matrices[[input$cm_model_choice]]
  })

  # --- CHART PLOTTING RESPONSES ---
  output$corr_plot <- renderPlot({
    req(pipeline_state$object)
    print(pipeline_state$object$plots$correlation)
  })

  output$kpi_intervals_plot <- renderPlot({
    req(pipeline_state$object)
    print(pipeline_state$object$plots$kpis)
  })

  output$risk_controls_plot <- renderPlot({
    req(pipeline_state$object)
    print(pipeline_state$object$plots$risks)
  })

  output$calibration_plot <- renderPlot({
    req(pipeline_state$object)
    print(pipeline_state$object$plots$calibration)
  })

  output$roc_canvas_plot <- renderPlot({
    req(pipeline_state$object)
    plot(pipeline_state$object, pace_output = FALSE)
  })
}

shinyApp(ui = ui, server = server)
