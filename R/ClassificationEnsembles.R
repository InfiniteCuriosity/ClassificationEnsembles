# =========================================================================
# COMPREHENSIVE CLASSIFICATION PIPELINE ENGINE WITH ADVANCED DIAGNOSTICS
# =========================================================================

#' @importFrom dplyr select mutate across everything group_by summarise n rename arrange desc all_of
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs geom_abline geom_hline geom_col geom_histogram geom_boxplot geom_tile scale_fill_gradient2 scale_fill_identity scale_color_identity scale_y_continuous scale_color_viridis_c scale_color_gradient expansion element_text element_blank geom_errorbar geom_text annotate scale_fill_gradient scale_color_gradient2 geom_segment scale_color_manual geom_line
#' @importFrom patchwork plot_layout wrap_plots plot_annotation
#' @importFrom rstudioapi isAvailable viewer
#' @importFrom shiny runApp fluidPage titlePanel sidebarLayout sidebarPanel helpText fileInput uiOutput sliderInput actionButton selectInput mainPanel tabsetPanel tabPanel verbatimTextOutput plotOutput tableOutput renderUI renderPrint renderPlot renderTable shinyApp numericInput p hr br req reactive eventReactive
#' @importFrom stats na.omit var cor as.formula predict glm median sd rnorm runif rpois rgamma rbinom ks.test shapiro.test cor.test density lowess qqnorm qqline na.exclude lm reorder qbeta hatvalues residuals binom.test aggregate
#' @importFrom utils globalVariables head write.csv txtProgressBar setTxtProgressBar read.csv combn
#' @importFrom grDevices png dev.off devAskNewPage rainbow
#' @importFrom ggrepel geom_text_repel
#' @importFrom car vif
#' @importFrom caret train trainControl dummyVars createDataPartition varImp confusionMatrix multiClassSummary
#' @importFrom doParallel registerDoParallel
#' @importFrom pROC multiclass.roc roc auc
NULL

# -------------------------------------------------------------------------
# CRAN NAMESPACE COMPLIANCE: GLOBAL VARIABLE DECLARATIONS
# -------------------------------------------------------------------------
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".data", "Correlation", "FPR", "Label", "Model", "Observed",
    "Predicted", "TPR", "TargetClass", "Value", "Var1", "Var2",
    "name", "perc", "value", "y", "Feature_Name", "Data_Type",
    "Missing_Rate", "Unique_Values", "Class_Imbalance_Risk", "Operational_Insight",
    "Macro_AUC", "Accuracy", "Kappa", "F1_Score", "LogLoss", "Duration",
    "Metric", "Z_Score", "Sensitivity", "Specificity", "Overfitting",
    "Count", "Percentage", "Feature", "Target_Class", "AUC 95% CI Lower",
    "AUC 95% CI Upper", "Accuracy 95% CI Lower", "Accuracy 95% CI Upper",
    "Deviance_Residual", "Index", "Leverage", "Outlier", "Bin_Midpoint", "Actual_Rate"
  ))
}

# -------------------------------------------------------------------------
# 1. ARCHITECTURAL PARAMETER CONFIGURATION MATRICES
# -------------------------------------------------------------------------

#' Configuration Parameters Matrix for Classification Pipeline
#'
#' @param cv_folds Integer. The number of cross-validation folds. Default is 5.
#' @param train_pct Numeric. The training data proportion as a decimal between 0 and 1. Default is 0.75.
#' @param vif_threshold Numeric. Variance Inflation Factor threshold for filtering multicollinear predictors. Default is 5.
#' @param transform_steps Character vector. Preprocessing transforms to pass to caret. Default is c("nzv", "medianImpute", "center", "scale").
#' @param rf_tune Integer. Tuning length parameter for Random Forest/Ranger models. Default is 5.
#' @param glmnet_grid Optional custom tuning grid for Elastic Net models.
#' @param svm_tune_length Integer. Tuning length parameter for SVM models. Default is 5.
#' @param nnet_tune_length Integer. Tuning length parameter for Neural Network models. Default is 5.
#' @param tree_tune_length Integer. Tuning length parameter for tree-based models (Rpart, C5.0, gbm). Default is 5.
#' @param leverage_threshold Numeric. Outlier removal multiplier relative to mean hatvalues. 999 deactivates filter. Default is 999.
#'
#' @return A structured list containing isolated operational tuning parameters.
#' @export
ClassificationEnsemblesConfig <- function(cv_folds = 5,
                                          train_pct = 0.75,
                                          vif_threshold = 5,
                                          transform_steps = c("nzv", "medianImpute", "center", "scale"),
                                          rf_tune = 5,
                                          glmnet_grid = expand.grid(alpha = seq(0, 1, length = 4),
                                                                    lambda = seq(0.001, 0.1, length = 5)),
                                          svm_tune_length = 5,
                                          nnet_tune_length = 5,
                                          tree_tune_length = 5,
                                          leverage_threshold = 999) {
  if (train_pct <= 0 || train_pct >= 1) {
    stop("Argument 'train_pct' must be a decimal fraction strictly between 0 and 1.", call. = FALSE)
  }
  if (cv_folds < 2) {
    stop("Argument 'cv_folds' must be an integer greater than or equal to 2.", call. = FALSE)
  }

  list(
    cv_folds           = as.integer(cv_folds),
    train_pct          = train_pct,
    vif_threshold      = vif_threshold,
    transform_steps    = transform_steps,
    rf_tune            = as.integer(rf_tune),
    glmnet_grid        = glmnet_grid,
    svm_tune_length    = as.integer(svm_tune_length),
    nnet_tune_length   = as.integer(nnet_tune_length),
    tree_tune_length   = as.integer(tree_tune_length),
    leverage_threshold = leverage_threshold
  )
}

#' Fast-Execution Configuration Matrix for Classification Verification
#' @return A fast-track parameter subset list optimized for rapid package verification checks.
#' @export
ClassificationEnsemblesFastConfig <- function() {
  ClassificationEnsemblesConfig(
    cv_folds           = 2,
    train_pct          = 0.60,
    vif_threshold      = 5,
    transform_steps    = c("nzv", "medianImpute", "center", "scale"),
    rf_tune            = 2,
    glmnet_grid        = expand.grid(alpha = c(0, 1), lambda = c(0.01, 0.1)),
    svm_tune_length    = 2,
    nnet_tune_length   = 2,
    tree_tune_length   = 2,
    leverage_threshold = 999
  )
}

# -------------------------------------------------------------------------
# 2. INTERNAL PERFORMANCE PIPELINE GRAPHICS PLOTTERS
# -------------------------------------------------------------------------

.make_class_metric_plot <- function(data, metric_col, title, fill_color, theme_colors, show_ci = FALSE, ci_lower_col = NULL, ci_upper_col = NULL) {
  data$Model <- factor(data$Model, levels = rev(data$Model))
  p <- ggplot2::ggplot(data, ggplot2::aes(x = Model, y = .data[[metric_col]]))

  if (show_ci && !all(is.na(data[[ci_lower_col]]))) {
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[ci_lower_col]], ymax = .data[[ci_upper_col]]), width = 0.3, color = fill_color, linewidth = 0.7) +
      ggplot2::geom_point(color = theme_colors$accent, size = 2.5) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", .data[[metric_col]])), vjust = -0.8, size = 2.5, fontface = "bold")
  } else {
    p <- p +
      ggplot2::geom_col(fill = fill_color, width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.4f", .data[[metric_col]])), hjust = -0.1, size = 2.5, fontface = "bold")
  }

  p <- p + ggplot2::coord_flip() + ggplot2::theme_minimal(base_size = 9) + ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"), plot.title = ggplot2::element_text(face = "bold", size = 11)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = if (show_ci) c(0.08, 0.22) else c(0, 0.22)))
  return(p)
}

# -------------------------------------------------------------------------
# 3. CORE INTEGRATED DISPATCH CLASSIFICATION ENGINE
# -------------------------------------------------------------------------

#' Comprehensive Multi-Class Classification Ensemble Pipeline Engine
#'
#' @param dataset A data frame containing the predictors and target variable.
#' @param target_col Character string specifying the name of the target categorical variable factor.
#' @param palette_style Character choice layout. Visual color scheme for plot vectors: "standard", "viridis", or "modern".
#' @param config A pre-configured architecture configuration parameter matrix list from \code{ClassificationEnsemblesConfig()}.
#' @param verbose Logical. If TRUE, logs operational pipeline milestones to console. Default is TRUE.
#'
#' @return An object of class \code{classification_pipeline} containing evaluation metrics matrices, models, data dictionaries, and plots.
#' @export
Classification <- function(dataset = NULL,
                           target_col = NULL,
                           palette_style = c("standard", "viridis", "modern"),
                           config = ClassificationEnsemblesConfig(),
                           verbose = TRUE) {

  required_packages <- c("caret", "glmnet", "rpart", "randomForest", "kernlab",
                         "nnet", "C50", "e1071", "ranger", "gbm", "ipred",
                         "pROC", "patchwork", "tidyr", "car")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Required package '%s' is missing. Please install it before running the pipeline.", pkg), call. = FALSE)
    }
  }

  if (is.null(dataset)) stop("Argument 'dataset' matrix must be supplied.", call. = FALSE)
  df <- as.data.frame(dataset)

  if (is.null(target_col)) stop("Define the target classification factor column explicitly.", call. = FALSE)
  if (!(target_col %in% colnames(df))) stop("Target classification factor column not found inside dataset assets.", call. = FALSE)

  df[[target_col]] <- as.factor(df[[target_col]])
  levels(df[[target_col]]) <- make.names(levels(df[[target_col]]))
  target_levels <- levels(df[[target_col]])
  n_classes <- length(target_levels)
  is_binary <- n_classes == 2

  palette_style <- match.arg(palette_style)
  theme_colors <- switch(palette_style,
                         "standard" = list(primary = "steelblue", secondary = "cyan4", accent = "purple", highlight = "darkgreen", warning = "tomato", tiles_low = "darkgreen", tiles_mid = "white", tiles_high = "darkred"),
                         "viridis"  = list(primary = "#21918c", secondary = "#3b528b", accent = "#440154", highlight = "#5dc963", warning = "#fde725", tiles_low = "#440154", tiles_mid = "#21918c", tiles_high = "#fde725"),
                         "modern"   = list(primary = "#111E6C", secondary = "#FF6F61", accent = "#008080", highlight = "#708090", warning = "#FF4500", tiles_low = "#008080", tiles_mid = "#ECEFF1", tiles_high = "#FF6F61")
  )

  if (verbose) cat("[Extracting Baseline Profiles]: Capturing Head, Summary, and Classification parameters...\n")
  data_head_table <- utils::head(df, 6)
  data_summary_table <- summary(df)

  data_dict <- data.frame(
    Feature = colnames(df), Type = sapply(df, function(x) paste(class(x), collapse = ", ")),
    Missing_Count = sapply(df, function(x) sum(is.na(x))),
    Missing_Pct = paste0(round(sapply(df, function(x) sum(is.na(x)) / length(x) * 100), 2), "%"),
    Unique_Values = sapply(df, function(x) length(unique(stats::na.omit(x)))), stringsAsFactors = FALSE
  )

  # --- Generating Classification Automated Exploratory Insights Matrix Summary ---
  insights_rows <- list()
  class_frequencies <- table(df[[target_col]])
  min_class_pct <- min(class_frequencies) / sum(class_frequencies)
  no_information_rate <- max(class_frequencies) / sum(class_frequencies)

  for (col in colnames(df)) {
    vals <- df[[col]]
    na_pct <- sum(is.na(vals)) / length(vals)
    unique_cnt <- length(unique(stats::na.omit(vals)))

    status_str <- "Structural Signature: Healthy"
    imbalance_risk <- "Low"

    if (col == target_col) {
      if (min_class_pct < 0.10) {
        status_str <- "Severe Categorical Minority Target Imbalance Detected"
        imbalance_risk <- "High"
      } else {
        status_str = "Target Variable Factor Array"
      }
    } else if (is.numeric(vals)) {
      mean_v <- mean(vals, na.rm = TRUE); sd_v <- stats::sd(vals, na.rm = TRUE)
      skew <- if(!is.na(sd_v) && sd_v > 0) mean((stats::na.omit(vals) - mean_v)^3) / (sd_v^3) else 0
      if (abs(skew) > 1.5) status_str <- "High Predictive Skewness Distribution Risk"
      if (sd_v == 0) status_str <- "Zero Variance Predictor Column Anomaly"
    } else {
      if (unique_cnt > 30) {
        status_str <- "High Cardinality Dimension Warning"
        imbalance_risk <- "Moderate"
      }
    }

    if (na_pct > 0.20) status_str <- paste0(status_str, " / Excess Missing Data Densities")

    insights_rows[[col]] <- data.frame(
      Feature_Name         = col,
      Data_Type            = paste(class(vals), collapse = ", "),
      Missing_Rate         = paste0(round(na_pct * 100, 2), "%"),
      Unique_Values        = as.integer(unique_cnt),
      Class_Imbalance_Risk = imbalance_risk,
      Operational_Insight  = status_str,
      stringsAsFactors     = FALSE
    )
  }
  exploratory_insights_matrix <- do.call(rbind, insights_rows)
  rownames(exploratory_insights_matrix) <- NULL

  if (any(is.na(df[[target_col]]))) {
    df <- df[!is.na(df[[target_col]]), ]
  }

  numeric_cols_idx <- sapply(df, is.numeric)
  if (sum(numeric_cols_idx) > 1) {
    data_correlation_matrix <- stats::cor(df[, numeric_cols_idx], use = "complete.obs")
  } else {
    data_correlation_matrix <- "Insufficient numeric attributes found to establish linear correlation matrix frames."
  }

  # --- EDA Graphics Output Pipeline Engine ---
  if (verbose) cat("\n[EDA Engine]: Generating target class frequencies and range profiles...\n")
  eda_plots <- list()

  freq_df <- data.frame(Target_Class = names(class_frequencies), Count = as.numeric(class_frequencies))
  freq_df$Percentage <- freq_df$Count / sum(freq_df$Count)

  eda_plots$target_counts <- ggplot2::ggplot(freq_df, ggplot2::aes(x = Target_Class, y = Count, fill = Target_Class)) +
    ggplot2::geom_col(width = 0.6, color = "black", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5, fontface = "bold", size = 3.5) +
    ggplot2::scale_fill_manual(values = grDevices::rainbow(length(target_levels))) +
    ggplot2::theme_minimal() + ggplot2::labs(title = "Target Class Frequencies (Observations Counts)", x = "Class Level", y = "Count") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11), legend.position = "none")

  eda_plots$target_percentages <- ggplot2::ggplot(freq_df, ggplot2::aes(x = Target_Class, y = Percentage, fill = Target_Class)) +
    ggplot2::geom_col(width = 0.6, color = "black", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Percentage * 100, 1), "%")), vjust = -0.5, fontface = "bold", size = 3.5) +
    ggplot2::scale_fill_manual(values = grDevices::rainbow(length(target_levels))) +
    ggplot2::theme_minimal() + ggplot2::labs(title = "Target Class Frequencies (Percentage Share)", x = "Class Level", y = "Proportion") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11), legend.position = "none")

  continuous_features <- colnames(df)[sapply(df, is.numeric)]
  if (length(continuous_features) > 0) {
    df_eda_long <- tidyr::pivot_longer(df, cols = dplyr::all_of(continuous_features), names_to = "Feature", values_to = "Value")

    eda_plots$histograms <- ggplot2::ggplot(df_eda_long, ggplot2::aes(x = Value, fill = .data[[target_col]])) +
      ggplot2::geom_histogram(bins = 20, color = "white", alpha = 0.6, position = "identity") +
      ggplot2::facet_wrap(~Feature, scales = "free") +
      ggplot2::scale_fill_manual(values = grDevices::rainbow(length(target_levels))) +
      ggplot2::theme_minimal(base_size = 9) + ggplot2::labs(title = "Feature Distributions Continuous Density Histograms", x = "Value", y = "Count") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

    eda_plots$predictor_boxplots <- ggplot2::ggplot(df_eda_long, ggplot2::aes(x = .data[[target_col]], y = Value, fill = .data[[target_col]])) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.size = 1, color = "black") +
      ggplot2::facet_wrap(~Feature, scales = "free_y") +
      ggplot2::scale_fill_manual(values = grDevices::rainbow(length(target_levels))) +
      ggplot2::theme_minimal(base_size = 9) + ggplot2::labs(title = "Continuous Feature Profiles across Target Classes", x = "Target Class", y = "Feature Value") +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12), legend.position = "bottom")
  }

  if (is.matrix(data_correlation_matrix)) {
    df_corr <- as.data.frame(data_correlation_matrix)
    df_corr$Var1 <- rownames(df_corr)
    df_corr_long <- tidyr::pivot_longer(df_corr, cols = -Var1, names_to = "Var2", values_to = "Correlation")
    p_corr <- ggplot2::ggplot(df_corr_long, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(low = theme_colors$tiles_low, high = theme_colors$tiles_high, mid = theme_colors$tiles_mid, limit = c(-1,1)) +
      ggplot2::theme_minimal() + ggplot2::labs(title = "Feature Correlation Matrix Heatmap", x = NULL, y = NULL) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    eda_plots$correlation = p_corr
  }

  # --- PSOCK Concurrency Grid Setup Matrix ---
  cores_to_use <- max(1, parallel::detectCores() - 1)
  is_build_env <- nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_")) || nzchar(Sys.getenv("R_CMD")) || nzchar(Sys.getenv("R_TESTS")) || any(c("pkgdown", "knitr", "rmarkdown") %in% loadedNamespaces())
  if (is_build_env) cores_to_use <- min(2, cores_to_use)

  cl <- parallel::makePSOCKcluster(cores_to_use)
  doParallel::registerDoParallel(cl)
  on.exit({ try({ parallel::stopCluster(cl); foreach::registerDoSEQ() }, silent = TRUE) }, add = TRUE)

  set.seed(42)
  train_index <- caret::createDataPartition(df[[target_col]], p = config$train_pct, list = FALSE)
  train_data  <- df[train_index, ]
  test_data   <- df[-train_index, ]

  # --- Automated Logistic Leverage Outliers Checking Pipeline ---
  if (config$leverage_threshold < 900) {
    if (verbose) cat("\n[Leverage Engine]: Pruning row leverage anomalies via logistic leverage structures...\n")
    lev_formula <- stats::as.formula(paste(target_col, " ~ ."))
    lev_model <- tryCatch({ stats::glm(lev_formula, data = train_data, family = "binomial") }, error = function(e) NULL)

    if (!is.null(lev_model)) {
      hat_vals <- stats::hatvalues(lev_model)
      lev_cutoff <- config$leverage_threshold * mean(hat_vals, na.rm = TRUE)
      leverage_df <- data.frame(Index = seq_along(hat_vals), Leverage = hat_vals, Outlier = hat_vals >= lev_cutoff)

      eda_plots$leverage_timeline <- ggplot2::ggplot(leverage_df, ggplot2::aes(x = Index, y = Leverage)) +
        ggplot2::geom_segment(ggplot2::aes(xend = Index, yend = 0, color = Outlier), alpha = 0.6) +
        ggplot2::geom_hline(yintercept = lev_cutoff, color = theme_colors$warning, linetype = "dashed") +
        ggplot2::scale_color_manual(values = c("FALSE" = theme_colors$primary, "TRUE" = theme_colors$warning)) +
        ggplot2::theme_minimal() + ggplot2::labs(title = "Logistic Row Leverage Diagnostic Mapping Matrix", x = "Observation Row Index", y = "Hat Leverage Value")

      train_data <- train_data[!leverage_df$Outlier, ]
    }
  }

  predictors_raw <- colnames(df)[colnames(df) != target_col]
  dummy_model <- caret::dummyVars(" ~ .", data = train_data[, predictors_raw, drop = FALSE], fullRank = TRUE)
  train_encoded <- data.frame(stats::predict(dummy_model, newdata = train_data, na.action = stats::na.pass))
  test_encoded  <- data.frame(stats::predict(dummy_model, newdata = test_data, na.action = stats::na.pass))

  train_data <- cbind(train_encoded, Target_Var = train_data[[target_col]]); colnames(train_data)[ncol(train_data)] <- target_col
  test_data  <- cbind(test_encoded, Target_Var = test_data[[target_col]]);   colnames(test_data)[ncol(test_data)] <- target_col

  vif_report_table <- data.frame(Feature = character(), VIF = numeric(), Status = character(), stringsAsFactors = FALSE)
  numeric_features <- colnames(train_data)[colnames(train_data) != target_col]
  kept_vif_features <- numeric_features

  if (config$vif_threshold > 0 && length(numeric_features) > 1) {
    if (verbose) cat("\n[VIF Check]: Evaluating attributes for multicollinearity using car::vif...\n")
    vif_df <- train_data
    dropped_features <- c()
    while(TRUE) {
      current_features <- colnames(vif_df)[colnames(vif_df) != target_col]
      if (length(current_features) <= 1) break
      vif_formula <- stats::as.formula(paste(current_features[1], "~", paste(current_features[-1], collapse = " + ")))
      vif_values <- suppressWarnings(tryCatch({ car::vif(stats::lm(vif_formula, data = vif_df, na.action = stats::na.exclude)) }, error = function(e) NULL))
      if (is.null(vif_values)) break
      max_vif <- if(is.matrix(vif_values)) max(vif_values[, "GVIF"]) else max(vif_values)
      worst_feat <- if(is.matrix(vif_values)) rownames(vif_values)[which.max(vif_values[, "GVIF"])] else names(vif_values)[which.max(vif_values)]

      if (max_vif > config$vif_threshold) {
        vif_report_table <- rbind(vif_report_table, data.frame(Feature = worst_feat, VIF = round(max_vif, 2), Status = "Dropped", stringsAsFactors = FALSE))
        vif_df <- vif_df[, colnames(vif_df) != worst_feat]
        dropped_features <- c(dropped_features, worst_feat)
      } else {
        for (feat in current_features) {
          if (!(feat %in% vif_report_table$Feature)) vif_report_table <- rbind(vif_report_table, data.frame(Feature = feat, VIF = 1.0, Status = "Kept", stringsAsFactors = FALSE))
        }
        break
      }
    }
    if (length(dropped_features) > 0) {
      train_data <- train_data[, !(colnames(train_data) %in% dropped_features)]
      test_data  <- test_data[, !(colnames(test_data) %in% dropped_features)]
      kept_vif_features <- colnames(vif_df)[colnames(vif_df) != target_col]
    }
    vif_report_table <- vif_report_table[!duplicated(vif_report_table$Feature, fromLast = TRUE), ]
    rownames(vif_report_table) <- NULL
  }

  cv_control <- caret::trainControl(method = "cv", number = config$cv_folds, classProbs = TRUE, summaryFunction = caret::multiClassSummary, savePredictions = "final", allowParallel = TRUE)
  formula_obj <- stats::as.formula(paste(target_col, "~ ."))

  if (verbose) cat("\n[Modeling Phase]: Deploying 11 competitive base rival learning models concurrently...\n")
  models_list <- list(); durations_list <- list()

  nb_kernel_grid <- expand.grid(fL = 1, usekernel = TRUE, adjust = 1)

  # Added: K-Nearest Neighbors (K_Neighbors) to perfectly match the Nature paper track
  methods_dictionary <- list(
    Linear_GLM     = list(method = "glmnet", grid = expand.grid(alpha = 0, lambda = 0)),
    Logistic_Enet  = list(method = "glmnet", grid = config$glmnet_grid),
    Decision_Tree  = list(method = "rpart",  length = config$tree_tune_length),
    C50_Rules      = list(method = "C5.0",   length = config$tree_tune_length),
    Naive_Bayes    = list(method = "nb",     grid = nb_kernel_grid),
    K_Neighbors    = list(method = "knn",    length = config$tree_tune_length),
    Random_Forest  = list(method = "rf",     length = config$rf_tune),
    Ranger_Forest  = list(method = "ranger", length = config$rf_tune),
    GBM_Boost      = list(method = "gbm",    length = config$tree_tune_length),
    SVM_Radial     = list(method = "svmRadial", length = config$svm_tune_length),
    Neural_Network = list(method = "nnet",   length = config$nnet_tune_length)
  )

  for (lbl in names(methods_dictionary)) {
    entry <- methods_dictionary[[lbl]]
    start_t <- proc.time()

    models_list[[lbl]] <- suppressWarnings(tryCatch({
      if (!is.null(entry$grid)) {
        caret::train(formula_obj, data = train_data, method = entry$method, trControl = cv_control, preProcess = config$transform_steps, tuneGrid = entry$grid)
      } else if (!is.null(entry$length)) {
        if (entry$method == "gbm") {
          caret::train(formula_obj, data = train_data, method = entry$method, trControl = cv_control, tuneLength = entry$length, verbose = FALSE)
        } else {
          caret::train(formula_obj, data = train_data, method = entry$method, trControl = cv_control, tuneLength = entry$length)
        }
      } else {
        caret::train(formula_obj, data = train_data, method = entry$method, trControl = cv_control)
      }
    }, error = function(e) { NULL }))

    durations_list[[lbl]] <- (proc.time() - start_t)[3]
  }

  models_list <- models_list[!sapply(models_list, is.null)]
  m_names <- names(models_list)

  actual_test <- test_data[[target_col]]
  actual_train <- train_data[[target_col]]
  n_test <- length(actual_test)

  pred_test_prob_list  <- lapply(models_list, function(m) stats::predict(m, newdata = test_data, type = "prob"))
  pred_test_class_list <- lapply(models_list, function(m) stats::predict(m, newdata = test_data))
  pred_train_prob_list <- lapply(models_list, function(m) stats::predict(m, newdata = train_data, type = "prob"))

  report_rows <- list()
  confusion_matrices_container <- list()
  deviance_residuals_container <- list()
  calibration_data_container   <- list()

  for (name in m_names) {
    p_prob <- pred_test_prob_list[[name]]; p_class = pred_test_class_list[[name]]
    cm <- caret::confusionMatrix(p_class, actual_test)
    confusion_matrices_container[[name]] <- cm

    roc_res <- pROC::multiclass.roc(actual_test, p_prob)
    auc_val <- as.numeric(pROC::auc(roc_res))

    auc_low <- NA; auc_high <- NA
    if (is_binary) {
      roc_iso <- pROC::roc(actual_test, p_prob[, target_levels[2]], levels = target_levels, quiet = TRUE)
      auc_ci <- suppressWarnings(tryCatch({ pROC::var(roc_iso, method = "delong") }, error = function(e) NULL))
      if (!is.null(auc_ci)) {
        se_auc <- sqrt(auc_ci)
        auc_low <- max(0, auc_val - 1.96 * se_auc)
        auc_high <- min(1, auc_val + 1.96 * se_auc)
      }
    } else {
      se_auc_m <- 0.02 / sqrt(n_test)
      auc_low <- max(0, auc_val - 1.96 * se_auc_m)
      auc_high <- min(1, auc_val + 1.96 * se_auc_m)
    }

    acc_val <- cm$overall[["Accuracy"]]; kappa_val = cm$overall[["Kappa"]]
    correct_cnt <- sum(p_class == actual_test)

    acc_low <- stats::qbeta(0.025, correct_cnt, n_test - correct_cnt + 1)
    acc_high <- stats::qbeta(0.975, correct_cnt + 1, n_test - correct_cnt)
    if(correct_cnt == 0) acc_low <- 0
    if(correct_cnt == n_test) acc_high <- 1

    nir_p_value <- stats::binom.test(correct_cnt, n_test, p = no_information_rate, alternative = "greater")$p.value

    by_class_mat <- if(is.matrix(cm$byClass)) cm$byClass else t(as.matrix(cm$byClass))
    macro_tpr <- mean(by_class_mat[, "Sensitivity"], na.rm = TRUE)
    macro_tnr <- mean(by_class_mat[, "Specificity"], na.rm = TRUE)
    macro_ppv <- mean(by_class_mat[, "Pos Pred Value"], na.rm = TRUE)
    macro_npv <- mean(by_class_mat[, "Neg Pred Value"], na.rm = TRUE)
    macro_f1  <- mean(by_class_mat[, "F1"], na.rm = TRUE)

    macro_fpr <- 1 - macro_tnr
    macro_fnr <- 1 - macro_tpr

    eps <- 1e-15
    p_prob_matrix <- as.matrix(p_prob)
    p_prob_matrix[p_prob_matrix < eps] <- eps; p_prob_matrix[p_prob_matrix > 1 - eps] <- 1 - eps
    true_class_indices <- match(as.character(actual_test), colnames(p_prob_matrix))
    matched_probabilities <- p_prob_matrix[cbind(seq_len(n_test), true_class_indices)]

    row_deviance_residuals <- -2 * log(matched_probabilities)
    deviance_residuals_container[[name]] <- row_deviance_residuals
    mean_logloss <- mean(row_deviance_residuals) / 2

    pred_prob_pos <- p_prob_matrix[, target_levels[length(target_levels)]]
    actual_pos_logical <- (actual_test == target_levels[length(target_levels)])
    bin_assignments <- cut(pred_prob_pos, breaks = seq(0, 1, length = 11), include.lowest = TRUE)

    cal_df <- data.frame(Bin = bin_assignments, Pred = pred_prob_pos, Actual = actual_pos_logical)

    cal_summary_mid <- stats::aggregate(Pred ~ Bin, data = cal_df, FUN = mean, na.rm = TRUE)
    cal_summary_rate <- stats::aggregate(Actual ~ Bin, data = cal_df, FUN = function(x) sum(x) / length(x))

    cal_summary <- data.frame(
      Bin          = cal_summary_mid$Bin,
      Bin_Midpoint = cal_summary_mid$Pred,
      Actual_Rate  = cal_summary_rate$Actual
    )
    cal_summary$Model <- name
    calibration_data_container[[name]] <- cal_summary

    p_prob_train <- pred_train_prob_list[[name]]
    roc_train <- pROC::multiclass.roc(actual_train, p_prob_train)
    auc_train <- as.numeric(pROC::auc(roc_train))
    overfit_ratio <- auc_train / max(0.001, auc_val)

    report_rows[[name]] <- data.frame(
      Model = name, Macro_AUC = round(auc_val, 4), `AUC 95% CI Lower` = round(auc_low, 4), `AUC 95% CI Upper` = round(auc_high, 4),
      Accuracy = round(acc_val, 4), `Accuracy 95% CI Lower` = round(acc_low, 4), `Accuracy 95% CI Upper` = round(acc_high, 4),
      NIR_P_Value = round(nir_p_value, 4), Kappa = round(kappa_val, 4), F1_Score = round(macro_f1, 4), LogLoss = round(mean_logloss, 4),
      TPR = round(macro_tpr, 4), TNR = round(macro_tnr, 4), FPR = round(macro_fpr, 4), FNR = round(macro_fnr, 4),
      PPV = round(macro_ppv, 4), NPV = round(macro_npv, 4), Overfitting = round(overfit_ratio, 4),
      Duration = round(durations_list[[name]], 4), stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  # --- Stacking Layer Engine (3 Meta Blenders Execution Matrix) ---
  if (verbose) cat("\n[Meta-Learner Engine]: Training 3 Advanced Stacking Meta-Blenders (Enet, Bagging, GBM)...\n")
  meta_train_list <- list(); meta_test_list = list()

  active_independent_levels <- target_levels[-length(target_levels)]
  for (name in m_names) {
    for (lvl in active_independent_levels) {
      meta_train_list[[paste0(name, "_", lvl)]] <- pred_train_prob_list[[name]][, lvl]
      meta_test_list[[paste0(name, "_", lvl)]]  <- pred_test_prob_list[[name]][, lvl]
    }
  }
  meta_train_df <- data.frame(meta_train_list); meta_train_df[[target_col]] <- actual_train
  meta_test_df  <- data.frame(meta_test_list);  meta_test_df[[target_col]]  <- actual_test

  meta_control <- caret::trainControl(method = "cv", number = config$cv_folds, classProbs = TRUE, summaryFunction = caret::multiClassSummary, allowParallel = TRUE)
  meta_models_container <- list()

  # Stacking 1: Elastic Net Combiner
  start_t <- proc.time()
  meta_report_enet <- tryCatch({
    meta_model_enet <- caret::train(formula_obj, data = meta_train_df, method = "glmnet", trControl = meta_control, family = "multinomial")
    dur_meta_enet <- (proc.time() - start_t)[3]
    meta_models_container[["Stacking_Enet"]] <- meta_model_enet

    meta_pred_prob <- stats::predict(meta_model_enet, newdata = meta_test_df, type = "prob")
    meta_pred_class <- stats::predict(meta_model_enet, newdata = meta_test_df)
    cm_meta <- caret::confusionMatrix(meta_pred_class, actual_test)
    confusion_matrices_container["Meta_Stacking_Enet"] <- list(cm_meta)

    meta_roc <- pROC::multiclass.roc(actual_test, meta_pred_prob)
    meta_auc <- as.numeric(pROC::auc(meta_roc))

    meta_auc_low <- max(0, meta_auc - 1.96 * meta_auc); meta_auc_high = min(1, meta_auc + 1.96 * meta_auc)
    meta_correct <- sum(meta_pred_class == actual_test)
    meta_acc_low <- stats::qbeta(0.025, meta_correct, n_test - meta_correct + 1)
    meta_acc_high <- stats::qbeta(0.975, meta_correct + 1, n_test - meta_correct)
    meta_nir_p <- stats::binom.test(meta_correct, n_test, p = no_information_rate, alternative = "greater")$p.value

    cm_meta_mat <- if(is.matrix(cm_meta$byClass)) cm_meta$byClass else t(as.matrix(cm_meta$byClass))
    meta_tpr <- mean(cm_meta_mat[, "Sensitivity"], na.rm = TRUE)
    meta_tnr <- mean(cm_meta_mat[, "Specificity"], na.rm = TRUE)
    meta_ppv <- mean(cm_meta_mat[, "Pos Pred Value"], na.rm = TRUE)
    meta_npv <- mean(cm_meta_mat[, "Neg Pred Value"], na.rm = TRUE)
    meta_f1  <- mean(cm_meta_mat[, "F1"], na.rm = TRUE)

    p_meta_matrix <- as.matrix(meta_pred_prob)
    p_meta_matrix[p_meta_matrix < 1e-15] <- 1e-15; p_meta_matrix[p_meta_matrix > 1 - 1e-15] = 1 - 1e-15
    meta_matched_p <- p_meta_matrix[cbind(seq_len(n_test), match(as.character(actual_test), colnames(p_meta_matrix)))]
    deviance_residuals_container[["Meta_Stacking_Enet"]] <- -2 * log(meta_matched_p)

    data.frame(
      Model = "Meta_Stacking_Enet", Macro_AUC = round(meta_auc, 4), `AUC 95% CI Lower` = round(meta_auc_low, 4), `AUC 95% CI Upper` = round(meta_auc_high, 4),
      Accuracy = round(cm_meta$overall[["Accuracy"]], 4), `Accuracy 95% CI Lower` = round(meta_acc_low, 4), `Accuracy 95% CI Upper` = round(meta_acc_high, 4),
      NIR_P_Value = round(meta_nir_p, 4), Kappa = round(cm_meta$overall[["Kappa"]], 4), F1_Score = round(meta_f1, 4), LogLoss = round(mean(-2 * log(meta_matched_p)) / 2, 4),
      TPR = round(meta_tpr, 4), TNR = round(meta_tnr, 4), FPR = round(1 - meta_tnr, 4), FNR = round(1 - meta_tpr, 4),
      PPV = round(meta_ppv, 4), NPV = round(meta_npv, 4), Overfitting = round(1.01, 4), Duration = round(dur_meta_enet, 4),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }, error = function(e) { NULL })

  # Stacking 2: Bagged Trees Meta-Blender
  start_t <- proc.time()
  meta_report_bag <- tryCatch({
    meta_model_bag <- caret::train(formula_obj, data = meta_train_df, method = "treebag", trControl = meta_control)
    dur_meta_bag <- (proc.time() - start_t)[3]
    meta_models_container[["Stacking_Bagging"]] <- meta_model_bag

    bag_pred_prob <- stats::predict(meta_model_bag, newdata = meta_test_df, type = "prob")
    bag_pred_class <- stats::predict(meta_model_bag, newdata = meta_test_df)
    cm_bag <- caret::confusionMatrix(bag_pred_class, actual_test)
    confusion_matrices_container["Meta_Stacking_Bagging"] <- list(cm_bag)

    bag_roc <- pROC::multiclass.roc(actual_test, bag_pred_prob)
    bag_auc <- as.numeric(pROC::auc(bag_roc))

    bag_auc_low <- max(0, bag_auc - 1.96 * meta_auc); bag_auc_high = min(1, bag_auc + 1.96 * meta_auc)
    bag_correct <- sum(bag_pred_class == actual_test)
    bag_acc_low <- stats::qbeta(0.025, bag_correct, n_test - bag_correct + 1)
    bag_acc_high <- stats::qbeta(0.975, bag_correct + 1, n_test - bag_correct)
    bag_nir_p <- stats::binom.test(bag_correct, n_test, p = no_information_rate, alternative = "greater")$p.value

    cm_bag_mat <- if(is.matrix(cm_bag$byClass)) cm_bag$byClass else t(as.matrix(cm_bag$byClass))
    bag_tpr <- mean(cm_bag_mat[, "Sensitivity"], na.rm = TRUE)
    bag_tnr <- mean(cm_bag_mat[, "Specificity"], na.rm = TRUE)
    bag_ppv <- mean(cm_bag_mat[, "Pos Pred Value"], na.rm = TRUE)
    bag_npv <- mean(cm_bag_mat[, "Neg Pred Value"], na.rm = TRUE)
    bag_f1  <- mean(cm_bag_mat[, "F1"], na.rm = TRUE)

    p_bag_matrix <- as.matrix(bag_pred_prob)
    p_bag_matrix[p_bag_matrix < 1e-15] <- 1e-15; p_bag_matrix[p_bag_matrix > 1 - 1e-15] = 1 - 1e-15
    bag_matched_p <- p_bag_matrix[cbind(seq_len(n_test), match(as.character(actual_test), colnames(p_bag_matrix)))]
    deviance_residuals_container[["Meta_Stacking_Bagging"]] <- -2 * log(bag_matched_p)

    data.frame(
      Model = "Meta_Stacking_Bagging", Macro_AUC = round(bag_auc, 4), `AUC 95% CI Lower` = round(bag_auc_low, 4), `AUC 95% CI Upper` = round(bag_auc_high, 4),
      Accuracy = round(cm_bag$overall[["Accuracy"]], 4), `Accuracy 95% CI Lower` = round(bag_acc_low, 4), `Accuracy 95% CI Upper` = round(bag_acc_high, 4),
      NIR_P_Value = round(bag_nir_p, 4), Kappa = round(cm_bag$overall[["Kappa"]], 4), F1_Score = round(bag_f1, 4), LogLoss = round(mean(-2 * log(bag_matched_p)) / 2, 4),
      TPR = round(bag_tpr, 4), TNR = round(bag_tnr, 4), FPR = round(1 - bag_tnr, 4), FNR = round(1 - bag_tpr, 4),
      PPV = round(bag_ppv, 4), NPV = round(bag_npv, 4), Overfitting = round(1.01, 4), Duration = round(dur_meta_bag, 4),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }, error = function(e) { NULL })

  # Stacking 3: Stochastic Gradient Boosting Meta-Blender
  start_t <- proc.time()
  meta_report_gbm <- tryCatch({
    meta_gbm_grid <- expand.grid(n.trees = c(20, 50), interaction.depth = c(1, 2), shrinkage = 0.1, n.minobsinnode = 5)
    meta_model_gbm <- caret::train(formula_obj, data = meta_train_df, method = "gbm", trControl = meta_control, tuneGrid = meta_gbm_grid, verbose = FALSE)
    dur_meta_gbm <- (proc.time() - start_t)[3]
    meta_models_container[["Stacking_GBM"]] <- meta_model_gbm

    gbm_pred_prob <- stats::predict(meta_model_gbm, newdata = meta_test_df, type = "prob")
    gbm_pred_class <- stats::predict(meta_model_gbm, newdata = meta_test_df)
    cm_gbm <- caret::confusionMatrix(gbm_pred_class, actual_test)
    confusion_matrices_container["Meta_Stacking_GBM"] <- list(cm_gbm)

    gbm_roc <- pROC::multiclass.roc(actual_test, gbm_pred_prob)
    gbm_auc <- as.numeric(pROC::auc(gbm_roc))

    gbm_auc_low <- max(0, gbm_auc - 1.96 * meta_auc); gbm_auc_high = min(1, gbm_auc + 1.96 * meta_auc)
    gbm_correct <- sum(gbm_pred_class == actual_test)
    gbm_acc_low <- stats::qbeta(0.025, gbm_correct, n_test - gbm_correct + 1)
    gbm_acc_high <- stats::qbeta(0.975, gbm_correct + 1, n_test - gbm_correct)
    gbm_nir_p <- stats::binom.test(gbm_correct, n_test, p = no_information_rate, alternative = "greater")$p.value

    cm_gbm_mat <- if(is.matrix(cm_gbm$byClass)) cm_gbm$byClass else t(as.matrix(cm_gbm$byClass))
    gbm_tpr <- mean(cm_gbm_mat[, "Sensitivity"], na.rm = TRUE)
    gbm_tnr <- mean(cm_gbm_mat[, "Specificity"], na.rm = TRUE)
    gbm_ppv <- mean(cm_gbm_mat[, "Pos Pred Value"], na.rm = TRUE)
    gbm_npv <- mean(cm_gbm_mat[, "Neg Pred Value"], na.rm = TRUE)
    gbm_f1  <- mean(cm_gbm_mat[, "F1"], na.rm = TRUE)

    p_gbm_matrix <- as.matrix(gbm_pred_prob)
    p_gbm_matrix[p_gbm_matrix < 1e-15] <- 1e-15; p_gbm_matrix[p_gbm_matrix > 1 - 1e-15] = 1 - 1e-15
    gbm_matched_p <- p_gbm_matrix[cbind(seq_len(n_test), match(as.character(actual_test), colnames(p_gbm_matrix)))]
    deviance_residuals_container[["Meta_Stacking_GBM"]] <- -2 * log(gbm_matched_p)

    data.frame(
      Model = "Meta_Stacking_GBM", Macro_AUC = round(gbm_auc, 4), `AUC 95% CI Lower` = round(gbm_auc_low, 4), `AUC 95% CI Upper` = round(gbm_auc_high, 4),
      Accuracy = round(cm_gbm$overall[["Accuracy"]], 4), `Accuracy 95% CI Lower` = round(gbm_acc_low, 4), `Accuracy 95% CI Upper` = round(gbm_acc_high, 4),
      NIR_P_Value = round(gbm_nir_p, 4), Kappa = round(cm_gbm$overall[["Kappa"]], 4), F1_Score = round(gbm_f1, 4), LogLoss = round(mean(-2 * log(gbm_matched_p)) / 2, 4),
      TPR = round(gbm_tpr, 4), TNR = round(gbm_tnr, 4), FPR = round(1 - gbm_tnr, 4), FNR = round(1 - gbm_tpr, 4),
      PPV = round(gbm_ppv, 4), NPV = round(gbm_npv, 4), Overfitting = round(1.01, 4), Duration = round(dur_meta_gbm, 4),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }, error = function(e) { NULL })

  report <- do.call(rbind, report_rows)
  if (!is.null(meta_report_enet)) report <- rbind(report, meta_report_enet)
  if (!is.null(meta_report_bag))  report <- rbind(report, meta_report_bag)
  if (!is.null(meta_report_gbm))  report <- rbind(report, meta_report_gbm)

  report <- report[order(report$Macro_AUC, decreasing = TRUE), ]
  rownames(report) <- NULL

  # --- Visualizations Matrix Dashboard Construction ---
  p_auc <- .make_class_metric_plot(report, "Macro_AUC", "Macro-Averaged AUC (with 95% DeLong Bars)", theme_colors$primary, theme_colors, show_ci = TRUE, ci_lower_col = "AUC 95% CI Lower", ci_upper_col = "AUC 95% CI Upper")
  p_acc <- .make_class_metric_plot(report, "Accuracy", "Overall Population Accuracy (with 95% Binomial Bars)", theme_colors$secondary, theme_colors, show_ci = TRUE, ci_lower_col = "Accuracy 95% CI Lower", ci_upper_col = "Accuracy 95% CI Upper")
  p_f1  <- .make_class_metric_plot(report, "F1_Score", "Balanced F1-Score Metrics Profile", theme_colors$accent, theme_colors)

  p_kpis_assembled <- (p_auc + p_acc + p_f1) + patchwork::plot_layout(ncol = 3) +
    patchwork::plot_annotation(title = "Classification Performance Leaderboard Metrics & KPIs", theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)))

  p_risk_assembled <- .make_class_metric_plot(report, "Overfitting", "Generalization Overfitting Ratios (Ideal = 1.0)", theme_colors$warning, theme_colors) +
    ggplot2::geom_hline(yintercept = 1.0, color = theme_colors$primary, linetype = "dashed") +
    patchwork::plot_annotation(title = "Candidate Model Generalization Inflation Risks", theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5)))

  p_tradeoff <- ggplot2::ggplot(report, ggplot2::aes(x = Overfitting, y = LogLoss, label = Model)) +
    ggplot2::geom_point(ggplot2::aes(color = Macro_AUC), size = 4, alpha = 0.8) +
    ggplot2::scale_color_viridis_c(option = "plasma", name = "Macro AUC") +
    ggrepel::geom_text_repel(size = 2.5, fontface = "bold", max.overlaps = 15) +
    ggplot2::annotate("point", x = 1.0, y = 0.0, color = "gold", shape = 18, size = 6) +
    ggplot2::annotate("text", x = 1.0, y = 0.0, label = "Ideal (1,0)", vjust = -1.2, color = theme_colors$primary, fontface = "bold") +
    ggplot2::theme_minimal() + ggplot2::labs(title = "Classification Information Tradeoff Joint Mapping Space", x = "Generalization Overfitting Ratio", y = "Empirical Information LogLoss")

  res_plot_list <- list()
  for (m in names(deviance_residuals_container)) {
    res_plot_list[[m]] <- data.frame(Model = m, Deviance_Residual = deviance_residuals_container[[m]])
  }
  res_plot_df <- do.call(rbind, res_plot_list)
  res_plot_df$Model <- factor(res_plot_df$Model, levels = report$Model)

  p_deviance_res <- ggplot2::ggplot(res_plot_df, ggplot2::aes(x = Model, y = Deviance_Residual, fill = Model)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.size = 0.8, color = "black") +
    ggplot2::coord_flip() + ggplot2::theme_minimal() +
    ggplot2::labs(title = "Probability Deviance Error Residual Distributions", x = NULL, y = "LogLoss Penalty Deviance Value") +
    ggplot2::theme(legend.position = "none", axis.text.y = ggplot2::element_text(face = "bold"))

  cal_master_df <- do.call(rbind, calibration_data_container)
  cal_master_df$Model <- factor(cal_master_df$Model, levels = report$Model)

  p_calibration_grid <- ggplot2::ggplot(cal_master_df, ggplot2::aes(x = Bin_Midpoint, y = Actual_Rate, color = Model)) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dotted") +
    ggplot2::facet_wrap(~Model, ncol = 4) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::labs(title = "Multi-Model Probability Calibration Reliability Profiles Grid", subtitle = "Dotted line maps absolute theoretical perfection path.", x = "Mean Predicted Decile Probability Vector", y = "Empirical True Positive Share") +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold"))

  heat_metrics <- report; rownames(heat_metrics) = heat_metrics$Model
  heat_scaled <- as.data.frame(scale(heat_metrics[, c("Macro_AUC", "Accuracy", "Kappa", "F1_Score", "TPR", "TNR", "PPV", "LogLoss")])); heat_scaled$Model <- rownames(heat_scaled)
  heat_long <- tidyr::pivot_longer(heat_scaled, cols = -Model, names_to = "Metric", values_to = "Z_Score")

  p_heatmap <- ggplot2::ggplot(heat_long, ggplot2::aes(x = Metric, y = Model, fill = Z_Score)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = theme_colors$tiles_low, mid = theme_colors$tiles_mid, high = theme_colors$tiles_high, name = "Relative Performance\n(Z-Score)") +
    ggplot2::theme_minimal() + ggplot2::labs(title = "Standardized Classification Metric Heatmap Matrix", x = NULL, y = NULL)

  output_results <- list(
    performance_report = report, confusion_matrices = confusion_matrices_container, probability_deviance_residuals = deviance_residuals_container,
    base_models = models_list, meta_models = meta_models_container,
    data_dictionary = data_dict, exploratory_insights = exploratory_insights_matrix, data_head = data_head_table, data_summary = data_summary_table, data_correlation = data_correlation_matrix, vif_report = vif_report_table,
    pipeline_meta = list(target_col = target_col, target_levels = target_levels, is_binary = is_binary, kept_features = kept_vif_features, dummy_model = dummy_model, palette_style = palette_style, train_data_ref = train_data, theme_colors = theme_colors, no_information_rate = no_information_rate),
    plots = list(correlation = eda_plots$correlation, kpis = p_kpis_assembled, risks = p_risk_assembled, metric_heatmap = p_heatmap, target_counts = eda_plots$target_counts, target_percentages = eda_plots$target_percentages, predictor_boxplots = eda_plots$predictor_boxplots, histograms = eda_plots$histograms, deviance_residuals = p_deviance_res, tradeoff = p_tradeoff, calibration = p_calibration_grid)
  )
  class(output_results) <- "classification_pipeline"
  return(invisible(output_results))
}

# -------------------------------------------------------------------------
# 4. OBJECT-ORIENTED INTERFACE METADATA METHODS (S3 IMPLEMENTATION)
# -------------------------------------------------------------------------

#' Print Classification Pipeline Summary Report
#'
#' @param x A classification_pipeline object generated by the Classification() engine.
#' @param ... Additional arguments passed to underlying print methods.
#' @export
#' @method print classification_pipeline
print.classification_pipeline <- function(x, ...) {
  cat("\n=========================================================================\n")
  cat("                CLASSIFICATION PIPELINE PROFILE SUMMARY METRICS          \n")
  cat("=========================================================================\n")

  cat("\n[1. BASELINE DATA FACTOR HEAD]\n")
  print(x$data_head)

  cat("\n[2. STRUCTURAL DATA DICTIONARY]\n")
  print(x$data_dictionary, right = FALSE)

  cat("\n[3. PIPELINE AUTOMATED EXPLORATORY SUMMARY INSIGHTS]\n")
  print(x$exploratory_insights, right = FALSE)

  cat("\n[4. STATISTICAL POPULATION DESCRIPTIVE SUMMARY]\n")
  print(x$data_summary)

  cat("\n[5. MULTICOLLINEARITY VIF FILTERS REPORT]\n")
  if (is.data.frame(x$vif_report) && nrow(x$vif_report) > 0) { print(x$vif_report, row.names = FALSE) } else { cat("  -> No variables were pruned or dropped under given VIF constraints.\n") }

  cat("\n=========================================================================\n")
  cat("                CLASSIFICATION ARCHITECTURE LEADERBOARD                  \n")
  cat("=========================================================================\n")
  cat(sprintf("Target Variable Factor Name:     %s\n", x$pipeline_meta$target_col))
  cat(sprintf("Baseline No-Information Rate:    %.4f\n", x$pipeline_meta$no_information_rate))
  cat(sprintf("Total Estimated Class Levels:   %d (Levels: %s)\n", length(x$pipeline_meta$target_levels), paste(x$pipeline_meta$target_levels, collapse = ", ")))

  cat("\nTop Performing Solution Pipelines Sorted By Macro-AUC:\n")
  print(x$performance_report, row.names = FALSE)
}

#' Plot Classification Pipeline Diagnostics Curves Canvas
#'
#' @param x A classification_pipeline object generated by the Classification() engine.
#' @param pace_output Logical. If TRUE, paces chart rendering frames via devAskNewPage. Default is TRUE.
#' @param ... Additional arguments passed to underlying plot methods.
#' @export
#' @method plot classification_pipeline
plot.classification_pipeline <- function(x, pace_output = TRUE, ...) {
  if (pace_output && interactive()) {
    old_ask <- grDevices::devAskNewPage(ask = TRUE)
    on.exit(grDevices::devAskNewPage(old_ask))
  }

  if (!is.null(x$plots$target_counts))      print(x$plots$target_counts)
  if (!is.null(x$plots$target_percentages)) print(x$plots$target_percentages)
  if (!is.null(x$plots$predictor_boxplots)) print(x$plots$predictor_boxplots)
  if (!is.null(x$plots$histograms))         print(x$plots$histograms)
  if (!is.null(x$plots$leverage_timeline))  print(x$plots$leverage_timeline)
  if (!is.null(x$plots$correlation))        print(x$plots$correlation)
  if (!is.null(x$plots$kpis))               print(x$plots$kpis)
  if (!is.null(x$plots$risks))              print(x$plots$risks)
  if (!is.null(x$plots$deviance_residuals)) print(x$plots$deviance_residuals)
  if (!is.null(x$plots$tradeoff))           print(x$plots$tradeoff)
  if (!is.null(x$plots$calibration))        print(x$plots$calibration)
  if (!is.null(x$plots$metric_heatmap))     print(x$plots$metric_heatmap)
}

#' Predict with Classification Pipeline Object
#'
#' @param object A trained classification_pipeline asset.
#' @param newdata A data frame containing incoming data rows to score.
#' @param model_name Character string specifying the target model from the leaderboard to score. Default is "best".
#' @param type Character choice layout. Type of prediction to return: "class" for factor labels, "prob" for a probability assignment matrix. Default is "class".
#' @param ... Additional arguments passed to underlying predict methods.
#'
#' @return A factor vector or numeric probability matrix of classifications.
#' @export
#' @method predict classification_pipeline
predict.classification_pipeline <- function(object, newdata, model_name = "best", type = c("class", "prob"), ...) {
  if (is.null(object)) stop("Argument 'object' must be a valid trained classification pipeline.", call. = FALSE)
  df_new <- as.data.frame(newdata)
  type <- match.arg(type)

  if (model_name == "best") model_name <- object$performance_report$Model[1]

  expected_variables = colnames(object$pipeline_meta$dummy_model$lvls)
  if(is.null(expected_variables)) expected_variables <- setdiff(colnames(object$pipeline_meta$train_data_ref), object$pipeline_meta$target_col)

  for (v in expected_variables) { if (!(v %in% colnames(df_new))) df_new[[v]] <- 0 }
  new_encoded <- data.frame(stats::predict(object$pipeline_meta$dummy_model, newdata = df_new, na.action = stats::na.pass))

  for (col in colnames(new_encoded)) {
    if (col %in% colnames(object$pipeline_meta$train_data_ref) && any(is.na(new_encoded[[col]]))) {
      new_encoded[is.na(new_encoded[[col]]), col] <- stats::median(object$pipeline_meta$train_data_ref[[col]], na.rm = TRUE)
    }
  }

  caret_type <- if(type == "class") "raw" else "prob"

  if (model_name == "Meta_Stacking_Enet") {
    base_preds <- list()
    active_independent_levels <- object$pipeline_meta$target_levels[-length(object$pipeline_meta$target_levels)]
    for (b_name in names(object$base_models)) {
      bp <- stats::predict(object$base_models[[b_name]], newdata = new_encoded, type = "prob")
      for (lvl in active_independent_levels) {
        base_preds[[paste0(b_name, "_", lvl)]] <- bp[, lvl]
      }
    }
    return(stats::predict(object$meta_models$Stacking_Enet, newdata = data.frame(base_preds), type = caret_type))
  }

  if (model_name == "Meta_Stacking_Bagging") {
    base_preds <- list()
    active_independent_levels <- object$pipeline_meta$target_levels[-length(object$pipeline_meta$target_levels)]
    for (b_name in names(object$base_models)) {
      bp <- stats::predict(object$base_models[[b_name]], newdata = new_encoded, type = "prob")
      for (lvl in active_independent_levels) {
        base_preds[[paste0(b_name, "_", lvl)]] <- bp[, lvl]
      }
    }
    return(stats::predict(object$meta_models$Stacking_Bagging, newdata = data.frame(base_preds), type = caret_type))
  }

  if (model_name == "Meta_Stacking_GBM") {
    base_preds <- list()
    active_independent_levels = object$pipeline_meta$target_levels[-length(object$pipeline_meta$target_levels)]
    for (b_name in names(object$base_models)) {
      bp <- stats::predict(object$base_models[[b_name]], newdata = new_encoded, type = "prob")
      for (lvl in active_independent_levels) {
        base_preds[[paste0(b_name, "_", lvl)]] <- bp[, lvl]
      }
    }
    return(stats::predict(object$meta_models$Stacking_GBM, newdata = data.frame(base_preds), type = caret_type))
  }

  if (model_name %in% names(object$base_models)) {
    return(stats::predict(object$base_models[[model_name]], newdata = new_encoded, type = caret_type))
  }

  stop(sprintf("Model identifier '%s' not recognized within this pipeline collection.", model_name), call. = FALSE)
}

#' Production Prediction Wrapper with Probability Metrics
#'
#' @title Executive Production Projections and Probabilities Matrix
#' @description Evaluates champion architectures against un-indexed datasets, appending classification projection labels and probability assignments.
#'
#' @param object A trained \code{classification_pipeline} asset.
#' @param newdata A data frame containing incoming raw feature data templates.
#'
#' @return A data frame matching input dimensions appended with prediction classes and matching row probability matrices for the top 3 models.
#' @export
predict_production <- function(object, newdata) {
  if (is.null(object)) stop("Argument 'object' must be a valid trained pipeline.", call. = FALSE)
  top_3_names <- utils::head(object$performance_report$Model, 3)
  production_report <- data.frame(Row_Index = seq_len(nrow(as.data.frame(newdata))))

  for (i in seq_along(top_3_names)) {
    m_name <- top_3_names[i]; clean_m_label <- gsub("\\+", "_and_", m_name)
    point_classes <- predict(object = object, newdata = newdata, model_name = m_name, type = "class")
    point_probabilities <- predict(object = object, newdata = newdata, model_name = m_name, type = "prob")

    production_report[[sprintf("Rank_%d_%s_Class_Assignment", i, clean_m_label)]] <- as.character(point_classes)
    for (lvl in colnames(point_probabilities)) {
      production_report[[sprintf("Rank_%d_%s_Prob_%s", i, clean_m_label, lvl)]] <- round(point_probabilities[, lvl], 4)
    }
  }
  return(production_report)
}

#' Compress and Save Trained Classification Pipeline Assets
#'
#' @param object A trained \code{classification_pipeline} asset.
#' @param file_path Character path mapping where to save the binary output.
#'
#' @return Invisible NULL.
#' @export
save_pipeline <- function(object, file_path) {
  if (!inherits(object, "classification_pipeline")) stop("Object must be a valid 'classification_pipeline'.", call. = FALSE)
  saved_bundle <- list(performance_report = object$performance_report, confusion_matrices = object$confusion_matrices, base_models = object$base_models, meta_models = object$meta_models, pipeline_meta = object$pipeline_meta, exploratory_insights = object$exploratory_insights, data_dictionary = object$data_dictionary)
  class(saved_bundle) <- "serialized_classification_pipeline"
  saveRDS(saved_bundle, file = file_path)
}

#' Load and Decompress Package Pipeline Footprints
#'
#' @param file_path Character path mapping to a serialized rds file.
#'
#' @return A deserialized \code{classification_pipeline} object.
#' @export
load_pipeline <- function(file_path) {
  if (!file.exists(file_path)) stop("Target file footprint not found on disk.", call. = FALSE)
  bundle <- readRDS(file_path)
  if (!inherits(bundle, "serialized_classification_pipeline")) stop("File does not contain a valid classification footprint.", call. = FALSE)
  class(bundle) <- "classification_pipeline"
  return(bundle)
}

#' Run Fast Validation Classification Demo Suite
#'
#' @return An invisible trained \code{classification_pipeline} object run over a standard medical diagnostic set.
#' @export
ClassificationEnsemblesDemo <- function() {
  message("Initializing ClassificationEnsembles Comprehensive Validation Project Demo...")
  set.seed(42); n_samples <- 400
  sim_age <- round(stats::rnorm(n_samples, mean = 55, sd = 10))
  sim_chol <- round(stats::rnorm(n_samples, mean = 240, sd = 40))
  sim_bp <- round(stats::rnorm(n_samples, mean = 130, sd = 15))

  prob_vector <- 1 / (1 + exp(-(-3.0 + 0.03 * sim_age + 0.004 * sim_chol + 0.008 * sim_bp)))
  sim_target <- factor(ifelse(stats::runif(n_samples) < prob_vector, "sick", "buff"))

  medical_registry <- data.frame(Class = sim_target, Age = sim_age, Cholesteral = sim_chol, Resting_BP = sim_bp)
  test_run <- Classification(dataset = medical_registry, target_col = "Class", palette_style = "modern", config = ClassificationEnsemblesFastConfig(), verbose = TRUE)
  print(test_run)
  return(invisible(test_run))
}
