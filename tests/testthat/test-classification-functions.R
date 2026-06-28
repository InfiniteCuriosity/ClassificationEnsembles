test_that("ClassificationConfig matrices build parameter constraints successfully", {
  cfg <- ClassificationConfig(rpart_tune = 3, rf_tune = 4)
  expect_type(cfg, "list")
  expect_equal(cfg$rpart_tune, 3)
  expect_equal(cfg$rf_tune, 4)

  fast_cfg <- ClassificationFastConfig()
  expect_equal(fast_cfg$rpart_tune, 1)
})

test_that("Classification master engine trains base models and generates S3 bundle lists", {
  set.seed(42)
  n_mock <- 140 # Increased size for robust cross-validation fold balance

  # Center distributions to guarantee a well-balanced target class split
  f1 <- rnorm(n_mock, mean = 0, sd = 1.2)
  f2 <- rnorm(n_mock, mean = 0, sd = 1.2)
  prob <- 1 / (1 + exp(-(1.2 * f1 - 1.2 * f2)))
  y_labels <- factor(ifelse(prob > 0.5, "Success", "Failure"), levels = c("Success", "Failure"))

  mock_df <- data.frame(Status = y_labels, Predictor_A = f1, Predictor_B = f2)

  pipeline_res <- Classification(
    dataset = mock_df,
    target_col = "Status",
    cv_folds = 2,
    train_pct = 0.70,
    vif_threshold = 10,
    sampling_method = "none",
    palette_style = "standard",
    config = ClassificationFastConfig(),
    verbose = FALSE
  )

  expect_s3_class(pipeline_res, "classification_pipeline")
  expect_true(is.data.frame(pipeline_res$performance_report))
  expect_true(is.matrix(pipeline_res$confusion_matrices[[1]]))
})

test_that("predict and predict_production S3 paths emit accurate deployment spreadsheets", {
  set.seed(42)
  n_mock <- 100
  mock_df <- data.frame(
    Status = factor(rbinom(n_mock, 1, 0.5), labels = c("Pass", "Fail")),
    Var_A = rnorm(n_mock),
    Var_B = runif(n_mock)
  )
  pipeline_res <- Classification(dataset = mock_df, target_col = "Status", config = ClassificationFastConfig(), verbose = FALSE)

  new_data <- data.frame(Var_A = c(0.8, -1.1), Var_B = c(0.4, 0.9))

  # Check point predictions
  class_preds <- predict(pipeline_res, newdata = new_data, model_name = "best", type = "class")
  expect_s3_class(class_preds, "factor")
  expect_length(class_preds, 2)

  # Check executive production spreadsheets dynamically based on the winning champion model
  executive_sheet <- predict_production(pipeline_res, newdata = new_data)
  expect_true(is.data.frame(executive_sheet))
  expect_equal(nrow(executive_sheet), 2)

  champion_model <- pipeline_res$performance_report$Model[1]
  clean_m_label  <- gsub("\\+", "_and_", champion_model)
  expected_col   <- sprintf("Rank_1_%s_Class", clean_m_label)

  expect_true(expected_col %in% colnames(executive_sheet))
})

test_that("ExportClassificationResults outputs matrices and graphics to disk locations", {
  set.seed(42)
  n_mock <- 80
  mock_df <- data.frame(Status = factor(rbinom(n_mock, 1, 0.5), labels = c("A", "B")), Var_A = rnorm(n_mock))
  pipeline_res <- Classification(dataset = mock_df, target_col = "Status", config = ClassificationFastConfig(), verbose = FALSE)

  sandbox_dir <- file.path(tempdir(), "classification_exports_sandbox")

  # Suppress graphical scale warnings from sparse mock metrics data rows during testing
  suppressWarnings({
    ExportClassificationResults(pipeline_object = pipeline_res, export_directory = sandbox_dir)
  })

  expect_true(file.exists(file.path(sandbox_dir, "performance_report.csv")))
  expect_true(file.exists(file.path(sandbox_dir, "confusion_matrices_audit.txt")))
  expect_true(file.exists(file.path(sandbox_dir, "09_faceted_roc_matrix.png")))

  unlink(sandbox_dir, recursive = TRUE)
})
