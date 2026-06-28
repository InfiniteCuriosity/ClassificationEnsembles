test_that(".initialize_pipeline_data correctly builds clean target factor levels", {
  # 1. Setup raw mock data with messy string labels
  mock_data <- data.frame(
    Risk_Tier = c("High Risk", "Low Risk", "High Risk", "Medium Risk", "Low Risk"),
    Feature_1 = c(2.5, 3.1, 1.2, 5.4, 4.1)
  )

  # 2. Ingest via the internal data sub-engine using triple colons
  processed <- ClassificationEnsembles:::.initialize_pipeline_data(
    dataset = mock_data,
    target_col = "Risk_Tier",
    train_pct = 0.60,
    stratify_col = NULL,
    verbose = FALSE
  )

  # 3. Guardrail Assertions
  expect_s3_class(processed$df$Risk_Tier, "factor")
  # Verifies that illegal characters are programmatically cleaned via make.names
  expect_equal(levels(processed$df$Risk_Tier), c("High.Risk", "Low.Risk", "Medium.Risk"))
})

test_that(".filter_pipeline_vif prunes perfectly collinear category indicators", {
  # Setup an evaluation matrix where Feature_3 has a near-perfect correlation with Feature_2.
  # Placing Feature_3 before Feature_2 ensures it breaks the tie-breaker inside which.max()
  # and is cleanly selected for pruning.
  mock_train <- data.frame(
    Target = factor(c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")),
    Feature_1 = c(1.0, 5.0, 2.0, 8.0, 3.0, 14.0, 7.0, 11.0),
    Feature_3 = c(1.01, 1.99, 3.02, 3.98, 5.01, 5.99, 7.02, 7.98),
    Feature_2 = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)
  )

  vif_filtered <- ClassificationEnsembles:::.filter_pipeline_vif(
    train_data = mock_train,
    test_data = mock_train,
    target_col = "Target",
    vif_threshold = 5,
    verbose = FALSE
  )

  expect_true(is.data.frame(vif_filtered$train_data))
  expect_true("Feature_1" %in% colnames(vif_filtered$train_data))
  expect_false("Feature_3" %in% colnames(vif_filtered$train_data)) # Feature_3 will now be cleanly dropped!
})
