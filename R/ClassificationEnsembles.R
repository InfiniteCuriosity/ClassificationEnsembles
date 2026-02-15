#' classification—function to perform classification analysis and return results to the user.

#' @param data a data set that includes classification data. For example, the Carseats data in the ISLR package
#' @param colnum the number of the column. For example, in the Carseats data this is column 7, ShelveLoc with three values, Good, Medium and Bad
#' @param numresamples the number of times to resample the analysis
#' @param how_to_handle_strings Option to convert strings to factor levels
#' @param set_seed Option to set a seed
#' @param predict_on_new_data asks if the user has new data to be analyzed using the trained models that were just developed
#' @param remove_VIF_above Removes columns with Variance Inflation Factors above the level chosen by the user
#' @param scale_all_numeric_predictors_in_data Scales all numeric predictors in the original data
#' @param save_all_trained_models Gives the user the option to save all trained models in the Environment
#' @param save_all_plots Saves all plots in the user's chosen format
#' @param stratified_random_column 0 if no stratified random sampling, or column number for stratified random sampling
#' @param use_parallel "Y" or "N" for parallel processing
#' @param train_amount set the amount for the training data
#' @param test_amount set the amount for the testing data
#' @param validation_amount Set the amount for the validation data

#' @returns a full analysis, including data visualizations, statistical summaries, and a full report on the results of 35 models on the data
#' @export Classification


#' @importFrom C50 C5.0
#' @importFrom car vif
#' @importFrom caret confusionMatrix
#' @importFrom corrplot corrplot
#' @importFrom dplyr across count mutate relocate select
#' @importFrom doParallel registerDoParallel
#' @importFrom e1071 svm
#' @importFrom ggplot2 geom_boxplot geom_histogram ggplot facet_wrap labs theme_bw labs aes
#' @importFrom graphics hist pairs panel.smooth par rect
#' @importFrom gt gt
#' @importFrom ipred bagging
#' @importFrom MachineShop fit
#' @importFrom magrittr %>%
#' @importFrom parallel makeCluster
#' @importFrom pls plsr
#' @importFrom purrr keep
#' @importFrom randomForest randomForest
#' @importFrom ranger ranger
#' @importFrom reactable reactable
#' @importFrom reactablefmtr add_title
#' @importFrom scales percent
#' @importFrom stats complete.cases cor predict reorder sd
#' @importFrom tidyr gather last_col pivot_longer
#' @importFrom tree tree cv.tree prune.misclass
#' @importFrom utils head read.csv str

#### Function definition ####
Classification <- function(data, colnum, numresamples, predict_on_new_data = c("Y", "N"), remove_VIF_above, scale_all_numeric_predictors_in_data,
                           how_to_handle_strings = c(0("No strings"), 1("Strings as factors")), set_seed = c("Y", "N"), save_all_trained_models = c("Y", "N"),
                           save_all_plots, stratified_random_column, use_parallel = c("Y", "N"), train_amount, test_amount, validation_amount) {

#### Set initial values to 0 ####
use_parallel <- 0
no_cores <- 0

if(set_seed == "Y"){
  seed = as.integer(readline("Which integer would you like to use for the seed? "))
}

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}

if(stratified_random_column > 0) {
  levels <- levels(as.factor((data[, stratified_random_column]))) # gets the levels for stratified data
}

y <- 0
colnames(data)[colnum] <- "y"

df <- data %>% dplyr::relocate(y, .after = tidyr::last_col()) # Moves the target column to the last column on the right
df <- df[sample(nrow(df)), ]

VIF <- car::vif(stats::lm(as.numeric(df$y) ~ ., data = df[, 1:ncol(df)]))
for (i in 1:ncol(df)) {
  if(max(VIF) > remove_VIF_above){
    df <- df %>% dplyr::select(-which.max(VIF))
    VIF <- car::vif(stats::lm(as.numeric(df$y) ~ ., data = df[, 1:ncol(df)]))
  }
}


VIF <- reactable::reactable(as.data.frame(VIF),
                            searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                            striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Variance Inflation Factor")

head_df <- head(df) %>% dplyr::mutate_if(is.numeric, round, digits = 4)

head_df <- reactable::reactable(head_df,
                                searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Head of the data frame")

data_summary <- summary(df)

#### Save all plots ####

if(save_all_plots == "Y"){
  reactablefmtr::save_reactable_test(head_df, "Head_of_the_data_frame.html")
}

if(save_all_plots == "Y"){
  width = as.numeric(readline("Width of the graphics: "))
  height = as.numeric(readline("Height of the graphics: "))
  units = readline("Which units? You may use in, cm, mm or px. ")
  scale = as.numeric(readline("What multiplicative scaling factor? "))
  device = readline("Which device to use? You may enter eps, jpeg, pdf, png, svg or tiff: ")
  dpi <- as.numeric(readline("Plot resolution. Applies only to raster output types (jpeg, png, tiff): "))
}

#### Barchart of the data against y ####
barchart <- df %>%
  dplyr::mutate(dplyr::across(-y, as.numeric)) %>%
  tidyr::pivot_longer(!y) %>%
  dplyr::summarise(dplyr::across(value, sum), .by = c(y, name)) %>%
  dplyr::mutate(perc = proportions(value), .by = c(name)) %>%
  ggplot2::ggplot(ggplot2::aes(x = y, y = value)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(aes(label = round(value, 4)),
                     vjust = -.5) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::facet_wrap(~ name, scales = "free") +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.25)))

barchart2 <- df %>%
  dplyr::mutate(dplyr::across(-y, as.numeric)) %>%
  tidyr::pivot_longer(!y) %>%
  dplyr::summarise(dplyr::across(value, sum), .by = c(y, name)) %>%
  dplyr::mutate(perc = proportions(value), .by = c(name)) %>%
  ggplot2::ggplot(ggplot2::aes(x = y, y = value)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(aes(label = scales::percent(round(perc, 3)),
                         vjust = -0.5)) +
  ggplot2::facet_wrap(~ name, scales = "free") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.25)))


if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("barchart.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("barchart.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("barchart.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("barchart.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("barchart.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("barchart.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

data_summary <- summary(df)

head_data <- head(df)

#### Correlation plot of numeric data ####
df1 <- df %>% purrr::keep(is.numeric)
M1 <- cor(df1)
title <- "Correlation plot of the numerical data"
corrplot::corrplot(M1, method = "number", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow.com/a/14754408/54964)
corrplot::corrplot(M1, method = "circle", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow.com/a/14754408/54964)

#### Print correlation matrix of numeric data ####
correlation_marix <- reactable::reactable(round(cor(df1), 4),
                                          searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                          striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Correlation of the data")

if(save_all_plots == "Y"){
  reactablefmtr::save_reactable_test(correlation_marix, "Correlation_matrix.html")
}

#### Boxplots of the numeric data ####
boxplots <- df1 %>%
  tidyr::gather(key = "var", value = "value") %>%
  ggplot2::ggplot(ggplot2::aes(x = "", y = value)) +
  ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggplot2::facet_wrap(~var, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Boxplots of the numeric data")
# Thanks to https://rstudio-pubs-static.s3.amazonaws.com/388596_e21196f1adf04e0ea7cd68edd9eba966.html

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("boxplots.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("boxplots.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("boxplots.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("boxplots.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("boxplots.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("boxplots.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Histograms of the numeric data ####
histograms <- ggplot2::ggplot(tidyr::gather(df1, cols, value), ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = round(nrow(df1) / 10)) +
  ggplot2::facet_wrap(. ~ cols, scales = "free") +
  ggplot2::labs(title = "Histograms of each numeric column. Each bar = 10 rows of data")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("histograms.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("histograms.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("histograms.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("histograms.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("histograms.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("histograms.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}

#### How to handle strings, move target value to the last column on the right ####

y <- 0
colnames(data)[colnum] <- "y"

df <- data %>% dplyr::relocate(y, .after = tidyr::last_col()) # Moves the target column to the last column on the right
df <- df[sample(nrow(df)), ]

if (how_to_handle_strings == 1) {
  df <- dplyr::mutate_if(df, is.character, as.factor)
}

if (how_to_handle_strings == 2) {
  df <- dplyr::mutate_if(df, is.character, as.factor)
  df <- dplyr::mutate_if(df, is.factor, as.numeric)
}

if (predict_on_new_data == "Y") {
  new_data <- readline("What is the URL of the new data? ")
  new_data <- read.csv(new_data, stringsAsFactors = TRUE)

  y <- 0
  colnames(new_data)[colnum] <- "y"

  new_data <- new_data %>% dplyr::relocate(y, .after = tidyr::last_col()) # Moves the target column to the last column on the right
}

if (predict_on_new_data == "Y") {
  new_data <- readline("What is the URL of the new data? ")
  new_data <- read.csv(new_data, stringsAsFactors = TRUE)

  y <- 0
  colnames(new_data)[colnum] <- "y"

  new_data <- new_data %>% dplyr::relocate(y, .after = tidyr::last_col()) # Moves the target column to the last column on the right
}

if (scale_all_numeric_predictors_in_data == "Y"){
  df <- as.data.frame(scale(df[, 1:ncol(df) -1]) %>% cbind(y = df$y))
}


#### Set initial values to zero ####

bagging_train_accuracy <- 0
bagging_test_accuracy <- 0
bagging_validation_accuracy <- 0
bagging_holdout_vs_train <- 0
bagging_holdout <- 0
bagging_residuals <- 0
bagging_duration <- 0
bagging_true_positive_rate <- 0
bagging_true_negative_rate <- 0
bagging_false_positive_rate <- 0
bagging_false_negative_rate <- 0
bagging_F1_score <- 0
bagging_table_total <- 0
bagging_classification_error <- 0
bagging_classification_error_mean <- 0
bagging_positive_pred_value <- 0
bagging_negative_pred_value <- 0
bagging_prevalence <- 0
bagging_detection_rate <- 0
bagging_detection_prevalence <- 0

bag_rf_train_accuracy <- 0
bag_rf_test_accuracy <- 0
bag_rf_validation_accuracy <- 0
bag_rf_holdout_vs_train <- 0
bag_rf_holdout <- 0
bag_rf_residuals <- 0
bag_rf_duration <- 0
bag_rf_true_positive_rate <- 0
bag_rf_true_negative_rate <- 0
bag_rf_false_positive_rate <- 0
bag_rf_false_negative_rate <- 0
bag_rf_F1_score <- 0
bag_rf_table_total <- 0
bag_rf_classification_error <- 0
bag_rf_classification_error_mean <- 0
bag_rf_positive_pred_value <- 0
bag_rf_negative_pred_value <- 0
bag_rf_prevalence <- 0
bag_rf_detection_rate <- 0
bag_rf_detection_prevalence <- 0

C50_train_accuracy <- 0
C50_test_accuracy <- 0
C50_validation_accuracy <- 0
C50_holdout_vs_train <- 0
C50_holdout <- 0
C50_residuals <- 0
C50_duration <- 0
C50_true_positive_rate <- 0
C50_true_negative_rate <- 0
C50_false_positive_rate <- 0
C50_false_negative_rate <- 0
C50_F1_score <- 0
C50_table_total <- 0
C50_classification_error <- 0
C50_classification_error_mean <- 0
C50_positive_pred_value <- 0
C50_negative_pred_value <- 0
C50_prevalence <- 0
C50_detection_rate <- 0
C50_detection_prevalence <- 0

linear_train_accuracy <- 0
linear_validation_accuracy <- 0
linear_test_accuracy <- 0
linear_test_accuracy_mean <- 0
linear_validation_accuracy_mean <- 0
linear_holdout_vs_train <- 0
linear_holdout <- 0
linear_residuals <- 0
linear_duration <- 0
linear_true_positive_rate <- 0
linear_true_negative_rate <- 0
linear_false_positive_rate <- 0
linear_false_negative_rate <- 0
linear_F1_score <- 0
linear_table_total <- 0
linear_classification_error <- 0
linear_classification_error_mean <- 0
linear_positive_pred_value <- 0
linear_negative_pred_value <- 0
linear_prevalence <- 0
linear_detection_rate <- 0
linear_detection_prevalence <- 0

n_bayes_train_accuracy <- 0
n_bayes_test_accuracy <- 0
n_bayes_validation_accuracy <- 0
n_bayes_accuracy <- 0
n_bayes_test_accuracy_mean <- 0
n_bayes_validation_accuracy_mean <- 0
n_bayes_holdout_vs_train <- 0
n_bayes_holdout <- 0
n_bayes_residuals <- 0
n_bayes_duration <- 0
n_bayes_true_positive_rate <- 0
n_bayes_true_negative_rate <- 0
n_bayes_false_positive_rate <- 0
n_bayes_false_negative_rate <- 0
n_bayes_F1_score <- 0
n_bayes_table_total <- 0
n_bayes_classification_error <- 0
n_bayes_classification_error_mean <- 0
n_bayes_positive_pred_value <- 0
n_bayes_negative_pred_value <- 0
n_bayes_prevalence <- 0
n_bayes_detection_rate <- 0
n_bayes_detection_prevalence <- 0

pls_train_accuracy <- 0
pls_test_accuracy <- 0
pls_test_accuracy_mean <- 0
pls_validation_accuracy <- 0
pls_validation_accuracy_mean <- 0
pls_holdout_vs_train <- 0
pls_holdout <- 0
pls_residuals <- 0
pls_duration <- 0
pls_true_positive_rate <- 0
pls_true_negative_rate <- 0
pls_false_positive_rate <- 0
pls_false_negative_rate <- 0
pls_F1_score <- 0
pls_table_total <- 0
pls_classification_error <- 0
pls_classification_error_mean <- 0
pls_positive_pred_value <- 0
pls_negative_pred_value <- 0
pls_prevalence <- 0
pls_detection_rate <- 0
pls_detection_prevalence <- 0

pda_train_accuracy <- 0
pda_test_accuracy <- 0
pda_test_accuracy_mean <- 0
pda_validation_accuracy <- 0
pda_validation_accuracy_mean <- 0
pda_holdout_vs_train <- 0
pda_holdout <- 0
pda_residuals <- 0
pda_duration <- 0
pda_true_positive_rate <- 0
pda_true_negative_rate <- 0
pda_false_positive_rate <- 0
pda_false_negative_rate <- 0
pda_F1_score <- 0
pda_table_total <- 0
pda_classification_error <- 0
pda_classification_error_mean <- 0
pda_positive_pred_value <- 0
pda_negative_pred_value <- 0
pda_prevalence <- 0
pda_detection_rate <- 0
pda_detection_prevalence <- 0

rf_train_accuracy <- 0
rf_test_accuracy <- 0
rf_test_accuracy_mean <- 0
rf_validation_accuracy <- 0
rf_validation_accuracy_mean <- 0
rf_holdout_vs_train <- 0
rf_holdout_vs_train_holdout <- 0
rf_holdout <- 0
rf_residuals <- 0
rf_duration <- 0
rf_true_positive_rate <- 0
rf_true_negative_rate <- 0
rf_false_positive_rate <- 0
rf_false_negative_rate <- 0
rf_F1_score <- 0
rf_table_total <- 0
rf_classification_error <- 0
rf_classification_error_mean <- 0
rf_positive_pred_value <- 0
rf_negative_pred_value <- 0
rf_prevalence <- 0
rf_detection_rate <- 0
rf_detection_prevalence <- 0

ranger_train_accuracy <- 0
ranger_test_accuracy <- 0
ranger_test_accuracy_mean <- 0
ranger_validation_accuracy <-
  ranger_validation_accuracy_mean <- 0
ranger_holdout_vs_train <- 0
ranger_holdout <- 0
ranger_residuals <- 0
ranger_duration <- 0
ranger_true_positive_rate <- 0
ranger_true_negative_rate <- 0
ranger_false_positive_rate <- 0
ranger_false_negative_rate <- 0
ranger_F1_score <- 0
ranger_table_total <- 0
ranger_classification_error <- 0
ranger_classification_error_mean <- 0
ranger_positive_pred_value <- 0
ranger_negative_pred_value <- 0
ranger_prevalence <- 0
ranger_detection_rate <- 0
ranger_detection_prevalence <- 0

rpart_train_accuracy <- 0
rpart_test_accuracy <- 0
rpart_test_accuracy_mean <- 0
rpart_validation_accuracy <- 0
rpart_validation_accuracy_mean <- 0
rpart_holdout_vs_train <- 0
rpart_holdout <- 0
rpart_residuals <- 0
rpart_duration <- 0
rpart_true_positive_rate <- 0
rpart_true_negative_rate <- 0
rpart_false_positive_rate <- 0
rpart_false_negative_rate <- 0
rpart_F1_score <- 0
rpart_table_total <- 0
rpart_classification_error <- 0
rpart_classification_error_mean <- 0
rpart_positive_pred_value <- 0
rpart_negative_pred_value <- 0
rpart_prevalence <- 0
rpart_detection_rate <- 0
rpart_detection_prevalence <- 0

svm_train_accuracy <- 0
svm_test_accuracy <- 0
svm_test_accuracy_mean <- 0
svm_validation_accuracy <- 0
svm_validation_accuracy_mean <- 0
svm_holdout_vs_train <- 0
svm_holdout <- 0
svm_residuals <- 0
svm_duration <- 0
svm_true_positive_rate <- 0
svm_true_negative_rate <- 0
svm_false_positive_rate <- 0
svm_false_negative_rate <- 0
svm_F1_score <- 0
svm_table_total <- 0
svm_classification_error <- 0
svm_classification_error_mean <- 0
svm_positive_pred_value <- 0
svm_negative_pred_value <- 0
svm_prevalence <- 0
svm_detection_rate <- 0
svm_detection_prevalence <- 0

tree_train_accuracy <- 0
tree_test_accuracy <- 0
tree_test_accuracy_mean <- 0
tree_validation_accuracy <- 0
tree_validation_accuracy_mean <- 0
tree_holdout_vs_train <- 0
tree_holdout <- 0
tree_residuals <- 0
tree_duration <- 0
tree_true_positive_rate <- 0
tree_true_negative_rate <- 0
tree_false_positive_rate <- 0
tree_false_negative_rate <- 0
tree_F1_score <- 0
tree_table_total <- 0
tree_classification_error <- 0
tree_classification_error_mean <- 0
tree_positive_pred_value <- 0
tree_negative_pred_value <- 0
tree_prevalence <- 0
tree_detection_rate <- 0
tree_detection_prevalence <- 0

ensemble_adaboost_train_accuracy <- 0
ensemble_adaboost_train_accuracy_mean <- 0
ensemble_adaboost_test_accuracy <- 0
ensemble_adaboost_test_accuracy_mean <- 0
ensemble_adaboost_validation_accuracy <- 0
ensemble_adaboost_validation_accuracy_mean <- 0
ensemble_adaboost_holdout_vs_train <- 0
ensemble_adaboost_holdout <- 0
ensemble_adaboost_residuals <- 0
ensemble_adaboost_duration <- 0
ensemble_adaboost_true_positive_rate <- 0
ensemble_adaboost_true_negative_rate <- 0
ensemble_adaboost_false_positive_rate <- 0
ensemble_adaboost_false_negative_rate <- 0
ensemble_adaboost_F1_score <- 0
ensemble_adaboost_table_total <- 0
ensemble_adaboost_classification_error <- 0
ensemble_adaboost_classification_error_mean <- 0
ensemble_adaboost_positive_pred_value <- 0
ensemble_adaboost_negative_pred_value <- 0
ensemble_adaboost_prevalence <- 0
ensemble_adaboost_detection_rate <- 0
ensemble_adaboost_detection_prevalence <- 0

ensemble_bag_cart_train_accuracy <- 0
ensemble_bag_cart_train_accuracy_mean <- 0
ensemble_bag_cart_test_accuracy <- 0
ensemble_bag_cart_test_accuracy_mean <- 0
ensemble_bag_cart_validation_accuracy <- 0
ensemble_bag_cart_validation_accuracy_mean <- 0
ensemble_bag_cart_holdout_vs_train <- 0
ensemble_bag_cart_holdout <- 0
ensemble_bag_cart_residuals <- 0
ensemble_bag_cart_duration <- 0
ensemble_bag_cart_true_positive_rate <- 0
ensemble_bag_cart_true_negative_rate <- 0
ensemble_bag_cart_false_positive_rate <- 0
ensemble_bag_cart_false_negative_rate <- 0
ensemble_bag_cart_F1_score <- 0
ensemble_bag_cart_table_total <- 0
ensemble_bag_cart_classification_error <- 0
ensemble_bag_cart_classification_error_mean <- 0
ensemble_bag_cart_positive_pred_value <- 0
ensemble_bag_cart_negative_pred_value <- 0
ensemble_bag_cart_prevalence <- 0
ensemble_bag_cart_detection_rate <- 0
ensemble_bag_cart_detection_prevalence <- 0

ensemble_bag_rf_train_accuracy <- 0
ensemble_bag_rf_train_accuracy_mean <- 0
ensemble_bag_rf_test_accuracy <- 0
ensemble_bag_rf_test_accuracy_mean <- 0
ensemble_bag_rf_validation_accuracy <- 0
ensemble_bag_rf_validation_accuracy_mean <- 0
ensemble_bag_rf_holdout_vs_train <- 0
ensemble_bag_rf_holdout <- 0
ensemble_bag_rf_residuals <- 0
ensemble_bag_rf_duration <- 0
ensemble_bag_rf_true_positive_rate <- 0
ensemble_bag_rf_true_negative_rate <- 0
ensemble_bag_rf_false_positive_rate <- 0
ensemble_bag_rf_false_negative_rate <- 0
ensemble_bag_rf_F1_score <- 0
ensemble_bag_rf_table_total <- 0
ensemble_bag_rf_classification_error <- 0
ensemble_bag_rf_classification_error_mean <- 0
ensemble_bag_rf_positive_pred_value <- 0
ensemble_bag_rf_negative_pred_value <- 0
ensemble_bag_rf_prevalence <- 0
ensemble_bag_rf_detection_rate <- 0
ensemble_bag_rf_detection_prevalence <- 0

ensemble_C50_train_accuracy <- 0
ensemble_C50_train_accuracy_mean <- 0
ensemble_C50_test_accuracy <- 0
ensemble_C50_test_accuracy_mean <- 0
ensemble_C50_validation_accuracy <- 0
ensemble_C50_validation_accuracy_mean <- 0
ensemble_C50_holdout_vs_train <- 0
ensemble_C50_holdout <- 0
ensemble_C50_residuals <- 0
ensemble_C50_duration <- 0
ensemble_C50_true_positive_rate <- 0
ensemble_C50_true_negative_rate <- 0
ensemble_C50_false_positive_rate <- 0
ensemble_C50_false_negative_rate <- 0
ensemble_C50_F1_score <- 0
ensemble_C50_table_total <- 0
ensemble_C50_classification_error <- 0
ensemble_C50_classification_error_mean <- 0
ensemble_C50_positive_pred_value <- 0
ensemble_C50_negative_pred_value <- 0
ensemble_C50_prevalence <- 0
ensemble_C50_detection_rate <- 0
ensemble_C50_detection_prevalence <- 0

ensemble_n_bayes_train_accuracy <- 0
ensemble_n_bayes_train_accuracy_mean <- 0
ensemble_n_bayes_test_accuracy <- 0
ensemble_n_bayes_test_accuracy_mean <- 0
ensemble_n_bayes_validation_accuracy <- 0
ensemble_n_bayes_validation_accuracy_mean <- 0
ensemble_n_bayes_holdout_vs_train <- 0
ensemble_n_bayes_holdout <- 0
ensemble_n_bayes_residuals <- 0
ensemble_n_bayes_duration <- 0
ensemble_n_bayes_true_positive_rate <- 0
ensemble_n_bayes_true_negative_rate <- 0
ensemble_n_bayes_false_positive_rate <- 0
ensemble_n_bayes_false_negative_rate <- 0
ensemble_n_bayes_F1_score <- 0
ensemble_n_bayes_table_total <- 0
ensemble_n_bayes_classification_error <- 0
ensemble_n_bayes_classification_error_mean <- 0
ensemble_n_bayes_positive_pred_value <- 0
ensemble_n_bayes_negative_pred_value <- 0
ensemble_n_bayes_prevalence <- 0
ensemble_n_bayes_detection_rate <- 0
ensemble_n_bayes_detection_prevalence <- 0

ensemble_ranger_train_accuracy <- 0
ensemble_ranger_train_accuracy_mean <- 0
ensemble_ranger_test_accuracy <- 0
ensemble_ranger_test_accuracy_mean <- 0
ensemble_ranger_validation_accuracy <- 0
ensemble_ranger_validation_accuracy_mean <- 0
ensemble_ranger_holdout_vs_train <- 0
ensemble_ranger_holdout <- 0
ensemble_ranger_residuals <- 0
ensemble_ranger_duration <- 0
ensemble_ranger_true_positive_rate <- 0
ensemble_ranger_true_negative_rate <- 0
ensemble_ranger_false_positive_rate <- 0
ensemble_ranger_false_negative_rate <- 0
ensemble_ranger_F1_score <- 0
ensemble_ranger_table_total <- 0
ensemble_ranger_classification_error <- 0
ensemble_ranger_classification_error_mean <- 0
ensemble_ranger_positive_pred_value <- 0
ensemble_ranger_negative_pred_value <- 0
ensemble_ranger_prevalence <- 0
ensemble_ranger_detection_rate <- 0
ensemble_ranger_detection_prevalence <- 0

ensemble_rf_train_accuracy <- 0
ensemble_rf_train_accuracy_mean <- 0
ensemble_rf_test_accuracy <- 0
ensemble_rf_test_accuracy_mean <- 0
ensemble_rf_validation_accuracy <- 0
ensemble_rf_validation_accuracy_mean <- 0
ensemble_rf_holdout_vs_train <- 0
ensemble_rf_holdout <- 0
ensemble_rf_residuals <- 0
ensemble_rf_duration <- 0
ensemble_rf_true_positive_rate <- 0
ensemble_rf_true_negative_rate <- 0
ensemble_rf_false_positive_rate <- 0
ensemble_rf_false_negative_rate <- 0
ensemble_rf_F1_score <- 0
ensemble_rf_table_total <- 0
ensemble_rf_classification_error <- 0
ensemble_rf_classification_error_mean <- 0
ensemble_rf_positive_pred_value <- 0
ensemble_rf_negative_pred_value <- 0
ensemble_rf_prevalence <- 0
ensemble_rf_detection_rate <- 0
ensemble_rf_detection_prevalence <- 0

ensemble_svm_train_accuracy <- 0
ensemble_svm_train_accuracy_mean <- 0
ensemble_svm_test_accuracy <- 0
ensemble_svm_test_accuracy_mean <- 0
ensemble_svm_validation_accuracy <- 0
ensemble_svm_validation_accuracy_mean <- 0
ensemble_svm_holdout_vs_train <- 0
ensemble_svm_holdout <- 0
ensemble_svm_residuals <- 0
ensemble_svm_duration <- 0
ensemble_svm_true_positive_rate <- 0
ensemble_svm_true_negative_rate <- 0
ensemble_svm_false_positive_rate <- 0
ensemble_svm_false_negative_rate <- 0
ensemble_svm_F1_score <- 0
ensemble_svm_table_total <- 0
ensemble_svm_classification_error <- 0
ensemble_svm_classification_error_mean <- 0
ensemble_svm_positive_pred_value <- 0
ensemble_svm_negative_pred_value <- 0
ensemble_svm_prevalence <- 0
ensemble_svm_detection_rate <- 0
ensemble_svm_detection_prevalence <- 0

ensemble_tree_train_accuracy <- 0
ensemble_tree_train_accuracy_mean <- 0
ensemble_tree_test_accuracy <- 0
ensemble_tree_test_accuracy_mean <- 0
ensemble_tree_validation_accuracy <- 0
ensemble_tree_validation_accuracy_mean <- 0
ensemble_tree_holdout_vs_train <- 0
ensemble_tree_holdout <- 0
ensemble_tree_residuals <- 0
ensemble_tree_duration <- 0
ensemble_tree_true_positive_rate <- 0
ensemble_tree_true_negative_rate <- 0
ensemble_tree_false_positive_rate <- 0
ensemble_tree_false_negative_rate <- 0
ensemble_tree_F1_score <- 0
ensemble_tree_table_total <- 0
ensemble_tree_classification_error <- 0
ensemble_tree_classification_error_mean <- 0
ensemble_tree_positive_pred_value <- 0
ensemble_tree_negative_pred_value <- 0
ensemble_tree_prevalence <- 0
ensemble_tree_detection_rate <- 0
ensemble_tree_detection_prevalence <- 0

value <- 0
cols <- 0
Mean_Holdout_Accuracy <- 0
count <- 0
model <- 0
holdout <- 0
name <- 0
perc <- 0
Model <- 0
Holdout_vs_train <- 0
Holdout_vs_train_St_Dev <- 0
Duration <- 0
Accuracy_Holdout_Std.Dev <- 0
holdout_vs_train_St_Dev <- 0
Duration_St_Dev <- 0
train_ratio_df <- data.frame()
test_ratio_df <- data.frame()
validation_ratio_df <- data.frame()
stratified_sampling_report <- 0

if(stratified_random_column > 0){
  df <- df[sample(nrow(df)),]
  train <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_random_column])) %>% dplyr::sample_frac(train_amount))
  train_ratio <- table(train[, stratified_random_column])/nrow(train)
  train_ratio_df <- dplyr::bind_rows(train_ratio_df, train_ratio)
  train_ratio_mean <- colMeans(train_ratio_df)

  test <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_random_column])) %>% dplyr::sample_frac(test_amount))
  test_ratio <- table(test[, stratified_random_column])/nrow(test)
  test_ratio_df <- dplyr::bind_rows(test_ratio_df, test_ratio)
  test_ratio_mean <- colMeans(test_ratio_df)

  validation <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_random_column])) %>% dplyr::sample_frac(validation_amount))
  validation_ratio <- table(validation[, stratified_random_column])/nrow(validation)
  validation_ratio_df <- dplyr::bind_rows(validation_ratio_df, validation_ratio)
  validation_ratio_mean <- colMeans(validation_ratio_df)

  total_data_mean <- table(data[, stratified_random_column])/nrow(data)

  df1 <- as.data.frame(rbind(total_data_mean, train_ratio_mean, test_ratio_mean, validation_ratio_mean))
  df1 <- data.frame(lapply(df1, function(x) if(is.numeric(x)) round(x, 4) else x))
  row.names(df1) = c('Mean total data ratios', 'Mean train data ratios', 'Mean test data ratio', 'Mean validation data ratios')
  colnames(df1) <- levels

  stratified_sampling_report <- reactable::reactable(df1, searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                                     striped = TRUE, highlight = TRUE, resizable = TRUE
  )%>%
    reactablefmtr::add_title("Stratified Random Sampling Report")
}

#### Random resampling starts here ####

for (i in 1:numresamples) {
  print(noquote(""))
  print(paste0("Resampling number ", i, " of ", numresamples, sep = ','))
  print(noquote(""))
  df <- df[sample(nrow(df)), ]

  index <- sample(c(1:3), nrow(df), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))

  train <- df[index == 1, ]
  test <- df[index == 2, ]
  validation <- df[index == 3, ]

  train01 <- train
  test01 <- test
  validation01 <- validation

  y_train <- train$y
  y_test <- test$y
  y_validation <- validation$y

  train <- df[index == 1, ] %>% dplyr::select(-y)
  test <- df[index == 2, ] %>% dplyr::select(-y)
  validation <- df[index == 3, ] %>% dplyr::select(-y)

  #### 1. Bagging ####
  bagging_start <- Sys.time()
  message("Working on Bagging analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    bagging_train_fit <- ipred::bagging(y ~ ., data = train01, coob = TRUE)
  }
  if(set_seed == "N"){
    bagging_train_fit <- ipred::bagging(y ~ ., data = train01, coob = TRUE)
  }
  bagging_train_pred <- predict(object = bagging_train_fit, newdata = train)
  bagging_train_table <- table(bagging_train_pred, y_train)
  bagging_train_accuracy[i] <- sum(diag(bagging_train_table)) / sum(bagging_train_table)
  bagging_train_accuracy_mean <- mean(bagging_train_accuracy)
  bagging_train_mean <- mean(diag(bagging_train_table)) / mean(bagging_train_table)
  bagging_train_sd <- sd(diag(bagging_train_table)) / sd(bagging_train_table)
  bagging_train_diag <- sum(diag(bagging_train_table))
  sum_diag_train_bagging <- sum(diag(bagging_train_table))
  bagging_train_prop <- diag(prop.table(bagging_train_table))

  bagging_test_pred <- predict(object = bagging_train_fit, newdata = test)
  bagging_test_table <- table(bagging_test_pred, y_test)
  bagging_test_accuracy[i] <- sum(diag(bagging_test_table)) / sum(bagging_test_table)
  bagging_test_accuracy_mean <- mean(bagging_test_accuracy)
  bagging_test_mean <- mean(diag(bagging_test_table)) / mean(bagging_test_table)
  bagging_test_sd <- sd(diag(bagging_test_table)) / sd(bagging_test_table)
  bagging_test_diag <- sum(diag(bagging_test_table))
  sum_diag_test_bagging <- sum(diag(bagging_test_table))
  bagging_test_prop <- diag(prop.table(bagging_test_table))

  bagging_validation_pred <- predict(object = bagging_train_fit, newdata = validation)
  bagging_validation_table <- table(bagging_validation_pred, y_validation)
  bagging_validation_accuracy[i] <- sum(diag(bagging_validation_table)) / sum(bagging_validation_table)
  bagging_validation_accuracy_mean <- mean(bagging_validation_accuracy)
  bagging_validation_mean <- mean(diag(bagging_validation_table)) / mean(bagging_validation_table)
  bagging_validation_sd <- sd(diag(bagging_validation_table)) / sd(bagging_validation_table)
  bagging_validation_diag <- sum(diag(bagging_validation_table))
  sum_diag_validation_bagging <- sum(diag(bagging_validation_table))
  bagging_validation_prop <- diag(prop.table(bagging_validation_table))

  bagging_holdout[i] <- mean(c(bagging_test_accuracy_mean, bagging_validation_accuracy_mean))
  bagging_holdout_mean <- mean(bagging_holdout)
  bagging_residuals[i] <- 1 - bagging_holdout[i]
  bagging_residuals_mean <- mean(bagging_residuals)
  bagging_holdout_sd <- sd(bagging_holdout)
  bagging_holdout_vs_train[i] <- bagging_holdout_mean / bagging_train_accuracy_mean
  bagging_holdout_vs_train_mean <- mean(bagging_holdout_vs_train)
  bagging_holdout_vs_train_range <- range(bagging_holdout_vs_train)
  bagging_holdout_vs_train_sd <- sd(bagging_holdout_vs_train)

  bagging_table <- bagging_test_table + bagging_validation_table
  bagging_table_total <- bagging_table_total + bagging_table
  bagging_table_sum_diag <- sum(diag(bagging_table))

  bagging_confusion_matrix <- caret::confusionMatrix(bagging_table_total)
  bagging_confusion_matrix_summary <- colMeans(bagging_confusion_matrix$byClass, na.rm = TRUE)

  bagging_true_positive_rate[i] <- bagging_confusion_matrix_summary[1]
  bagging_true_positive_rate_mean <- mean(bagging_true_positive_rate[i])
  bagging_true_negative_rate[i] <- bagging_confusion_matrix_summary[2]
  bagging_true_negative_rate_mean <- mean(bagging_true_negative_rate)
  bagging_false_negative_rate[i] <- 1 -  bagging_true_positive_rate[i]
  bagging_false_negative_rate_mean <- mean(bagging_false_negative_rate)
  bagging_false_positive_rate[i] <- 1 - bagging_true_negative_rate[i]
  bagging_false_positive_rate_mean <- mean(bagging_false_positive_rate)
  bagging_positive_pred_value[i] <- bagging_confusion_matrix_summary[3]
  bagging_positive_pred_value_mean <- mean(bagging_positive_pred_value)
  bagging_negative_pred_value[i] <- bagging_confusion_matrix_summary[4]
  bagging_negative_pred_value_mean <- mean(bagging_negative_pred_value)
  bagging_prevalence[i] <- bagging_confusion_matrix_summary[8]
  bagging_prevalence_mean <- mean(bagging_prevalence)
  bagging_detection_rate[i] <- bagging_confusion_matrix_summary[9]
  bagging_detection_rate_mean <- mean(bagging_detection_rate)
  bagging_detection_prevalence[i] <- bagging_confusion_matrix_summary[10]
  bagging_detection_prevalence_mean <- mean(bagging_detection_prevalence)
  bagging_classification_error[i] <- 1 - bagging_holdout[i]
  bagging_classification_error_mean <- mean(bagging_classification_error)
  bagging_F1_score[i] <- bagging_confusion_matrix_summary[7]
  bagging_F1_score_mean <- mean(bagging_F1_score[i])

  bagging_end <- Sys.time()
  bagging_duration[i] <- bagging_end - bagging_start
  bagging_duration_mean <- mean(bagging_duration)
  bagging_duration_sd <- sd(bagging_duration)

  #### 2. Bagged Random Forest ####
  bag_rf_start <- Sys.time()
  message("Working on Bagged Random Forest analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    bag_rf_train_fit <- randomForest::randomForest(y ~ ., data = train01, mtry = ncol(train))
  }
  if(set_seed == "N"){
    bag_rf_train_fit <- randomForest::randomForest(y ~ ., data = train01, mtry = ncol(train))
  }
  bag_rf_train_pred <- predict(bag_rf_train_fit, train, type = "class")
  bag_rf_train_table <- table(bag_rf_train_pred, y_train)
  bag_rf_train_accuracy[i] <- sum(diag(bag_rf_train_table)) / sum(bag_rf_train_table)
  bag_rf_train_accuracy_mean <- mean(bag_rf_train_accuracy)
  bag_rf_train_diag <- sum(bag_rf_train_table)
  bag_rf_train_mean <- mean(diag(bag_rf_train_table)) / mean(bag_rf_train_table)
  bag_rf_train_sd <- sd(diag(bag_rf_train_table)) / sd(bag_rf_train_table)
  sum_diag_bag_train_rf <- sum(diag(bag_rf_train_table))
  bag_rf_train_prop <- diag(prop.table(bag_rf_train_table, margin = 1))

  bag_rf_test_pred <- predict(bag_rf_train_fit, test, type = "class")
  bag_rf_test_table <- table(bag_rf_test_pred, y_test)
  bag_rf_test_accuracy[i] <- sum(diag(bag_rf_test_table)) / sum(bag_rf_test_table)
  bag_rf_test_accuracy_mean <- mean(bag_rf_test_accuracy)
  bag_rf_test_diag <- sum(bag_rf_test_table)
  bag_rf_test_mean <- mean(diag(bag_rf_test_table)) / mean(bag_rf_test_table)
  bag_rf_test_sd <- sd(diag(bag_rf_test_table)) / sd(bag_rf_test_table)
  sum_diag_bag_test_rf <- sum(diag(bag_rf_test_table))
  bag_rf_test_prop <- diag(prop.table(bag_rf_test_table, margin = 1))

  bag_rf_validation_pred <- predict(bag_rf_train_fit, validation, type = "class")
  bag_rf_validation_table <- table(bag_rf_validation_pred, y_validation)
  bag_rf_validation_accuracy[i] <- sum(diag(bag_rf_validation_table)) / sum(bag_rf_validation_table)
  bag_rf_validation_accuracy_mean <- mean(bag_rf_validation_accuracy)
  bag_rf_validation_diag <- sum(bag_rf_validation_table)
  bag_rf_validation_mean <- mean(diag(bag_rf_validation_table)) / mean(bag_rf_validation_table)
  bag_rf_validation_sd <- sd(diag(bag_rf_validation_table)) / sd(bag_rf_validation_table)
  sum_diag_bag_validation_rf <- sum(diag(bag_rf_validation_table))
  bag_rf_validation_prop <- diag(prop.table(bag_rf_validation_table, margin = 1))

  bag_rf_holdout[i] <- mean(c(bag_rf_test_accuracy_mean, bag_rf_validation_accuracy_mean))
  bag_rf_holdout_mean <- mean(bag_rf_holdout)
  bag_rf_residuals[i] <- 1 - bag_rf_holdout[i]
  bag_rf_residuals_mean <- mean(bag_rf_residuals)
  bag_rf_holdout_sd <- sd(bag_rf_holdout)
  bag_rf_holdout_vs_train[i] <- bag_rf_holdout_mean / bag_rf_train_accuracy_mean
  bag_rf_holdout_vs_train_mean <- mean(bag_rf_holdout_vs_train)
  bag_rf_holdout_vs_train_range <- range(bag_rf_holdout_vs_train)
  bag_rf_holdout_vs_train_sd <- sd(bag_rf_holdout_vs_train)

  bag_rf_table <- bag_rf_test_table + bag_rf_validation_table
  bag_rf_table_total <- bag_rf_table_total + bag_rf_table
  bag_rf_table_sum_diag <- sum(diag(bag_rf_table))

  bag_rf_confusion_matrix <- caret::confusionMatrix(bag_rf_table_total)
  bag_rf_confusion_matrix_summary <- colMeans(bag_rf_confusion_matrix$byClass, na.rm = TRUE)

  bag_rf_true_positive_rate[i] <- bag_rf_confusion_matrix_summary[1]
  bag_rf_true_positive_rate_mean <- mean(bag_rf_true_positive_rate[i])
  bag_rf_true_negative_rate[i] <- bag_rf_confusion_matrix_summary[2]
  bag_rf_true_negative_rate_mean <- mean(bag_rf_true_negative_rate)
  bag_rf_false_negative_rate[i] <- 1 -  bag_rf_true_positive_rate[i]
  bag_rf_false_negative_rate_mean <- mean(bag_rf_false_negative_rate)
  bag_rf_false_positive_rate[i] <- 1 - bag_rf_true_negative_rate[i]
  bag_rf_false_positive_rate_mean <- mean(bag_rf_false_positive_rate)
  bag_rf_positive_pred_value[i] <- bag_rf_confusion_matrix_summary[3]
  bag_rf_positive_pred_value_mean <- mean(bag_rf_positive_pred_value)
  bag_rf_negative_pred_value[i] <- bag_rf_confusion_matrix_summary[4]
  bag_rf_negative_pred_value_mean <- mean(bag_rf_negative_pred_value)
  bag_rf_prevalence[i] <- bag_rf_confusion_matrix_summary[8]
  bag_rf_prevalence_mean <- mean(bag_rf_prevalence)
  bag_rf_detection_rate[i] <- bag_rf_confusion_matrix_summary[9]
  bag_rf_detection_rate_mean <- mean(bag_rf_detection_rate)
  bag_rf_detection_prevalence[i] <- bag_rf_confusion_matrix_summary[10]
  bag_rf_detection_prevalence_mean <- mean(bag_rf_detection_prevalence)
  bag_rf_classification_error[i] <- 1 - bag_rf_holdout[i]
  bag_rf_classification_error_mean <- mean(bag_rf_classification_error)
  bag_rf_F1_score[i] <- bag_rf_confusion_matrix_summary[7]
  bag_rf_F1_score_mean <- mean(bag_rf_F1_score[i])

  bag_rf_end <- Sys.time()
  bag_rf_duration[i] <- bag_rf_end - bag_rf_start
  bag_rf_duration_mean <- mean(bag_rf_duration)
  bag_rf_duration_sd <- sd(bag_rf_duration)

  #### 3. C50 ####
  C50_start <- Sys.time()
  message("Working on C50 analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    C50_train_fit <- C50::C5.0(as.factor(y_train) ~ ., data = train)
  }
  if(set_seed == "N"){
    C50_train_fit <- C50::C5.0(as.factor(y_train) ~ ., data = train)
  }
  C50_train_pred <- predict(C50_train_fit, train)
  C50_train_table <- table(C50_train_pred, y_train)
  C50_train_accuracy[i] <- sum(diag(C50_train_table)) / sum(C50_train_table)
  C50_train_accuracy_mean <- mean(C50_train_accuracy)
  C50_train_mean <- mean(diag(C50_train_table)) / mean(C50_train_table)
  C50_train_sd <- sd(diag(C50_train_table)) / sd(C50_train_table)
  sum_diag_train_C50 <- sum(diag(C50_train_table))
  C50_train_prop <- diag(prop.table(C50_train_table, margin = 1))

  C50_test_pred <- predict(C50_train_fit, test)
  C50_test_table <- table(C50_test_pred, y_test)
  C50_test_accuracy[i] <- sum(diag(C50_test_table)) / sum(C50_test_table)
  C50_test_accuracy_mean <- mean(C50_test_accuracy)
  C50_test_mean <- mean(diag(C50_test_table)) / mean(C50_test_table)
  C50_test_sd <- sd(diag(C50_test_table)) / sd(C50_test_table)
  sum_diag_test_C50 <- sum(diag(C50_test_table))
  C50_test_prop <- diag(prop.table(C50_test_table, margin = 1))

  C50_validation_pred <- predict(C50_train_fit, validation)
  C50_validation_table <- table(C50_validation_pred, y_validation)
  C50_validation_accuracy[i] <- sum(diag(C50_validation_table)) / sum(C50_validation_table)
  C50_validation_accuracy_mean <- mean(C50_validation_accuracy)
  C50_validation_mean <- mean(diag(C50_validation_table)) / mean(C50_validation_table)
  C50_validation_sd <- sd(diag(C50_validation_table)) / sd(C50_validation_table)
  sum_diag_validation_C50 <- sum(diag(C50_validation_table))
  C50_validation_prop <- diag(prop.table(C50_validation_table, margin = 1))

  C50_holdout[i] <- mean(c(C50_test_accuracy_mean, C50_validation_accuracy_mean))
  C50_holdout_mean <- mean(C50_holdout)
  C50_residuals[i] <- 1 - C50_holdout[i]
  C50_residuals_mean <- mean(C50_residuals)
  C50_holdout_sd <- sd(C50_holdout)
  C50_holdout_vs_train[i] <- C50_holdout_mean / C50_train_accuracy_mean
  C50_holdout_vs_train_mean <- mean(C50_holdout_vs_train)
  C50_holdout_vs_train_range <- range(C50_holdout_vs_train)
  C50_holdout_vs_train_sd <- sd(C50_holdout_vs_train)

  C50_table <- C50_test_table + C50_validation_table
  C50_table_total <- C50_table_total + C50_table
  C50_table_sum_diag <- sum(diag(C50_table))

  C50_confusion_matrix <- caret::confusionMatrix(C50_table_total)
  C50_confusion_matrix_summary <- colMeans(C50_confusion_matrix$byClass, na.rm = TRUE)

  C50_true_positive_rate[i] <- C50_confusion_matrix_summary[1]
  C50_true_positive_rate_mean <- mean(C50_true_positive_rate[i])
  C50_true_negative_rate[i] <- C50_confusion_matrix_summary[2]
  C50_true_negative_rate_mean <- mean(C50_true_negative_rate)
  C50_false_negative_rate[i] <- 1 -  C50_true_positive_rate[i]
  C50_false_negative_rate_mean <- mean(C50_false_negative_rate)
  C50_false_positive_rate[i] <- 1 - C50_true_negative_rate[i]
  C50_false_positive_rate_mean <- mean(C50_false_positive_rate)
  C50_positive_pred_value[i] <- C50_confusion_matrix_summary[3]
  C50_positive_pred_value_mean <- mean(C50_positive_pred_value)
  C50_negative_pred_value[i] <- C50_confusion_matrix_summary[4]
  C50_negative_pred_value_mean <- mean(C50_negative_pred_value)
  C50_prevalence[i] <- C50_confusion_matrix_summary[8]
  C50_prevalence_mean <- mean(C50_prevalence)
  C50_detection_rate[i] <- C50_confusion_matrix_summary[9]
  C50_detection_rate_mean <- mean(C50_detection_rate)
  C50_detection_prevalence[i] <- C50_confusion_matrix_summary[10]
  C50_detection_prevalence_mean <- mean(C50_detection_prevalence)
  C50_classification_error[i] <- 1 - C50_holdout[i]
  C50_classification_error_mean <- mean(C50_classification_error)
  C50_F1_score[i] <- C50_confusion_matrix_summary[7]
  C50_F1_score_mean <- mean(C50_F1_score[i])

  C50_end <- Sys.time()
  C50_duration[i] <- C50_end - C50_start
  C50_duration_mean <- mean(C50_duration)
  C50_duration_sd <- sd(C50_duration)

  #### 4. Linear Model ####
  linear_start <- Sys.time()
  message("Working on Linear analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    linear_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "LMModel")
  }
  if(set_seed == "N"){
    linear_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "LMModel")
  }
  linear_train_pred <- predict(object = linear_train_fit, newdata = train01)
  linear_train_table <- table(linear_train_pred, y_train)
  linear_train_accuracy[i] <- sum(diag(linear_train_table)) / sum(linear_train_table)
  linear_train_accuracy_mean <- mean(linear_train_accuracy)
  linear_train_mean <- mean(diag(linear_train_table)) / mean(linear_train_table)
  linear_train_sd <- sd(diag(linear_train_table)) / sd(linear_train_table)
  sum_diag_train_linear <- sum(diag(linear_train_table))
  linear_train_prop <- diag(prop.table(linear_train_table, margin = 1))

  linear_test_pred <- predict(object = linear_train_fit, newdata = test01)
  linear_test_table <- table(linear_test_pred, y_test)
  linear_test_accuracy[i] <- sum(diag(linear_test_table)) / sum(linear_test_table)
  linear_test_accuracy_mean <- mean(linear_test_accuracy)
  linear_test_mean <- mean(diag(linear_test_table)) / mean(linear_test_table)
  linear_test_sd <- sd(diag(linear_test_table)) / sd(linear_test_table)
  sum_diag_test_linear <- sum(diag(linear_test_table))
  linear_test_prop <- diag(prop.table(linear_test_table, margin = 1))

  linear_validation_pred <- predict(object = linear_train_fit, newdata = validation01)
  linear_validation_table <- table(linear_validation_pred, y_validation)
  linear_validation_accuracy[i] <- sum(diag(linear_validation_table)) / sum(linear_validation_table)
  linear_validation_accuracy_mean <- mean(linear_validation_accuracy)
  linear_validation_mean <- mean(diag(linear_validation_table)) / mean(linear_validation_table)
  linear_validation_sd <- sd(diag(linear_validation_table)) / sd(linear_validation_table)
  sum_diag_validation_linear <- sum(diag(linear_validation_table))
  linear_validation_prop <- diag(prop.table(linear_validation_table, margin = 1))

  linear_holdout[i] <- mean(c(linear_test_accuracy_mean, linear_validation_accuracy_mean))
  linear_holdout_mean <- mean(linear_holdout)
  linear_residuals[i] <- 1 - linear_holdout[i]
  linear_residuals_mean <- mean(linear_residuals)
  linear_holdout_sd <- sd(linear_holdout)
  linear_holdout_vs_train[i] <- linear_holdout_mean / linear_train_accuracy_mean
  linear_holdout_vs_train_mean <- mean(linear_holdout_vs_train)
  linear_holdout_vs_train_range <- range(linear_holdout_vs_train)
  linear_holdout_vs_train_sd <- sd(linear_holdout_vs_train)

  linear_table <- linear_test_table + linear_validation_table
  linear_table_total <- linear_table_total + linear_table
  linear_table_sum_diag <- sum(diag(linear_table))

  linear_confusion_matrix <- caret::confusionMatrix(linear_table_total)
  linear_confusion_matrix_summary <- colMeans(linear_confusion_matrix$byClass, na.rm = TRUE)

  linear_true_positive_rate[i] <- linear_confusion_matrix_summary[1]
  linear_true_positive_rate_mean <- mean(linear_true_positive_rate[i])
  linear_true_negative_rate[i] <- linear_confusion_matrix_summary[2]
  linear_true_negative_rate_mean <- mean(linear_true_negative_rate)
  linear_false_negative_rate[i] <- 1 -  linear_true_positive_rate[i]
  linear_false_negative_rate_mean <- mean(linear_false_negative_rate)
  linear_false_positive_rate[i] <- 1 - linear_true_negative_rate[i]
  linear_false_positive_rate_mean <- mean(linear_false_positive_rate)
  linear_positive_pred_value[i] <- linear_confusion_matrix_summary[3]
  linear_positive_pred_value_mean <- mean(linear_positive_pred_value)
  linear_negative_pred_value[i] <- linear_confusion_matrix_summary[4]
  linear_negative_pred_value_mean <- mean(linear_negative_pred_value)
  linear_prevalence[i] <- linear_confusion_matrix_summary[8]
  linear_prevalence_mean <- mean(linear_prevalence)
  linear_detection_rate[i] <- linear_confusion_matrix_summary[9]
  linear_detection_rate_mean <- mean(linear_detection_rate)
  linear_detection_prevalence[i] <- linear_confusion_matrix_summary[10]
  linear_detection_prevalence_mean <- mean(linear_detection_prevalence)
  linear_classification_error[i] <- 1 - linear_holdout[i]
  linear_classification_error_mean <- mean(linear_classification_error)
  linear_F1_score[i] <- linear_confusion_matrix_summary[7]
  linear_F1_score_mean <- mean(linear_F1_score[i])

  linear_end <- Sys.time()
  linear_duration[i] <- linear_end - linear_start
  linear_duration_mean <- mean(linear_duration)
  linear_duration_sd <- sd(linear_duration)

  #### 5. Naive Bayes ####
  n_bayes_start <- Sys.time()
  message("Working on Naive Bayes analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    n_bayes_train_fit <- e1071::naiveBayes(y_train ~ ., data = train)
  }
  if(set_seed == "N"){
    n_bayes_train_fit <- e1071::naiveBayes(y_train ~ ., data = train)
  }
  n_bayes_train_pred <- predict(n_bayes_train_fit, train)
  n_bayes_train_table <- table(n_bayes_train_pred, y_train)
  n_bayes_train_accuracy[i] <- sum(diag(n_bayes_train_table)) / sum(n_bayes_train_table)
  n_bayes_train_accuracy_mean <- mean(n_bayes_train_accuracy)
  n_bayes_train_diag <- sum(diag(n_bayes_train_table))
  n_bayes_train_mean <- mean(diag(n_bayes_train_table)) / mean(n_bayes_train_table)
  n_bayes_train_sd <- sd(diag(n_bayes_train_table)) / sd(n_bayes_train_table)
  sum_diag_n_train_bayes <- sum(diag(n_bayes_train_table))
  n_bayes_train_prop <- diag(prop.table(n_bayes_train_table, margin = 1))

  n_bayes_test_pred <- predict(n_bayes_train_fit, test)
  n_bayes_test_table <- table(n_bayes_test_pred, y_test)
  n_bayes_test_accuracy[i] <- sum(diag(n_bayes_test_table)) / sum(n_bayes_test_table)
  n_bayes_test_accuracy_mean <- mean(n_bayes_test_accuracy)
  n_bayes_test_diag <- sum(diag(n_bayes_test_table))
  n_bayes_test_mean <- mean(diag(n_bayes_test_table)) / mean(n_bayes_test_table)
  n_bayes_test_sd <- sd(diag(n_bayes_test_table)) / sd(n_bayes_test_table)
  sum_diag_n_test_bayes <- sum(diag(n_bayes_test_table))
  n_bayes_test_prop <- diag(prop.table(n_bayes_test_table, margin = 1))

  n_bayes_validation_pred <- predict(n_bayes_train_fit, validation)
  n_bayes_validation_table <- table(n_bayes_validation_pred, y_validation)
  n_bayes_validation_accuracy[i] <- sum(diag(n_bayes_validation_table)) / sum(n_bayes_validation_table)
  n_bayes_validation_accuracy_mean <- mean(n_bayes_validation_accuracy)
  n_bayes_validation_diag <- sum(diag(n_bayes_validation_table))
  n_bayes_validation_mean <- mean(diag(n_bayes_validation_table)) / mean(n_bayes_validation_table)
  n_bayes_validation_sd <- sd(diag(n_bayes_validation_table)) / sd(n_bayes_validation_table)
  sum_diag_n_validation_bayes <- sum(diag(n_bayes_validation_table))
  n_bayes_validation_prop <- diag(prop.table(n_bayes_validation_table, margin = 1))

  n_bayes_holdout[i] <- mean(c(n_bayes_test_accuracy_mean, n_bayes_validation_accuracy_mean))
  n_bayes_holdout_mean <- mean(n_bayes_holdout)
  n_bayes_residuals[i] <- 1 - n_bayes_holdout[i]
  n_bayes_residuals_mean <- mean(n_bayes_residuals)
  n_bayes_holdout_sd <- sd(n_bayes_holdout)
  n_bayes_holdout_vs_train[i] <- n_bayes_holdout_mean / n_bayes_train_accuracy_mean
  n_bayes_holdout_vs_train_mean <- mean(n_bayes_holdout_vs_train)
  n_bayes_holdout_vs_train_range <- range(n_bayes_holdout_vs_train)
  n_bayes_holdout_vs_train_sd <- sd(n_bayes_holdout_vs_train)

  n_bayes_table <- n_bayes_test_table + n_bayes_validation_table
  n_bayes_table_total <- n_bayes_table_total + n_bayes_table
  n_bayes_table_sum_diag <- sum(diag(n_bayes_table))

  n_bayes_confusion_matrix <- caret::confusionMatrix(n_bayes_table_total)
  n_bayes_confusion_matrix_summary <- colMeans(n_bayes_confusion_matrix$byClass, na.rm = TRUE)

  n_bayes_true_positive_rate[i] <- n_bayes_confusion_matrix_summary[1]
  n_bayes_true_positive_rate_mean <- mean(n_bayes_true_positive_rate[i])
  n_bayes_true_negative_rate[i] <- n_bayes_confusion_matrix_summary[2]
  n_bayes_true_negative_rate_mean <- mean(n_bayes_true_negative_rate)
  n_bayes_false_negative_rate[i] <- 1 -  n_bayes_true_positive_rate[i]
  n_bayes_false_negative_rate_mean <- mean(n_bayes_false_negative_rate)
  n_bayes_false_positive_rate[i] <- 1 - n_bayes_true_negative_rate[i]
  n_bayes_false_positive_rate_mean <- mean(n_bayes_false_positive_rate)
  n_bayes_positive_pred_value[i] <- n_bayes_confusion_matrix_summary[3]
  n_bayes_positive_pred_value_mean <- mean(n_bayes_positive_pred_value)
  n_bayes_negative_pred_value[i] <- n_bayes_confusion_matrix_summary[4]
  n_bayes_negative_pred_value_mean <- mean(n_bayes_negative_pred_value)
  n_bayes_prevalence[i] <- n_bayes_confusion_matrix_summary[8]
  n_bayes_prevalence_mean <- mean(n_bayes_prevalence)
  n_bayes_detection_rate[i] <- n_bayes_confusion_matrix_summary[9]
  n_bayes_detection_rate_mean <- mean(n_bayes_detection_rate)
  n_bayes_detection_prevalence[i] <- n_bayes_confusion_matrix_summary[10]
  n_bayes_detection_prevalence_mean <- mean(n_bayes_detection_prevalence)
  n_bayes_classification_error[i] <- 1 - n_bayes_holdout[i]
  n_bayes_classification_error_mean <- mean(n_bayes_classification_error)
  n_bayes_F1_score[i] <- n_bayes_confusion_matrix_summary[7]
  n_bayes_F1_score_mean <- mean(n_bayes_F1_score[i])

  n_bayes_end <- Sys.time()
  n_bayes_duration[i] <- n_bayes_end - n_bayes_start
  n_bayes_duration_mean <- mean(n_bayes_duration)
  n_bayes_duration_sd <- sd(n_bayes_duration)

  #### 6. Partial Least Squares ####
  pls_start <- Sys.time()
  message("Working on Partial Least Squares analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    pls_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "PLSModel")
  }
  if(set_seed == "N"){
    pls_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "PLSModel")
  }
  pls_train_predict <- predict(object = pls_train_fit, newdata = train01)
  pls_train_table <- table(pls_train_predict, y_train)
  pls_train_accuracy[i] <- sum(diag(pls_train_table)) / sum(pls_train_table)
  pls_train_accuracy_mean <- mean(pls_train_accuracy)
  pls_train_pred <- pls_train_predict
  pls_train_mean <- mean(diag(pls_train_table)) / sum(pls_train_table)
  pls_train_sd <- sd(diag(pls_train_table)) / sd(pls_train_table)
  sum_diag_train_pls <- sum(diag(pls_train_table))
  pls_train_prop <- diag(prop.table(pls_train_table, margin = 1))

  pls_test_predict <- predict(object = pls_train_fit, newdata = test01)
  pls_test_table <- table(pls_test_predict, y_test)
  pls_test_accuracy[i] <- sum(diag(pls_test_table)) / sum(pls_test_table)
  pls_test_accuracy_mean <- mean(pls_test_accuracy)
  pls_test_pred <- pls_test_predict
  pls_test_mean <- mean(diag(pls_test_table)) / sum(pls_test_table)
  pls_test_sd <- sd(diag(pls_test_table)) / sd(pls_test_table)
  sum_diag_test_pls <- sum(diag(pls_test_table))
  pls_test_prop <- diag(prop.table(pls_test_table, margin = 1))

  pls_validation_predict <- predict(object = pls_train_fit, newdata = validation01)
  pls_validation_table <- table(pls_validation_predict, y_validation)
  pls_validation_accuracy[i] <- sum(diag(pls_validation_table)) / sum(pls_validation_table)
  pls_validation_accuracy_mean <- mean(pls_validation_accuracy)
  pls_validation_pred <- pls_validation_predict
  pls_validation_mean <- mean(diag(pls_validation_table)) / sum(pls_validation_table)
  pls_validation_sd <- sd(diag(pls_validation_table)) / sd(pls_validation_table)
  sum_diag_validation_pls <- sum(diag(pls_validation_table))
  pls_validation_prop <- diag(prop.table(pls_validation_table, margin = 1))

  pls_holdout[i] <- mean(c(pls_test_accuracy_mean, pls_validation_accuracy_mean))
  pls_holdout_mean <- mean(pls_holdout)
  pls_residuals[i] <- 1 - pls_holdout[i]
  pls_residuals_mean <- mean(pls_residuals)
  pls_holdout_sd <- sd(pls_holdout)
  pls_holdout_vs_train[i] <- pls_holdout_mean / pls_train_accuracy_mean
  pls_holdout_vs_train_mean <- mean(pls_holdout_vs_train)
  pls_holdout_vs_train_range <- range(pls_holdout_vs_train)
  pls_holdout_vs_train_sd <- sd(pls_holdout_vs_train)

  pls_table <- pls_test_table + pls_validation_table
  pls_table_total <- pls_table_total + pls_table
  pls_table_sum_diag <- sum(diag(pls_table))

  pls_confusion_matrix <- caret::confusionMatrix(pls_table_total)
  pls_confusion_matrix_summary <- colMeans(pls_confusion_matrix$byClass, na.rm = TRUE)

  pls_true_positive_rate[i] <- pls_confusion_matrix_summary[1]
  pls_true_positive_rate_mean <- mean(pls_true_positive_rate[i])
  pls_true_negative_rate[i] <- pls_confusion_matrix_summary[2]
  pls_true_negative_rate_mean <- mean(pls_true_negative_rate)
  pls_false_negative_rate[i] <- 1 -  pls_true_positive_rate[i]
  pls_false_negative_rate_mean <- mean(pls_false_negative_rate)
  pls_false_positive_rate[i] <- 1 - pls_true_negative_rate[i]
  pls_false_positive_rate_mean <- mean(pls_false_positive_rate)
  pls_positive_pred_value[i] <- pls_confusion_matrix_summary[3]
  pls_positive_pred_value_mean <- mean(pls_positive_pred_value)
  pls_negative_pred_value[i] <- pls_confusion_matrix_summary[4]
  pls_negative_pred_value_mean <- mean(pls_negative_pred_value)
  pls_prevalence[i] <- pls_confusion_matrix_summary[8]
  pls_prevalence_mean <- mean(pls_prevalence)
  pls_detection_rate[i] <- pls_confusion_matrix_summary[9]
  pls_detection_rate_mean <- mean(pls_detection_rate)
  pls_detection_prevalence[i] <- pls_confusion_matrix_summary[10]
  pls_detection_prevalence_mean <- mean(pls_detection_prevalence)
  pls_classification_error[i] <- 1 - pls_holdout[i]
  pls_classification_error_mean <- mean(pls_classification_error)
  pls_F1_score[i] <- pls_confusion_matrix_summary[7]
  pls_F1_score_mean <- mean(pls_F1_score[i])

  pls_end <- Sys.time()
  pls_duration[i] <- pls_end - pls_start
  pls_duration_mean <- mean(pls_duration)
  pls_duration_sd <- sd(pls_duration)

  #### 7. Penalized Discriminant Analysis Model ####
  pda_start <- Sys.time()
  message("Working on Penalized Discriminant analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    pda_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "PDAModel")
  }
  if(set_seed == "N"){
    pda_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "PDAModel")
  }
  pda_train_predict <- predict(object = pda_train_fit, newdata = train01)
  pda_train_table <- table(pda_train_predict, y_train)
  pda_train_accuracy[i] <- sum(diag(pda_train_table)) / sum(pda_train_table)
  pda_train_accuracy_mean <- mean(pda_train_accuracy)
  pda_train_pred <- pda_train_predict
  pda_train_mean <- mean(diag(pda_train_table)) / sum(pda_train_table)
  pda_train_sd <- sd(diag(pda_train_table)) / sd(pda_train_table)
  sum_diag_train_pda <- sum(diag(pda_train_table))
  pda_train_prop <- diag(prop.table(pda_train_table, margin = 1))

  pda_test_predict <- predict(object = pda_train_fit, newdata = test01)
  pda_test_table <- table(pda_test_predict, y_test)
  pda_test_accuracy[i] <- sum(diag(pda_test_table)) / sum(pda_test_table)
  pda_test_accuracy_mean <- mean(pda_test_accuracy)
  pda_test_pred <- pda_test_predict
  pda_test_mean <- mean(diag(pda_test_table)) / sum(pda_test_table)
  pda_test_sd <- sd(diag(pda_test_table)) / sd(pda_test_table)
  sum_diag_test_pda <- sum(diag(pda_test_table))
  pda_test_prop <- diag(prop.table(pda_test_table, margin = 1))

  pda_validation_predict <- predict(object = pda_train_fit, newdata = validation01)
  pda_validation_table <- table(pda_validation_predict, y_validation)
  pda_validation_accuracy[i] <- sum(diag(pda_validation_table)) / sum(pda_validation_table)
  pda_validation_accuracy_mean <- mean(pda_validation_accuracy)
  pda_validation_pred <- pda_validation_predict
  pda_validation_mean <- mean(diag(pda_validation_table)) / sum(pda_validation_table)
  pda_validation_sd <- sd(diag(pda_validation_table)) / sd(pda_validation_table)
  sum_diag_validation_pda <- sum(diag(pda_validation_table))
  pda_validation_prop <- diag(prop.table(pda_validation_table, margin = 1))

  pda_holdout[i] <- mean(c(pda_test_accuracy_mean, pda_validation_accuracy_mean))
  pda_holdout_mean <- mean(pda_holdout)
  pda_residuals[i] <- 1 - pda_holdout[i]
  pda_residuals_mean <- mean(pda_residuals)
  pda_holdout_sd <- sd(pda_holdout)
  pda_holdout_vs_train[i] <- pda_holdout_mean / pda_train_accuracy_mean
  pda_holdout_vs_train_mean <- mean(pda_holdout_vs_train)
  pda_holdout_vs_train_range <- range(pda_holdout_vs_train)
  pda_holdout_vs_train_sd <- sd(pda_holdout_vs_train)

  pda_table <- pda_test_table + pda_validation_table
  pda_table_total <- pda_table_total + pda_table
  pda_table_sum_diag <- sum(diag(pda_table))

  pda_confusion_matrix <- caret::confusionMatrix(pda_table_total)
  pda_confusion_matrix_summary <- colMeans(pda_confusion_matrix$byClass, na.rm = TRUE)

  pda_true_positive_rate[i] <- pda_confusion_matrix_summary[1]
  pda_true_positive_rate_mean <- mean(pda_true_positive_rate[i])
  pda_true_negative_rate[i] <- pda_confusion_matrix_summary[2]
  pda_true_negative_rate_mean <- mean(pda_true_negative_rate)
  pda_false_negative_rate[i] <- 1 -  pda_true_positive_rate[i]
  pda_false_negative_rate_mean <- mean(pda_false_negative_rate)
  pda_false_positive_rate[i] <- 1 - pda_true_negative_rate[i]
  pda_false_positive_rate_mean <- mean(pda_false_positive_rate)
  pda_positive_pred_value[i] <- pda_confusion_matrix_summary[3]
  pda_positive_pred_value_mean <- mean(pda_positive_pred_value)
  pda_negative_pred_value[i] <- pda_confusion_matrix_summary[4]
  pda_negative_pred_value_mean <- mean(pda_negative_pred_value)
  pda_prevalence[i] <- pda_confusion_matrix_summary[8]
  pda_prevalence_mean <- mean(pda_prevalence)
  pda_detection_rate[i] <- pda_confusion_matrix_summary[9]
  pda_detection_rate_mean <- mean(pda_detection_rate)
  pda_detection_prevalence[i] <- pda_confusion_matrix_summary[10]
  pda_detection_prevalence_mean <- mean(pda_detection_prevalence)
  pda_classification_error[i] <- 1 - pda_holdout[i]
  pda_classification_error_mean <- mean(pda_classification_error)
  pda_F1_score[i] <- pda_confusion_matrix_summary[7]
  pda_F1_score_mean <- mean(pda_F1_score[i])

  pda_end <- Sys.time()
  pda_duration[i] <- pda_end - pda_start
  pda_duration_mean <- mean(pda_duration)
  pda_duration_sd <- sd(pda_duration)

  #### 8. Random Forest ####
  rf_start <- Sys.time()
  message("Working on Random Forest analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    rf_train_fit <- randomForest::randomForest(x = train, y = y_train, data = df)
  }
  if(set_seed == "N"){
    rf_train_fit <- randomForest::randomForest(x = train, y = y_train, data = df)
  }
  rf_train_pred <- predict(rf_train_fit, train, type = "class")
  rf_train_table <- table(rf_train_pred, y_train)
  rf_train_accuracy[i] <- sum(diag(rf_train_table)) / sum(rf_train_table)
  rf_train_accuracy_mean <- mean(rf_train_accuracy)
  rf_train_diag <- sum(diag(rf_train_table))
  rf_train_mean <- mean(diag(rf_train_table)) / mean(rf_train_table)
  rf_train_sd <- sd(diag(rf_train_table)) / sd(rf_train_table)
  sum_diag_train_rf <- sum(diag(rf_train_table))
  rf_train_prop <- diag(prop.table(rf_train_table, margin = 1))

  rf_test_pred <- predict(rf_train_fit, test, type = "class")
  rf_test_table <- table(rf_test_pred, y_test)
  rf_test_accuracy[i] <- sum(diag(rf_test_table)) / sum(rf_test_table)
  rf_test_accuracy_mean <- mean(rf_test_accuracy)
  rf_test_diag <- sum(diag(rf_test_table))
  rf_test_mean <- mean(diag(rf_test_table)) / mean(rf_test_table)
  rf_test_sd <- sd(diag(rf_test_table)) / sd(rf_test_table)
  sum_diag_test_rf <- sum(diag(rf_test_table))
  rf_test_prop <- diag(prop.table(rf_test_table, margin = 1))

  rf_validation_pred <- predict(rf_train_fit, validation, type = "class")
  rf_validation_table <- table(rf_validation_pred, y_validation)
  rf_validation_accuracy[i] <- sum(diag(rf_validation_table)) / sum(rf_validation_table)
  rf_validation_accuracy_mean <- mean(rf_validation_accuracy)
  rf_validation_diag <- sum(diag(rf_validation_table))
  rf_validation_mean <- mean(diag(rf_validation_table)) / mean(rf_validation_table)
  rf_validation_sd <- sd(diag(rf_validation_table)) / sd(rf_validation_table)
  sum_diag_validation_rf <- sum(diag(rf_validation_table))
  rf_validation_prop <- diag(prop.table(rf_validation_table, margin = 1))

  rf_holdout[i] <- mean(c(rf_test_accuracy_mean, rf_validation_accuracy_mean))
  rf_holdout_mean <- mean(rf_holdout)
  rf_residuals[i] <- 1 - rf_holdout[i]
  rf_residuals_mean <- mean(rf_residuals)
  rf_holdout_sd <- sd(rf_holdout)
  rf_holdout_vs_train[i] <- rf_holdout_mean / rf_train_accuracy_mean
  rf_holdout_vs_train_mean <- mean(rf_holdout_vs_train)
  rf_holdout_vs_train_range <- range(rf_holdout_vs_train)
  rf_holdout_vs_train_sd <- sd(rf_holdout_vs_train)

  rf_table <- rf_test_table + rf_validation_table
  rf_table_total <- rf_table_total + rf_table
  rf_table_sum_diag <- sum(diag(rf_table))

  rf_confusion_matrix <- caret::confusionMatrix(rf_table_total)
  rf_confusion_matrix_summary <- colMeans(rf_confusion_matrix$byClass, na.rm = TRUE)

  rf_true_positive_rate[i] <- rf_confusion_matrix_summary[1]
  rf_true_positive_rate_mean <- mean(rf_true_positive_rate[i])
  rf_true_negative_rate[i] <- rf_confusion_matrix_summary[2]
  rf_true_negative_rate_mean <- mean(rf_true_negative_rate)
  rf_false_negative_rate[i] <- 1 -  rf_true_positive_rate[i]
  rf_false_negative_rate_mean <- mean(rf_false_negative_rate)
  rf_false_positive_rate[i] <- 1 - rf_true_negative_rate[i]
  rf_false_positive_rate_mean <- mean(rf_false_positive_rate)
  rf_positive_pred_value[i] <- rf_confusion_matrix_summary[3]
  rf_positive_pred_value_mean <- mean(rf_positive_pred_value)
  rf_negative_pred_value[i] <- rf_confusion_matrix_summary[4]
  rf_negative_pred_value_mean <- mean(rf_negative_pred_value)
  rf_prevalence[i] <- rf_confusion_matrix_summary[8]
  rf_prevalence_mean <- mean(rf_prevalence)
  rf_detection_rate[i] <- rf_confusion_matrix_summary[9]
  rf_detection_rate_mean <- mean(rf_detection_rate)
  rf_detection_prevalence[i] <- rf_confusion_matrix_summary[10]
  rf_detection_prevalence_mean <- mean(rf_detection_prevalence)
  rf_classification_error[i] <- 1 - rf_holdout[i]
  rf_classification_error_mean <- mean(rf_classification_error)
  rf_F1_score[i] <- rf_confusion_matrix_summary[7]
  rf_F1_score_mean <- mean(rf_F1_score[i])

  rf_end <- Sys.time()
  rf_duration[i] <- rf_end - rf_start
  rf_duration_mean <- mean(rf_duration)
  rf_duration_sd <- sd(rf_duration)

  #### 9. Ranger Model ####
  ranger_start <- Sys.time()
  message("Working on Ranger analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ranger_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "RangerModel")
  }
  if(set_seed == "N"){
    ranger_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "RangerModel")
  }
  ranger_train_predict <- predict(object = ranger_train_fit, newdata = train01)
  ranger_train_table <- table(ranger_train_predict, y_train)
  ranger_train_accuracy[i] <- sum(diag(ranger_train_table)) / sum(ranger_train_table)
  ranger_train_accuracy_mean <- mean(ranger_train_accuracy)
  ranger_train_pred <- ranger_train_predict
  ranger_train_mean <- mean(diag(ranger_train_table)) / sum(ranger_train_table)
  ranger_train_sd <- sd(diag(ranger_train_table)) / sd(ranger_train_table)
  sum_diag_train_ranger <- sum(diag(ranger_train_table))
  ranger_train_prop <- diag(prop.table(ranger_train_table, margin = 1))

  ranger_test_predict <- predict(object = ranger_train_fit, newdata = test01)
  ranger_test_table <- table(ranger_test_predict, y_test)
  ranger_test_accuracy[i] <- sum(diag(ranger_test_table)) / sum(ranger_test_table)
  ranger_test_accuracy_mean <- mean(ranger_test_accuracy)
  ranger_test_pred <- ranger_test_predict
  ranger_test_mean <- mean(diag(ranger_test_table)) / sum(ranger_test_table)
  ranger_test_sd <- sd(diag(ranger_test_table)) / sd(ranger_test_table)
  sum_diag_test_ranger <- sum(diag(ranger_test_table))
  ranger_test_prop <- diag(prop.table(ranger_test_table, margin = 1))

  ranger_validation_predict <- predict(object = ranger_train_fit, newdata = validation01)
  ranger_validation_table <- table(ranger_validation_predict, y_validation)
  ranger_validation_accuracy[i] <- sum(diag(ranger_validation_table)) / sum(ranger_validation_table)
  ranger_validation_accuracy_mean <- mean(ranger_validation_accuracy)
  ranger_validation_pred <- ranger_validation_predict
  ranger_validation_mean <- mean(diag(ranger_validation_table)) / sum(ranger_validation_table)
  ranger_validation_sd <- sd(diag(ranger_validation_table)) / sd(ranger_validation_table)
  sum_diag_validation_ranger <- sum(diag(ranger_validation_table))
  ranger_validation_prop <- diag(prop.table(ranger_validation_table, margin = 1))

  ranger_holdout[i] <- mean(c(ranger_test_accuracy_mean, ranger_validation_accuracy_mean))
  ranger_holdout_mean <- mean(ranger_holdout)
  ranger_residuals[i] <- 1 - ranger_holdout[i]
  ranger_residuals_mean <- mean(ranger_residuals)
  ranger_holdout_sd <- sd(ranger_holdout)
  ranger_holdout_vs_train[i] <- ranger_holdout_mean / ranger_train_accuracy_mean
  ranger_holdout_vs_train_mean <- mean(ranger_holdout_vs_train)
  ranger_holdout_vs_train_range <- range(ranger_holdout_vs_train)
  ranger_holdout_vs_train_sd <- sd(ranger_holdout_vs_train)

  ranger_table <- ranger_test_table + ranger_validation_table
  ranger_table_total <- ranger_table_total + ranger_table
  ranger_table_sum_diag <- sum(diag(ranger_table))

  ranger_confusion_matrix <- caret::confusionMatrix(ranger_table_total)
  ranger_confusion_matrix_summary <- colMeans(ranger_confusion_matrix$byClass, na.rm = TRUE)

  ranger_true_positive_rate[i] <- ranger_confusion_matrix_summary[1]
  ranger_true_positive_rate_mean <- mean(ranger_true_positive_rate[i])
  ranger_true_negative_rate[i] <- ranger_confusion_matrix_summary[2]
  ranger_true_negative_rate_mean <- mean(ranger_true_negative_rate)
  ranger_false_negative_rate[i] <- 1 -  ranger_true_positive_rate[i]
  ranger_false_negative_rate_mean <- mean(ranger_false_negative_rate)
  ranger_false_positive_rate[i] <- 1 - ranger_true_negative_rate[i]
  ranger_false_positive_rate_mean <- mean(ranger_false_positive_rate)
  ranger_positive_pred_value[i] <- ranger_confusion_matrix_summary[3]
  ranger_positive_pred_value_mean <- mean(ranger_positive_pred_value)
  ranger_negative_pred_value[i] <- ranger_confusion_matrix_summary[4]
  ranger_negative_pred_value_mean <- mean(ranger_negative_pred_value)
  ranger_prevalence[i] <- ranger_confusion_matrix_summary[8]
  ranger_prevalence_mean <- mean(ranger_prevalence)
  ranger_detection_rate[i] <- ranger_confusion_matrix_summary[9]
  ranger_detection_rate_mean <- mean(ranger_detection_rate)
  ranger_detection_prevalence[i] <- ranger_confusion_matrix_summary[10]
  ranger_detection_prevalence_mean <- mean(ranger_detection_prevalence)
  ranger_classification_error[i] <- 1 - ranger_holdout[i]
  ranger_classification_error_mean <- mean(ranger_classification_error)
  ranger_F1_score[i] <- ranger_confusion_matrix_summary[7]
  ranger_F1_score_mean <- mean(ranger_F1_score[i])

  ranger_end <- Sys.time()
  ranger_duration[i] <- ranger_end - ranger_start
  ranger_duration_mean <- mean(ranger_duration)
  ranger_duration_sd <- sd(ranger_duration)

  #### 10. RPart Model ####
  rpart_start <- Sys.time()
  message("Working on RPart analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    rpart_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "RPartModel")
  }
  if(set_seed == "N"){
    rpart_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "RPartModel")
  }
  rpart_train_predict <- predict(object = rpart_train_fit, newdata = train01)
  rpart_train_table <- table(rpart_train_predict, y_train)
  rpart_train_accuracy[i] <- sum(diag(rpart_train_table)) / sum(rpart_train_table)
  rpart_train_accuracy_mean <- mean(rpart_train_accuracy)
  rpart_train_pred <- rpart_train_predict
  rpart_train_mean <- mean(diag(rpart_train_table)) / sum(rpart_train_table)
  rpart_train_sd <- sd(diag(rpart_train_table)) / sd(rpart_train_table)
  sum_diag_train_rpart <- sum(diag(rpart_train_table))
  rpart_train_prop <- diag(prop.table(rpart_train_table, margin = 1))

  rpart_test_predict <- predict(object = rpart_train_fit, newdata = test01)
  rpart_test_table <- table(rpart_test_predict, y_test)
  rpart_test_accuracy[i] <- sum(diag(rpart_test_table)) / sum(rpart_test_table)
  rpart_test_accuracy_mean <- mean(rpart_test_accuracy)
  rpart_test_pred <- rpart_test_predict
  rpart_test_mean <- mean(diag(rpart_test_table)) / sum(rpart_test_table)
  rpart_test_sd <- sd(diag(rpart_test_table)) / sd(rpart_test_table)
  sum_diag_test_rpart <- sum(diag(rpart_test_table))
  rpart_test_prop <- diag(prop.table(rpart_test_table, margin = 1))

  rpart_validation_predict <- predict(object = rpart_train_fit, newdata = validation01)
  rpart_validation_table <- table(rpart_validation_predict, y_validation)
  rpart_validation_accuracy[i] <- sum(diag(rpart_validation_table)) / sum(rpart_validation_table)
  rpart_validation_accuracy_mean <- mean(rpart_validation_accuracy)
  rpart_validation_pred <- rpart_validation_predict
  rpart_validation_mean <- mean(diag(rpart_validation_table)) / sum(rpart_validation_table)
  rpart_validation_sd <- sd(diag(rpart_validation_table)) / sd(rpart_validation_table)
  sum_diag_validation_rpart <- sum(diag(rpart_validation_table))
  rpart_validation_prop <- diag(prop.table(rpart_validation_table, margin = 1))

  rpart_holdout[i] <- mean(c(rpart_test_accuracy_mean, rpart_validation_accuracy_mean))
  rpart_holdout_mean <- mean(rpart_holdout)
  rpart_residuals[i] <- 1 - rpart_holdout[i]
  rpart_residuals_mean <- mean(rpart_residuals)
  rpart_holdout_sd <- sd(rpart_holdout)
  rpart_holdout_vs_train[i] <- rpart_holdout_mean / rpart_train_accuracy_mean
  rpart_holdout_vs_train_mean <- mean(rpart_holdout_vs_train)
  rpart_holdout_vs_train_range <- range(rpart_holdout_vs_train)
  rpart_holdout_vs_train_sd <- sd(rpart_holdout_vs_train)

  rpart_table <- rpart_test_table + rpart_validation_table
  rpart_table_total <- rpart_table_total + rpart_table
  rpart_table_sum_diag <- sum(diag(rpart_table))

  rpart_confusion_matrix <- caret::confusionMatrix(rpart_table_total)
  rpart_confusion_matrix_summary <- colMeans(rpart_confusion_matrix$byClass, na.rm = TRUE)

  rpart_true_positive_rate[i] <- rpart_confusion_matrix_summary[1]
  rpart_true_positive_rate_mean <- mean(rpart_true_positive_rate[i])
  rpart_true_negative_rate[i] <- rpart_confusion_matrix_summary[2]
  rpart_true_negative_rate_mean <- mean(rpart_true_negative_rate)
  rpart_false_negative_rate[i] <- 1 -  rpart_true_positive_rate[i]
  rpart_false_negative_rate_mean <- mean(rpart_false_negative_rate)
  rpart_false_positive_rate[i] <- 1 - rpart_true_negative_rate[i]
  rpart_false_positive_rate_mean <- mean(rpart_false_positive_rate)
  rpart_positive_pred_value[i] <- rpart_confusion_matrix_summary[3]
  rpart_positive_pred_value_mean <- mean(rpart_positive_pred_value)
  rpart_negative_pred_value[i] <- rpart_confusion_matrix_summary[4]
  rpart_negative_pred_value_mean <- mean(rpart_negative_pred_value)
  rpart_prevalence[i] <- rpart_confusion_matrix_summary[8]
  rpart_prevalence_mean <- mean(rpart_prevalence)
  rpart_detection_rate[i] <- rpart_confusion_matrix_summary[9]
  rpart_detection_rate_mean <- mean(rpart_detection_rate)
  rpart_detection_prevalence[i] <- rpart_confusion_matrix_summary[10]
  rpart_detection_prevalence_mean <- mean(rpart_detection_prevalence)
  rpart_classification_error[i] <- 1 - rpart_holdout[i]
  rpart_classification_error_mean <- mean(rpart_classification_error)
  rpart_F1_score[i] <- rpart_confusion_matrix_summary[7]
  rpart_F1_score_mean <- mean(rpart_F1_score[i])

  rpart_end <- Sys.time()
  rpart_duration[i] <- rpart_end - rpart_start
  rpart_duration_mean <- mean(rpart_duration)
  rpart_duration_sd <- sd(rpart_duration)


  #### 11. Support Vector Machines ####
  svm_start <- Sys.time()
  message("Working on Support Vector Machine analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    svm_train_fit <- e1071::svm(y_train ~ ., data = train, kernel = "radial", gamma = 1, cost = 1)
  }
  if(set_seed == "N"){
    svm_train_fit <- e1071::svm(y_train ~ ., data = train, kernel = "radial", gamma = 1, cost = 1)
  }
  svm_train_pred <- predict(svm_train_fit, train, type = "class")
  svm_train_table <- table(svm_train_pred, y_train)
  svm_train_accuracy[i] <- sum(diag(svm_train_table)) / sum(svm_train_table)
  svm_train_accuracy_mean <- mean(svm_train_accuracy)
  svm_train_diag <- sum(diag(svm_train_table))
  svm_train_mean <- mean(diag(svm_train_table)) / mean(svm_train_table)
  svm_train_sd <- sd(diag(svm_train_table)) / sd(svm_train_table)
  sum_diag_train_svm <- sum(diag(svm_train_table))
  svm_train_prop <- diag(prop.table(svm_train_table, margin = 1))

  svm_test_pred <- predict(svm_train_fit, test, type = "class")
  svm_test_table <- table(svm_test_pred, y_test)
  svm_test_accuracy[i] <- sum(diag(svm_test_table)) / sum(svm_test_table)
  svm_test_accuracy_mean <- mean(svm_test_accuracy)
  svm_test_diag <- sum(diag(svm_test_table))
  svm_test_mean <- mean(diag(svm_test_table)) / mean(svm_test_table)
  svm_test_sd <- sd(diag(svm_test_table)) / sd(svm_test_table)
  sum_diag_test_svm <- sum(diag(svm_test_table))
  svm_test_prop <- diag(prop.table(svm_test_table, margin = 1))

  svm_validation_pred <- predict(svm_train_fit, validation, type = "class")
  svm_validation_table <- table(svm_validation_pred, y_validation)
  svm_validation_accuracy[i] <- sum(diag(svm_validation_table)) / sum(svm_validation_table)
  svm_validation_accuracy_mean <- mean(svm_validation_accuracy)
  svm_validation_diag <- sum(diag(svm_validation_table))
  svm_validation_mean <- mean(diag(svm_validation_table)) / mean(svm_validation_table)
  svm_validation_sd <- sd(diag(svm_validation_table)) / sd(svm_validation_table)
  sum_diag_validation_svm <- sum(diag(svm_validation_table))
  svm_validation_prop <- diag(prop.table(svm_validation_table, margin = 1))
  svm_holdout[i] <- mean(c(svm_test_accuracy_mean, svm_validation_accuracy_mean))
  svm_holdout_mean <- mean(svm_holdout)
  svm_residuals[i] <- 1 - svm_holdout[i]
  svm_residuals_mean <- mean(svm_residuals)
  svm_holdout_sd <- sd(svm_holdout)
  svm_holdout_vs_train[i] <- svm_holdout_mean / svm_train_accuracy_mean
  svm_holdout_vs_train_mean <- mean(svm_holdout_vs_train)
  svm_holdout_vs_train_range <- range(svm_holdout_vs_train)
  svm_holdout_vs_train_sd <- sd(svm_holdout_vs_train)

  svm_table <- svm_test_table + svm_validation_table
  svm_table_total <- svm_table_total + svm_table
  svm_table_sum_diag <- sum(diag(svm_table))

  svm_confusion_matrix <- caret::confusionMatrix(svm_table_total)
  svm_confusion_matrix_summary <- colMeans(svm_confusion_matrix$byClass, na.rm = TRUE)

  svm_true_positive_rate[i] <- svm_confusion_matrix_summary[1]
  svm_true_positive_rate_mean <- mean(svm_true_positive_rate[i])
  svm_true_negative_rate[i] <- svm_confusion_matrix_summary[2]
  svm_true_negative_rate_mean <- mean(svm_true_negative_rate)
  svm_false_negative_rate[i] <- 1 -  svm_true_positive_rate[i]
  svm_false_negative_rate_mean <- mean(svm_false_negative_rate)
  svm_false_positive_rate[i] <- 1 - svm_true_negative_rate[i]
  svm_false_positive_rate_mean <- mean(svm_false_positive_rate)
  svm_positive_pred_value[i] <- svm_confusion_matrix_summary[3]
  svm_positive_pred_value_mean <- mean(svm_positive_pred_value)
  svm_negative_pred_value[i] <- svm_confusion_matrix_summary[4]
  svm_negative_pred_value_mean <- mean(svm_negative_pred_value)
  svm_prevalence[i] <- svm_confusion_matrix_summary[8]
  svm_prevalence_mean <- mean(svm_prevalence)
  svm_detection_rate[i] <- svm_confusion_matrix_summary[9]
  svm_detection_rate_mean <- mean(svm_detection_rate)
  svm_detection_prevalence[i] <- svm_confusion_matrix_summary[10]
  svm_detection_prevalence_mean <- mean(svm_detection_prevalence)
  svm_classification_error[i] <- 1 - svm_holdout[i]
  svm_classification_error_mean <- mean(svm_classification_error)
  svm_F1_score[i] <- svm_confusion_matrix_summary[7]
  svm_F1_score_mean <- mean(svm_F1_score[i])

  svm_end <- Sys.time()
  svm_duration[i] <- svm_end - svm_start
  svm_duration_mean <- mean(svm_duration)
  svm_duration_sd <- sd(svm_duration)


  #### 12. Trees ####
  tree_start <- Sys.time()
  message("Working on Trees analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    tree_train_fit <- tree::tree(y_train ~ ., data = train)
  }
  if(set_seed == "N"){
    tree_train_fit <- tree::tree(y_train ~ ., data = train)
  }
  tree_train_pred <- predict(tree_train_fit, train, type = "class")
  tree_train_table <- table(tree_train_pred, y_train)
  tree_train_accuracy[i] <- sum(diag(tree_train_table)) / sum(tree_train_table)
  tree_train_accuracy_mean <- mean(tree_train_accuracy)
  tree_train_diag <- sum(diag(tree_train_table))
  tree_train_mean <- mean(diag(tree_train_table)) / mean(tree_train_table)
  tree_train_sd <- sd(diag(tree_train_table)) / sd(tree_train_table)
  sum_diag_train_tree <- sum(diag(tree_train_table))
  tree_train_prop <- diag(prop.table(tree_train_table, margin = 1))

  tree_test_pred <- predict(tree_train_fit, test, type = "class")
  tree_test_table <- table(tree_test_pred, y_test)
  tree_test_accuracy[i] <- sum(diag(tree_test_table)) / sum(tree_test_table)
  tree_test_accuracy_mean <- mean(tree_test_accuracy)
  tree_test_diag <- sum(diag(tree_test_table))
  tree_test_mean <- mean(diag(tree_test_table)) / mean(tree_test_table)
  tree_test_sd <- sd(diag(tree_test_table)) / sd(tree_test_table)
  sum_diag_test_tree <- sum(diag(tree_test_table))
  tree_test_prop <- diag(prop.table(tree_test_table, margin = 1))

  tree_validation_pred <- predict(tree_train_fit, validation, type = "class")
  tree_validation_table <- table(tree_validation_pred, y_validation)
  tree_validation_accuracy[i] <- sum(diag(tree_validation_table)) / sum(tree_validation_table)
  tree_validation_accuracy_mean <- mean(tree_validation_accuracy)
  tree_validation_diag <- sum(diag(tree_validation_table))
  tree_validation_mean <- mean(diag(tree_validation_table)) / mean(tree_validation_table)
  tree_validation_sd <- sd(diag(tree_validation_table)) / sd(tree_validation_table)
  sum_diag_validation_tree <- sum(diag(tree_validation_table))
  tree_validation_prop <- diag(prop.table(tree_validation_table, margin = 1))

  tree_holdout[i] <- mean(c(tree_test_accuracy_mean, tree_validation_accuracy_mean))
  tree_holdout_mean <- mean(tree_holdout)
  tree_residuals[i] <- 1 - tree_holdout[i]
  tree_residuals_mean <- mean(tree_residuals)
  tree_holdout_sd <- sd(tree_holdout)
  tree_holdout_vs_train[i] <- tree_holdout_mean / tree_train_accuracy_mean
  tree_holdout_vs_train_mean <- mean(tree_holdout_vs_train)
  tree_holdout_vs_train_range <- range(tree_holdout_vs_train)
  tree_holdout_vs_train_sd <- sd(tree_holdout_vs_train)

  tree_table <- tree_test_table + tree_validation_table
  tree_table_total <- tree_table_total + tree_table
  tree_table_sum_diag <- sum(diag(tree_table))

  tree_confusion_matrix <- caret::confusionMatrix(tree_table_total)
  tree_confusion_matrix_summary <- colMeans(tree_confusion_matrix$byClass, na.rm = TRUE)

  tree_true_positive_rate[i] <- tree_confusion_matrix_summary[1]
  tree_true_positive_rate_mean <- mean(tree_true_positive_rate[i])
  tree_true_negative_rate[i] <- tree_confusion_matrix_summary[2]
  tree_true_negative_rate_mean <- mean(tree_true_negative_rate)
  tree_false_negative_rate[i] <- 1 -  tree_true_positive_rate[i]
  tree_false_negative_rate_mean <- mean(tree_false_negative_rate)
  tree_false_positive_rate[i] <- 1 - tree_true_negative_rate[i]
  tree_false_positive_rate_mean <- mean(tree_false_positive_rate)
  tree_positive_pred_value[i] <- tree_confusion_matrix_summary[3]
  tree_positive_pred_value_mean <- mean(tree_positive_pred_value)
  tree_negative_pred_value[i] <- tree_confusion_matrix_summary[4]
  tree_negative_pred_value_mean <- mean(tree_negative_pred_value)
  tree_prevalence[i] <- tree_confusion_matrix_summary[8]
  tree_prevalence_mean <- mean(tree_prevalence)
  tree_detection_rate[i] <- tree_confusion_matrix_summary[9]
  tree_detection_rate_mean <- mean(tree_detection_rate)
  tree_detection_prevalence[i] <- tree_confusion_matrix_summary[10]
  tree_detection_prevalence_mean <- mean(tree_detection_prevalence)
  tree_classification_error[i] <- 1 - tree_holdout[i]
  tree_classification_error_mean <- mean(tree_classification_error)
  tree_F1_score[i] <- tree_confusion_matrix_summary[7]
  tree_F1_score_mean <- mean(tree_F1_score[i])

  tree_end <- Sys.time()
  tree_duration[i] <- tree_end - tree_start
  tree_duration_mean <- mean(tree_duration)
  tree_duration_sd <- sd(tree_duration)



  #### Ensembles start here ####
  ensemble1 <- data.frame(
    "Bagged_Random_Forest" = c(bag_rf_test_pred, bag_rf_validation_pred),
    "Bagging" = c(bagging_test_pred, bagging_validation_pred),
    "C50" = c(C50_test_pred, C50_validation_pred),
    "Linear" = c(linear_test_pred, linear_validation_pred),
    "Naive_Bayes" = c(n_bayes_test_pred, n_bayes_validation_pred),
    "Partial_Least_Squares" = c(pls_test_pred, pls_validation_pred),
    "Penalized_Discriminant_Analysis" = c(pda_test_pred, pda_validation_pred),
    "Random_Forest" = c(rf_test_pred, rf_validation_pred),
    "Ranger" = c(ranger_test_pred, ranger_validation_pred),
    "RPart" = c(rpart_test_pred, rpart_validation_pred),
    "Support_Vector_Machines" = c(svm_test_pred, svm_validation_pred),
    "Trees" = c(tree_test_pred, tree_validation_pred)
  )

  ensemble_row_numbers <- as.numeric(row.names(ensemble1))
  ensemble1$y <- df[ensemble_row_numbers, "y"]

  ensemble1 <- ensemble1[complete.cases(ensemble1), ]

  head_ensemble <- reactable::reactable(head(ensemble1),
                                        searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                        striped = TRUE, highlight = TRUE, resizable = TRUE
  )%>%
    reactablefmtr::add_title("Head of the ensemble")

  if(save_all_plots == "Y"){
    reactablefmtr::save_reactable_test(head_ensemble, "Head_of_the_ensemble.html")
  }

  ensemble_index <- sample(c(1:3), nrow(ensemble1), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))
  ensemble_train <- ensemble1[ensemble_index == 1, ]
  ensemble_test <- ensemble1[ensemble_index == 2, ]
  ensemble_validation <- ensemble1[ensemble_index == 3, ]
  ensemble_y_train <- ensemble_train$y
  ensemble_y_test <- ensemble_test$y
  ensemble_y_validation <- ensemble_validation$y

  print(noquote(""))
  print("Working on the Ensembles section")
  print(noquote(""))

  #### 13. Ensemble Bagged CART ####
  ensemble_bag_cart_start <- Sys.time()
  message("Working on Ensemble Bagged CART analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_bag_cart_train_fit <- ipred::bagging(y ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_bag_cart_train_fit <- ipred::bagging(y ~ ., data = ensemble_train)
  }
  ensemble_bag_cart_train_pred <- predict(ensemble_bag_cart_train_fit, ensemble_train)
  ensemble_bag_cart_train_table <- table(ensemble_bag_cart_train_pred, ensemble_train$y)
  ensemble_bag_cart_train_accuracy[i] <- sum(diag(ensemble_bag_cart_train_table)) / sum(ensemble_bag_cart_train_table)
  ensemble_bag_cart_train_accuracy_mean <- mean(ensemble_bag_cart_train_accuracy)
  ensemble_bag_cart_train_mean <- mean(diag(ensemble_bag_cart_train_table)) / mean(ensemble_bag_cart_train_table)
  ensemble_bag_cart_train_sd <- sd(diag(ensemble_bag_cart_train_table)) / sd(ensemble_bag_cart_train_table)
  ensemble_sum_diag_bag_train_cart <- sum(diag(ensemble_bag_cart_train_table))
  ensemble_bag_cart_train_prop <- diag(prop.table(ensemble_bag_cart_train_table, margin = 1))

  ensemble_bag_cart_test_pred <- predict(ensemble_bag_cart_train_fit, ensemble_test)
  ensemble_bag_cart_test_table <- table(ensemble_bag_cart_test_pred, ensemble_test$y)
  ensemble_bag_cart_test_accuracy[i] <- sum(diag(ensemble_bag_cart_test_table)) / sum(ensemble_bag_cart_test_table)
  ensemble_bag_cart_test_accuracy_mean <- mean(ensemble_bag_cart_test_accuracy)
  ensemble_bag_cart_test_mean <- mean(diag(ensemble_bag_cart_test_table)) / mean(ensemble_bag_cart_test_table)
  ensemble_bag_cart_test_sd <- sd(diag(ensemble_bag_cart_test_table)) / sd(ensemble_bag_cart_test_table)
  ensemble_sum_diag_bag_test_cart <- sum(diag(ensemble_bag_cart_test_table))
  ensemble_bag_cart_test_prop <- diag(prop.table(ensemble_bag_cart_test_table, margin = 1))

  ensemble_bag_cart_validation_pred <- predict(ensemble_bag_cart_train_fit, ensemble_validation)
  ensemble_bag_cart_validation_table <- table(ensemble_bag_cart_validation_pred, ensemble_validation$y)
  ensemble_bag_cart_validation_accuracy[i] <- sum(diag(ensemble_bag_cart_validation_table)) / sum(ensemble_bag_cart_validation_table)
  ensemble_bag_cart_validation_accuracy_mean <- mean(ensemble_bag_cart_validation_accuracy)
  ensemble_bag_cart_validation_mean <- mean(diag(ensemble_bag_cart_validation_table)) / mean(ensemble_bag_cart_validation_table)
  ensemble_bag_cart_validation_sd <- sd(diag(ensemble_bag_cart_validation_table)) / sd(ensemble_bag_cart_validation_table)
  ensemble_sum_diag_bag_validation_cart <- sum(diag(ensemble_bag_cart_validation_table))
  ensemble_bag_cart_validation_prop <- diag(prop.table(ensemble_bag_cart_validation_table, margin = 1))

  ensemble_bag_cart_holdout[i] <- mean(c(ensemble_bag_cart_test_accuracy_mean, ensemble_bag_cart_validation_accuracy_mean))
  ensemble_bag_cart_holdout_mean <- mean(ensemble_bag_cart_holdout)
  ensemble_bag_cart_residuals[i] <- 1-ensemble_bag_cart_holdout[i]
  ensemble_bag_cart_residuals_mean <- mean(ensemble_bag_cart_residuals)
  ensemble_bag_cart_holdout_sd <- sd(ensemble_bag_cart_holdout)
  ensemble_bag_cart_holdout_vs_train[i] <- ensemble_bag_cart_holdout_mean / ensemble_bag_cart_train_accuracy_mean
  ensemble_bag_cart_holdout_vs_train_mean <- mean(ensemble_bag_cart_holdout_vs_train)
  ensemble_bag_cart_holdout_vs_train_range <- range(ensemble_bag_cart_holdout_vs_train)
  ensemble_bag_cart_holdout_vs_train_sd <- sd(ensemble_bag_cart_holdout_vs_train)

  ensemble_bag_cart_table <- ensemble_bag_cart_test_table + ensemble_bag_cart_validation_table
  ensemble_bag_cart_table_total <- ensemble_bag_cart_table_total + ensemble_bag_cart_table
  ensemble_bag_cart_table_sum_diag <- sum(diag(ensemble_bag_cart_table))

  ensemble_bag_cart_confusion_matrix <- caret::confusionMatrix(ensemble_bag_cart_table_total)
  ensemble_bag_cart_confusion_matrix_summary <- colMeans(ensemble_bag_cart_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_bag_cart_true_positive_rate[i] <- ensemble_bag_cart_confusion_matrix_summary[1]
  ensemble_bag_cart_true_positive_rate_mean <- mean(ensemble_bag_cart_true_positive_rate[i])
  ensemble_bag_cart_true_negative_rate[i] <- ensemble_bag_cart_confusion_matrix_summary[2]
  ensemble_bag_cart_true_negative_rate_mean <- mean(ensemble_bag_cart_true_negative_rate)
  ensemble_bag_cart_false_negative_rate[i] <- 1 -  ensemble_bag_cart_true_positive_rate[i]
  ensemble_bag_cart_false_negative_rate_mean <- mean(ensemble_bag_cart_false_negative_rate)
  ensemble_bag_cart_false_positive_rate[i] <- 1 - ensemble_bag_cart_true_negative_rate[i]
  ensemble_bag_cart_false_positive_rate_mean <- mean(ensemble_bag_cart_false_positive_rate)
  ensemble_bag_cart_positive_pred_value[i] <- ensemble_bag_cart_confusion_matrix_summary[3]
  ensemble_bag_cart_positive_pred_value_mean <- mean(ensemble_bag_cart_positive_pred_value)
  ensemble_bag_cart_negative_pred_value[i] <- ensemble_bag_cart_confusion_matrix_summary[4]
  ensemble_bag_cart_negative_pred_value_mean <- mean(ensemble_bag_cart_negative_pred_value)
  ensemble_bag_cart_prevalence[i] <- ensemble_bag_cart_confusion_matrix_summary[8]
  ensemble_bag_cart_prevalence_mean <- mean(ensemble_bag_cart_prevalence)
  ensemble_bag_cart_detection_rate[i] <- ensemble_bag_cart_confusion_matrix_summary[9]
  ensemble_bag_cart_detection_rate_mean <- mean(ensemble_bag_cart_detection_rate)
  ensemble_bag_cart_detection_prevalence[i] <- ensemble_bag_cart_confusion_matrix_summary[10]
  ensemble_bag_cart_detection_prevalence_mean <- mean(ensemble_bag_cart_detection_prevalence)
  ensemble_bag_cart_classification_error[i] <- 1 - ensemble_bag_cart_holdout[i]
  ensemble_bag_cart_classification_error_mean <- mean(ensemble_bag_cart_classification_error)
  ensemble_bag_cart_F1_score[i] <- ensemble_bag_cart_confusion_matrix_summary[7]
  ensemble_bag_cart_F1_score_mean <- mean(ensemble_bag_cart_F1_score[i])

  ensemble_bag_cart_end <- Sys.time()
  ensemble_bag_cart_duration[i] <- ensemble_bag_cart_end - ensemble_bag_cart_start
  ensemble_bag_cart_duration_mean <- mean(ensemble_bag_cart_duration)
  ensemble_bag_cart_duration_sd <- sd(ensemble_bag_cart_duration)

  #### 14. Ensemble Bagged Random Forest ####
  ensemble_bag_rf_start <- Sys.time()
  message("Working on Ensemble Bagged Random Forest analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_bag_train_rf <- randomForest::randomForest(ensemble_y_train ~ ., data = ensemble_train, mtry = ncol(ensemble_train) - 1)
  }
  if(set_seed == "N"){
    ensemble_bag_train_rf <- randomForest::randomForest(ensemble_y_train ~ ., data = ensemble_train, mtry = ncol(ensemble_train) - 1)
  }
  ensemble_bag_rf_train_pred <- predict(ensemble_bag_train_rf, ensemble_train, type = "class")
  ensemble_bag_rf_train_table <- table(ensemble_bag_rf_train_pred, ensemble_train$y)
  ensemble_bag_rf_train_accuracy[i] <- sum(diag(ensemble_bag_rf_train_table)) / sum(ensemble_bag_rf_train_table)
  ensemble_bag_rf_train_accuracy_mean <- mean(ensemble_bag_rf_train_accuracy)
  ensemble_bag_rf_train_diag <- sum(diag(ensemble_bag_rf_train_table))
  ensemble_bag_rf_train_mean <- mean(diag(ensemble_bag_rf_train_table)) / mean(ensemble_bag_rf_train_table)
  ensemble_bag_rf_train_sd <- sd(diag(ensemble_bag_rf_train_table)) / sd(ensemble_bag_rf_train_table)
  sum_ensemble_bag_train_rf <- sum(diag(ensemble_bag_rf_train_table))
  ensemble_bag_rf_train_prop <- diag(prop.table(ensemble_bag_rf_train_table, margin = 1))

  ensemble_bag_rf_test_pred <- predict(ensemble_bag_train_rf, ensemble_test, type = "class")
  ensemble_bag_rf_test_table <- table(ensemble_bag_rf_test_pred, ensemble_test$y)
  ensemble_bag_rf_test_accuracy[i] <- sum(diag(ensemble_bag_rf_test_table)) / sum(ensemble_bag_rf_test_table)
  ensemble_bag_rf_test_accuracy_mean <- mean(ensemble_bag_rf_test_accuracy)
  ensemble_bag_rf_test_diag <- sum(diag(ensemble_bag_rf_test_table))
  ensemble_bag_rf_test_mean <- mean(diag(ensemble_bag_rf_test_table)) / mean(ensemble_bag_rf_test_table)
  ensemble_bag_rf_test_sd <- sd(diag(ensemble_bag_rf_test_table)) / sd(ensemble_bag_rf_test_table)
  sum_ensemble_bag_test_rf <- sum(diag(ensemble_bag_rf_test_table))
  ensemble_bag_rf_test_prop <- diag(prop.table(ensemble_bag_rf_test_table, margin = 1))

  ensemble_bag_rf_validation_pred <- predict(ensemble_bag_train_rf, ensemble_validation, type = "class")
  ensemble_bag_rf_validation_table <- table(ensemble_bag_rf_validation_pred, ensemble_validation$y)
  ensemble_bag_rf_validation_accuracy[i] <- sum(diag(ensemble_bag_rf_validation_table)) / sum(ensemble_bag_rf_validation_table)
  ensemble_bag_rf_validation_accuracy_mean <- mean(ensemble_bag_rf_validation_accuracy)
  ensemble_bag_rf_validation_diag <- sum(diag(ensemble_bag_rf_validation_table))
  ensemble_bag_rf_validation_mean <- mean(diag(ensemble_bag_rf_validation_table)) / mean(ensemble_bag_rf_validation_table)
  ensemble_bag_rf_validation_sd <- sd(diag(ensemble_bag_rf_validation_table)) / sd(ensemble_bag_rf_validation_table)
  sum_ensemble_bag_validation_rf <- sum(diag(ensemble_bag_rf_validation_table))
  ensemble_bag_rf_validation_prop <- diag(prop.table(ensemble_bag_rf_validation_table, margin = 1))

  ensemble_bag_rf_holdout[i] <- mean(c(ensemble_bag_rf_test_accuracy_mean, ensemble_bag_rf_validation_accuracy_mean))
  ensemble_bag_rf_holdout_mean <- mean(ensemble_bag_rf_holdout)
  ensemble_bag_rf_residuals[i] <- 1 - ensemble_bag_rf_holdout[i]
  ensemble_bag_rf_residuals_mean <- mean(ensemble_bag_rf_residuals)

  ensemble_bag_rf_holdout_sd <- sd(ensemble_bag_rf_holdout)
  ensemble_bag_rf_holdout_vs_train[i] <- ensemble_bag_rf_holdout_mean / ensemble_bag_rf_train_accuracy_mean
  ensemble_bag_rf_holdout_vs_train_mean <- mean(ensemble_bag_rf_holdout_vs_train)
  ensemble_bag_rf_holdout_vs_train_range <- range(ensemble_bag_rf_holdout_vs_train)
  ensemble_bag_rf_holdout_vs_train_sd <- sd(ensemble_bag_rf_holdout_vs_train)

  ensemble_bag_rf_table <- ensemble_bag_rf_test_table + ensemble_bag_rf_validation_table
  ensemble_bag_rf_table_total <- ensemble_bag_rf_table_total + ensemble_bag_rf_table
  ensemble_bag_rf_table_sum_diag <- sum(diag(ensemble_bag_rf_table))

  ensemble_bag_rf_confusion_matrix <- caret::confusionMatrix(ensemble_bag_rf_table_total)
  ensemble_bag_rf_confusion_matrix_summary <- colMeans(ensemble_bag_rf_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_bag_rf_true_positive_rate[i] <- ensemble_bag_rf_confusion_matrix_summary[1]
  ensemble_bag_rf_true_positive_rate_mean <- mean(ensemble_bag_rf_true_positive_rate[i])
  ensemble_bag_rf_true_negative_rate[i] <- ensemble_bag_rf_confusion_matrix_summary[2]
  ensemble_bag_rf_true_negative_rate_mean <- mean(ensemble_bag_rf_true_negative_rate)
  ensemble_bag_rf_false_negative_rate[i] <- 1 -  ensemble_bag_rf_true_positive_rate[i]
  ensemble_bag_rf_false_negative_rate_mean <- mean(ensemble_bag_rf_false_negative_rate)
  ensemble_bag_rf_false_positive_rate[i] <- 1 - ensemble_bag_rf_true_negative_rate[i]
  ensemble_bag_rf_false_positive_rate_mean <- mean(ensemble_bag_rf_false_positive_rate)
  ensemble_bag_rf_positive_pred_value[i] <- ensemble_bag_rf_confusion_matrix_summary[3]
  ensemble_bag_rf_positive_pred_value_mean <- mean(ensemble_bag_rf_positive_pred_value)
  ensemble_bag_rf_negative_pred_value[i] <- ensemble_bag_rf_confusion_matrix_summary[4]
  ensemble_bag_rf_negative_pred_value_mean <- mean(ensemble_bag_rf_negative_pred_value)
  ensemble_bag_rf_prevalence[i] <- ensemble_bag_rf_confusion_matrix_summary[8]
  ensemble_bag_rf_prevalence_mean <- mean(ensemble_bag_rf_prevalence)
  ensemble_bag_rf_detection_rate[i] <- ensemble_bag_rf_confusion_matrix_summary[9]
  ensemble_bag_rf_detection_rate_mean <- mean(ensemble_bag_rf_detection_rate)
  ensemble_bag_rf_detection_prevalence[i] <- ensemble_bag_rf_confusion_matrix_summary[10]
  ensemble_bag_rf_detection_prevalence_mean <- mean(ensemble_bag_rf_detection_prevalence)
  ensemble_bag_rf_classification_error[i] <- 1 - ensemble_bag_rf_holdout[i]
  ensemble_bag_rf_classification_error_mean <- mean(ensemble_bag_rf_classification_error)
  ensemble_bag_rf_F1_score[i] <- ensemble_bag_rf_confusion_matrix_summary[7]
  ensemble_bag_rf_F1_score_mean <- mean(ensemble_bag_rf_F1_score[i])

  ensemble_bag_rf_end <- Sys.time()
  ensemble_bag_rf_duration[i] <- ensemble_bag_rf_end - ensemble_bag_rf_start
  ensemble_bag_rf_duration_mean <- mean(ensemble_bag_rf_duration)
  ensemble_bag_rf_duration_sd <- sd(ensemble_bag_rf_duration)

  #### 15. Ensemble C50 ####
  ensemble_C50_start <- Sys.time()
  message("Working on Ensemble C50 analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_C50_train_fit <- C50::C5.0(ensemble_y_train ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_C50_train_fit <- C50::C5.0(ensemble_y_train ~ ., data = ensemble_train)
  }
  ensemble_C50_train_pred <- predict(ensemble_C50_train_fit, ensemble_train)
  ensemble_C50_train_table <- table(ensemble_C50_train_pred, ensemble_y_train)
  ensemble_C50_train_accuracy[i] <- sum(diag(ensemble_C50_train_table)) / sum(ensemble_C50_train_table)
  ensemble_C50_train_accuracy_mean <- mean(ensemble_C50_train_accuracy)
  ensemble_C50_train_mean <- mean(diag(ensemble_C50_train_table)) / mean(ensemble_C50_train_table)
  ensemble_C50_train_sd <- sd(diag(ensemble_C50_train_table)) / sd(ensemble_C50_train_table)
  sum_diag_ensemble_train_C50 <- sum(diag(ensemble_C50_train_table))
  ensemble_C50_train_prop <- diag(prop.table(ensemble_C50_train_table, margin = 1))

  ensemble_C50_test_pred <- predict(ensemble_C50_train_fit, ensemble_test)
  ensemble_C50_test_table <- table(ensemble_C50_test_pred, ensemble_y_test)
  ensemble_C50_test_accuracy[i] <- sum(diag(ensemble_C50_test_table)) / sum(ensemble_C50_test_table)
  ensemble_C50_test_accuracy_mean <- mean(ensemble_C50_test_accuracy)
  ensemble_C50_test_mean <- mean(diag(ensemble_C50_test_table)) / mean(ensemble_C50_test_table)
  ensemble_C50_test_sd <- sd(diag(ensemble_C50_test_table)) / sd(ensemble_C50_test_table)
  sum_diag_ensemble_test_C50 <- sum(diag(ensemble_C50_test_table))
  ensemble_C50_test_prop <- diag(prop.table(ensemble_C50_test_table, margin = 1))

  ensemble_C50_validation_pred <- predict(ensemble_C50_train_fit, ensemble_validation)
  ensemble_C50_validation_table <- table(ensemble_C50_validation_pred, ensemble_y_validation)
  ensemble_C50_validation_accuracy[i] <- sum(diag(ensemble_C50_validation_table)) / sum(ensemble_C50_validation_table)
  ensemble_C50_validation_accuracy_mean <- mean(ensemble_C50_validation_accuracy)
  ensemble_C50_validation_mean <- mean(diag(ensemble_C50_validation_table)) / mean(ensemble_C50_validation_table)
  ensemble_C50_validation_sd <- sd(diag(ensemble_C50_validation_table)) / sd(ensemble_C50_validation_table)
  sum_diag_ensemble_validation_C50 <- sum(diag(ensemble_C50_validation_table))
  ensemble_C50_validation_prop <- diag(prop.table(ensemble_C50_validation_table, margin = 1))

  ensemble_C50_holdout[i] <- mean(c(ensemble_C50_test_accuracy_mean, ensemble_C50_validation_accuracy_mean))
  ensemble_C50_holdout_mean <- mean(ensemble_C50_holdout)
  ensemble_C50_residuals[i] <- 1 - ensemble_C50_holdout[i]
  ensemble_C50_residuals_mean <- mean(ensemble_C50_residuals)
  ensemble_C50_holdout_sd <- sd(ensemble_C50_holdout)
  ensemble_C50_holdout_vs_train[i] <- ensemble_C50_holdout_mean / ensemble_C50_train_accuracy_mean
  ensemble_C50_holdout_vs_train_mean <- mean(ensemble_C50_holdout_vs_train)
  ensemble_C50_holdout_vs_train_range <- range(ensemble_C50_holdout_vs_train)
  ensemble_C50_holdout_vs_train_sd <- sd(ensemble_C50_holdout_vs_train)

  ensemble_C50_table <- ensemble_C50_test_table + ensemble_C50_validation_table
  ensemble_C50_table_total <- ensemble_C50_table_total + ensemble_C50_table
  ensemble_C50_table_sum_diag <- sum(diag(ensemble_C50_table))

  ensemble_C50_confusion_matrix <- caret::confusionMatrix(ensemble_C50_table_total)
  ensemble_C50_confusion_matrix_summary <- colMeans(ensemble_C50_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_C50_true_positive_rate[i] <- ensemble_C50_confusion_matrix_summary[1]
  ensemble_C50_true_positive_rate_mean <- mean(ensemble_C50_true_positive_rate[i])
  ensemble_C50_true_negative_rate[i] <- ensemble_C50_confusion_matrix_summary[2]
  ensemble_C50_true_negative_rate_mean <- mean(ensemble_C50_true_negative_rate)
  ensemble_C50_false_negative_rate[i] <- 1 -  ensemble_C50_true_positive_rate[i]
  ensemble_C50_false_negative_rate_mean <- mean(ensemble_C50_false_negative_rate)
  ensemble_C50_false_positive_rate[i] <- 1 - ensemble_C50_true_negative_rate[i]
  ensemble_C50_false_positive_rate_mean <- mean(ensemble_C50_false_positive_rate)
  ensemble_C50_positive_pred_value[i] <- ensemble_C50_confusion_matrix_summary[3]
  ensemble_C50_positive_pred_value_mean <- mean(ensemble_C50_positive_pred_value)
  ensemble_C50_negative_pred_value[i] <- ensemble_C50_confusion_matrix_summary[4]
  ensemble_C50_negative_pred_value_mean <- mean(ensemble_C50_negative_pred_value)
  ensemble_C50_prevalence[i] <- ensemble_C50_confusion_matrix_summary[8]
  ensemble_C50_prevalence_mean <- mean(ensemble_C50_prevalence)
  ensemble_C50_detection_rate[i] <- ensemble_C50_confusion_matrix_summary[9]
  ensemble_C50_detection_rate_mean <- mean(ensemble_C50_detection_rate)
  ensemble_C50_detection_prevalence[i] <- ensemble_C50_confusion_matrix_summary[10]
  ensemble_C50_detection_prevalence_mean <- mean(ensemble_C50_detection_prevalence)
  ensemble_C50_classification_error[i] <- 1 - ensemble_C50_holdout[i]
  ensemble_C50_classification_error_mean <- mean(ensemble_C50_classification_error)
  ensemble_C50_F1_score[i] <- ensemble_C50_confusion_matrix_summary[7]
  ensemble_C50_F1_score_mean <- mean(ensemble_C50_F1_score[i])

  ensemble_C50_end <- Sys.time()
  ensemble_C50_duration[i] <- ensemble_C50_end - ensemble_C50_start
  ensemble_C50_duration_mean <- mean(ensemble_C50_duration)
  ensemble_C50_duration_sd <- sd(ensemble_C50_duration)


  #### 16. Ensemble Naive Bayes ####
  ensemble_n_bayes_start <- Sys.time()
  message("Working on Ensembles using Naive Bayes analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_n_bayes_train_fit <- e1071::naiveBayes(ensemble_y_train ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_n_bayes_train_fit <- e1071::naiveBayes(ensemble_y_train ~ ., data = ensemble_train)
  }
  ensemble_n_bayes_train_pred <- predict(ensemble_n_bayes_train_fit, ensemble_train)
  ensemble_n_bayes_train_table <- table(ensemble_n_bayes_train_pred, ensemble_y_train)
  ensemble_n_bayes_train_accuracy[i] <- sum(diag(ensemble_n_bayes_train_table)) / sum(ensemble_n_bayes_train_table)
  ensemble_n_bayes_train_accuracy_mean <- mean(ensemble_n_bayes_train_accuracy)
  ensemble_n_bayes_train_diag <- sum(diag(ensemble_n_bayes_train_table))
  ensemble_n_bayes_train_mean <- mean(diag(ensemble_n_bayes_train_table)) / mean(ensemble_n_bayes_train_table)
  ensemble_n_bayes_train_sd <- sd(diag(ensemble_n_bayes_train_table)) / sd(ensemble_n_bayes_train_table)
  sum_ensemble_n_train_bayes <- sum(diag(ensemble_n_bayes_train_table))
  ensemble_n_bayes_train_prop <- diag(prop.table(ensemble_n_bayes_train_table, margin = 1))

  ensemble_n_bayes_test_fit <- e1071::naiveBayes(ensemble_y_train ~ ., data = ensemble_train)
  ensemble_n_bayes_test_pred <- predict(ensemble_n_bayes_test_fit, ensemble_test)
  ensemble_n_bayes_test_table <- table(ensemble_n_bayes_test_pred, ensemble_y_test)
  ensemble_n_bayes_test_accuracy[i] <- sum(diag(ensemble_n_bayes_test_table)) / sum(ensemble_n_bayes_test_table)
  ensemble_n_bayes_test_accuracy_mean <- mean(ensemble_n_bayes_test_accuracy)
  ensemble_n_bayes_test_diag <- sum(diag(ensemble_n_bayes_test_table))
  ensemble_n_bayes_test_mean <- mean(diag(ensemble_n_bayes_test_table)) / mean(ensemble_n_bayes_test_table)
  ensemble_n_bayes_test_sd <- sd(diag(ensemble_n_bayes_test_table)) / sd(ensemble_n_bayes_test_table)
  sum_ensemble_n_test_bayes <- sum(diag(ensemble_n_bayes_test_table))
  ensemble_n_bayes_test_prop <- diag(prop.table(ensemble_n_bayes_test_table, margin = 1))

  ensemble_n_bayes_validation_fit <- e1071::naiveBayes(ensemble_y_train ~ ., data = ensemble_train)
  ensemble_n_bayes_validation_pred <- predict(ensemble_n_bayes_validation_fit, ensemble_validation)
  ensemble_n_bayes_validation_table <- table(ensemble_n_bayes_validation_pred, ensemble_y_validation)
  ensemble_n_bayes_validation_accuracy[i] <- sum(diag(ensemble_n_bayes_validation_table)) / sum(ensemble_n_bayes_validation_table)
  ensemble_n_bayes_validation_accuracy_mean <- mean(ensemble_n_bayes_validation_accuracy)
  ensemble_n_bayes_validation_diag <- sum(diag(ensemble_n_bayes_validation_table))
  ensemble_n_bayes_validation_mean <- mean(diag(ensemble_n_bayes_validation_table)) / mean(ensemble_n_bayes_validation_table)
  ensemble_n_bayes_validation_sd <- sd(diag(ensemble_n_bayes_validation_table)) / sd(ensemble_n_bayes_validation_table)
  sum_ensemble_n_validation_bayes <- sum(diag(ensemble_n_bayes_validation_table))
  ensemble_n_bayes_validation_prop <- diag(prop.table(ensemble_n_bayes_validation_table, margin = 1))

  ensemble_n_bayes_holdout[i] <- mean(c(ensemble_n_bayes_test_accuracy_mean, ensemble_n_bayes_validation_accuracy_mean))
  ensemble_n_bayes_holdout_mean <- mean(ensemble_n_bayes_holdout)
  ensemble_n_bayes_residuals[i] <- 1 - ensemble_n_bayes_holdout[i]
  ensemble_n_bayes_residuals_mean <- mean(ensemble_n_bayes_residuals)
  ensemble_n_bayes_holdout_sd <- sd(ensemble_n_bayes_holdout)
  ensemble_n_bayes_holdout_vs_train[i] <- ensemble_n_bayes_holdout_mean / ensemble_n_bayes_train_accuracy_mean
  ensemble_n_bayes_holdout_vs_train_mean <- mean(ensemble_n_bayes_holdout_vs_train)
  ensemble_n_bayes_holdout_vs_train_range <- range(ensemble_n_bayes_holdout_vs_train)
  ensemble_n_bayes_holdout_vs_train_sd <- sd(ensemble_n_bayes_holdout_vs_train)

  ensemble_n_bayes_table <- ensemble_n_bayes_test_table + ensemble_n_bayes_validation_table
  ensemble_n_bayes_table_total <- ensemble_n_bayes_table_total + ensemble_n_bayes_table
  ensemble_n_bayes_table_sum_diag <- sum(diag(ensemble_n_bayes_table))

  ensemble_n_bayes_confusion_matrix <- caret::confusionMatrix(ensemble_n_bayes_table_total)
  ensemble_n_bayes_confusion_matrix_summary <- colMeans(ensemble_n_bayes_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_n_bayes_true_positive_rate[i] <- ensemble_n_bayes_confusion_matrix_summary[1]
  ensemble_n_bayes_true_positive_rate_mean <- mean(ensemble_n_bayes_true_positive_rate[i])
  ensemble_n_bayes_true_negative_rate[i] <- ensemble_n_bayes_confusion_matrix_summary[2]
  ensemble_n_bayes_true_negative_rate_mean <- mean(ensemble_n_bayes_true_negative_rate)
  ensemble_n_bayes_false_negative_rate[i] <- 1 -  ensemble_n_bayes_true_positive_rate[i]
  ensemble_n_bayes_false_negative_rate_mean <- mean(ensemble_n_bayes_false_negative_rate)
  ensemble_n_bayes_false_positive_rate[i] <- 1 - ensemble_n_bayes_true_negative_rate[i]
  ensemble_n_bayes_false_positive_rate_mean <- mean(ensemble_n_bayes_false_positive_rate)
  ensemble_n_bayes_positive_pred_value[i] <- ensemble_n_bayes_confusion_matrix_summary[3]
  ensemble_n_bayes_positive_pred_value_mean <- mean(ensemble_n_bayes_positive_pred_value)
  ensemble_n_bayes_negative_pred_value[i] <- ensemble_n_bayes_confusion_matrix_summary[4]
  ensemble_n_bayes_negative_pred_value_mean <- mean(ensemble_n_bayes_negative_pred_value)
  ensemble_n_bayes_prevalence[i] <- ensemble_n_bayes_confusion_matrix_summary[8]
  ensemble_n_bayes_prevalence_mean <- mean(ensemble_n_bayes_prevalence)
  ensemble_n_bayes_detection_rate[i] <- ensemble_n_bayes_confusion_matrix_summary[9]
  ensemble_n_bayes_detection_rate_mean <- mean(ensemble_n_bayes_detection_rate)
  ensemble_n_bayes_detection_prevalence[i] <- ensemble_n_bayes_confusion_matrix_summary[10]
  ensemble_n_bayes_detection_prevalence_mean <- mean(ensemble_n_bayes_detection_prevalence)
  ensemble_n_bayes_classification_error[i] <- 1 - ensemble_n_bayes_holdout[i]
  ensemble_n_bayes_classification_error_mean <- mean(ensemble_n_bayes_classification_error)
  ensemble_n_bayes_F1_score[i] <- ensemble_n_bayes_confusion_matrix_summary[7]
  ensemble_n_bayes_F1_score_mean <- mean(ensemble_n_bayes_F1_score[i])

  ensemble_n_bayes_end <- Sys.time()
  ensemble_n_bayes_duration[i] <- ensemble_n_bayes_end - ensemble_n_bayes_start
  ensemble_n_bayes_duration_mean <- mean(ensemble_n_bayes_duration)
  ensemble_n_bayes_duration_sd <- sd(ensemble_n_bayes_duration)

  #### 17. Ensemble Ranger Model #####
  ensemble_ranger_start <- Sys.time()
  message("Working on Ensembles using Ranger analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_ranger_train_fit <- MachineShop::fit(y ~ ., data = ensemble_train, model = "RangerModel")
  }
  if(set_seed == "N"){
    ensemble_ranger_train_fit <- MachineShop::fit(y ~ ., data = ensemble_train, model = "RangerModel")
  }
  ensemble_ranger_train_pred <- predict(ensemble_ranger_train_fit, newdata = ensemble_train)
  ensemble_ranger_train_table <- table(ensemble_ranger_train_pred, ensemble_y_train)
  ensemble_ranger_train_accuracy[i] <- sum(diag(ensemble_ranger_train_table)) / sum(ensemble_ranger_train_table)
  ensemble_ranger_train_accuracy_mean <- mean(ensemble_ranger_train_accuracy)
  ensemble_ranger_train_diag <- sum(diag(ensemble_ranger_train_table))
  ensemble_ranger_train_mean <- mean(diag(ensemble_ranger_train_table)) / mean(ensemble_ranger_train_table)
  ensemble_ranger_train_sd <- sd(diag(ensemble_ranger_train_table)) / sd(diag(ensemble_ranger_train_table))
  sum_ensemble_train_ranger <- sum(diag(ensemble_ranger_train_table))
  ensemble_ranger_train_prop <- diag(prop.table(ensemble_ranger_train_table, margin = 1))

  ensemble_ranger_test_fit <- MachineShop::fit(y ~ ., data = ensemble_train, model = "RangerModel")
  ensemble_ranger_test_pred <- predict(ensemble_ranger_test_fit, newdata = ensemble_test)
  ensemble_ranger_test_table <- table(ensemble_ranger_test_pred, ensemble_y_test)
  ensemble_ranger_test_accuracy[i] <- sum(diag(ensemble_ranger_test_table)) / sum(ensemble_ranger_test_table)
  ensemble_ranger_test_accuracy_mean <- mean(ensemble_ranger_test_accuracy)
  ensemble_ranger_test_diag <- sum(diag(ensemble_ranger_test_table))
  ensemble_ranger_test_mean <- mean(diag(ensemble_ranger_test_table)) / mean(ensemble_ranger_test_table)
  ensemble_ranger_test_sd <- sd(diag(ensemble_ranger_test_table)) / sd(diag(ensemble_ranger_test_table))
  sum_ensemble_test_ranger <- sum(diag(ensemble_ranger_test_table))
  ensemble_ranger_test_prop <- diag(prop.table(ensemble_ranger_test_table, margin = 1))

  ensemble_ranger_validation_fit <- MachineShop::fit(y ~ ., data = ensemble_train, model = "RangerModel")
  ensemble_ranger_validation_pred <- predict(ensemble_ranger_validation_fit, newdata = ensemble_validation)
  ensemble_ranger_validation_table <- table(ensemble_ranger_validation_pred, ensemble_y_validation)
  ensemble_ranger_validation_accuracy[i] <- sum(diag(ensemble_ranger_validation_table)) / sum(ensemble_ranger_validation_table)
  ensemble_ranger_validation_accuracy_mean <- mean(ensemble_ranger_validation_accuracy)
  ensemble_ranger_validation_diag <- sum(diag(ensemble_ranger_validation_table))
  ensemble_ranger_validation_mean <- mean(diag(ensemble_ranger_validation_table)) / mean(ensemble_ranger_validation_table)
  ensemble_ranger_validation_sd <- sd(diag(ensemble_ranger_validation_table)) / sd(diag(ensemble_ranger_validation_table))
  sum_ensemble_validation_ranger <- sum(diag(ensemble_ranger_validation_table))
  ensemble_ranger_validation_prop <- diag(prop.table(ensemble_ranger_validation_table, margin = 1))

  ensemble_ranger_holdout[i] <- mean(c(ensemble_ranger_test_accuracy_mean, ensemble_ranger_validation_accuracy_mean))
  ensemble_ranger_holdout_mean <- mean(ensemble_ranger_holdout)
  ensemble_ranger_residuals[i] <- 1 - ensemble_ranger_holdout[i]
  ensemble_ranger_residuals_mean <- mean(ensemble_ranger_residuals)
  ensemble_ranger_holdout_sd <- sd(ensemble_ranger_holdout)
  ensemble_ranger_holdout_vs_train[i] <- ensemble_ranger_holdout_mean / ensemble_ranger_train_accuracy_mean
  ensemble_ranger_holdout_vs_train_mean <- mean(ensemble_ranger_holdout_vs_train)
  ensemble_ranger_holdout_vs_train_range <- range(ensemble_ranger_holdout_vs_train)
  ensemble_ranger_holdout_vs_train_sd <- sd(ensemble_ranger_holdout_vs_train)

  ensemble_ranger_table <- ensemble_ranger_test_table + ensemble_ranger_validation_table
  ensemble_ranger_table_total <- ensemble_ranger_table_total + ensemble_ranger_table
  ensemble_ranger_table_sum_diag <- sum(diag(ensemble_ranger_table))

  ensemble_ranger_confusion_matrix <- caret::confusionMatrix(ensemble_ranger_table_total)
  ensemble_ranger_confusion_matrix_summary <- colMeans(ensemble_ranger_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_ranger_true_positive_rate[i] <- ensemble_ranger_confusion_matrix_summary[1]
  ensemble_ranger_true_positive_rate_mean <- mean(ensemble_ranger_true_positive_rate[i])
  ensemble_ranger_true_negative_rate[i] <- ensemble_ranger_confusion_matrix_summary[2]
  ensemble_ranger_true_negative_rate_mean <- mean(ensemble_ranger_true_negative_rate)
  ensemble_ranger_false_negative_rate[i] <- 1 -  ensemble_ranger_true_positive_rate[i]
  ensemble_ranger_false_negative_rate_mean <- mean(ensemble_ranger_false_negative_rate)
  ensemble_ranger_false_positive_rate[i] <- 1 - ensemble_ranger_true_negative_rate[i]
  ensemble_ranger_false_positive_rate_mean <- mean(ensemble_ranger_false_positive_rate)
  ensemble_ranger_positive_pred_value[i] <- ensemble_ranger_confusion_matrix_summary[3]
  ensemble_ranger_positive_pred_value_mean <- mean(ensemble_ranger_positive_pred_value)
  ensemble_ranger_negative_pred_value[i] <- ensemble_ranger_confusion_matrix_summary[4]
  ensemble_ranger_negative_pred_value_mean <- mean(ensemble_ranger_negative_pred_value)
  ensemble_ranger_prevalence[i] <- ensemble_ranger_confusion_matrix_summary[8]
  ensemble_ranger_prevalence_mean <- mean(ensemble_ranger_prevalence)
  ensemble_ranger_detection_rate[i] <- ensemble_ranger_confusion_matrix_summary[9]
  ensemble_ranger_detection_rate_mean <- mean(ensemble_ranger_detection_rate)
  ensemble_ranger_detection_prevalence[i] <- ensemble_ranger_confusion_matrix_summary[10]
  ensemble_ranger_detection_prevalence_mean <- mean(ensemble_ranger_detection_prevalence)
  ensemble_ranger_classification_error[i] <- 1 - ensemble_ranger_holdout[i]
  ensemble_ranger_classification_error_mean <- mean(ensemble_ranger_classification_error)
  ensemble_ranger_F1_score[i] <- ensemble_ranger_confusion_matrix_summary[7]
  ensemble_ranger_F1_score_mean <- mean(ensemble_ranger_F1_score[i])

  ensemble_ranger_end <- Sys.time()
  ensemble_ranger_duration[i] <- ensemble_ranger_end - ensemble_ranger_start
  ensemble_ranger_duration_mean <- mean(ensemble_ranger_duration)
  ensemble_ranger_duration_sd <- sd(ensemble_ranger_duration)

  #### 18. Ensemble Random Forest ####
  ensemble_rf_start <- Sys.time()
  message("Working on Ensembles using Random Forest analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_train_rf_fit <- randomForest::randomForest(x = ensemble_train, y = ensemble_y_train)
  }
  if(set_seed == "N"){
    ensemble_train_rf_fit <- randomForest::randomForest(x = ensemble_train, y = ensemble_y_train)
  }
  ensemble_rf_train_pred <- predict(ensemble_train_rf_fit, ensemble_train, type = "class")
  ensemble_rf_train_table <- table(ensemble_rf_train_pred, ensemble_y_train)
  ensemble_rf_train_accuracy[i] <- sum(diag(ensemble_rf_train_table)) / sum(ensemble_rf_train_table)
  ensemble_rf_train_accuracy_mean <- mean(ensemble_rf_train_accuracy)
  ensemble_rf_train_diag <- sum(diag(ensemble_rf_train_table))
  ensemble_rf_train_mean <- mean(diag(ensemble_rf_train_table)) / mean(ensemble_rf_train_table)
  ensemble_rf_train_sd <- sd(diag(ensemble_rf_train_table)) / sd(ensemble_rf_train_table)
  sum_ensemble_train_rf <- sum(diag(ensemble_rf_train_table))
  ensemble_rf_train_prop <- diag(prop.table(ensemble_rf_train_table, margin = 1))

  ensemble_rf_test_pred <- predict(ensemble_train_rf_fit, ensemble_test, type = "class")
  ensemble_rf_test_table <- table(ensemble_rf_test_pred, ensemble_y_test)
  ensemble_rf_test_accuracy[i] <- sum(diag(ensemble_rf_test_table)) / sum(ensemble_rf_test_table)
  ensemble_rf_test_accuracy_mean <- mean(ensemble_rf_test_accuracy)
  ensemble_rf_test_diag <- sum(diag(ensemble_rf_test_table))
  ensemble_rf_test_mean <- mean(diag(ensemble_rf_test_table)) / mean(ensemble_rf_test_table)
  ensemble_rf_test_sd <- sd(diag(ensemble_rf_test_table)) / sd(ensemble_rf_test_table)
  sum_ensemble_test_rf <- sum(diag(ensemble_rf_test_table))
  ensemble_rf_test_prop <- diag(prop.table(ensemble_rf_test_table, margin = 1))

  ensemble_rf_validation_pred <- predict(ensemble_train_rf_fit, ensemble_validation, type = "class")
  ensemble_rf_validation_table <- table(ensemble_rf_validation_pred, ensemble_y_validation)
  ensemble_rf_validation_accuracy[i] <- sum(diag(ensemble_rf_validation_table)) / sum(ensemble_rf_validation_table)
  ensemble_rf_validation_accuracy_mean <- mean(ensemble_rf_validation_accuracy)
  ensemble_rf_validation_diag <- sum(diag(ensemble_rf_validation_table))
  ensemble_rf_validation_mean <- mean(diag(ensemble_rf_validation_table)) / mean(ensemble_rf_validation_table)
  ensemble_rf_validation_sd <- sd(diag(ensemble_rf_validation_table)) / sd(ensemble_rf_validation_table)
  sum_ensemble_validation_rf <- sum(diag(ensemble_rf_validation_table))
  ensemble_rf_validation_prop <- diag(prop.table(ensemble_rf_validation_table, margin = 1))

  ensemble_rf_holdout[i] <- mean(c(ensemble_rf_test_accuracy_mean, ensemble_rf_validation_accuracy_mean))
  ensemble_rf_holdout_mean <- mean(ensemble_rf_holdout)
  ensemble_rf_residuals[i] <- 1 - ensemble_rf_holdout[i]
  ensemble_rf_residuals_mean <- mean(ensemble_rf_residuals)
  ensemble_rf_holdout_sd <- sd(ensemble_rf_holdout)
  ensemble_rf_holdout_vs_train[i] <- ensemble_rf_holdout_mean / ensemble_rf_train_accuracy_mean
  ensemble_rf_holdout_vs_train_mean <- mean(ensemble_rf_holdout_vs_train)
  ensemble_rf_holdout_vs_train_range <- range(ensemble_rf_holdout_vs_train)
  ensemble_rf_holdout_vs_train_sd <- sd(ensemble_rf_holdout_vs_train)

  ensemble_rf_table <- ensemble_rf_test_table + ensemble_rf_validation_table
  ensemble_rf_table_total <- ensemble_rf_table_total + ensemble_rf_table
  ensemble_rf_table_sum_diag <- sum(diag(ensemble_rf_table))

  ensemble_rf_confusion_matrix <- caret::confusionMatrix(ensemble_rf_table_total)
  ensemble_rf_confusion_matrix_summary <- colMeans(ensemble_rf_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_rf_true_positive_rate[i] <- ensemble_rf_confusion_matrix_summary[1]
  ensemble_rf_true_positive_rate_mean <- mean(ensemble_rf_true_positive_rate[i])
  ensemble_rf_true_negative_rate[i] <- ensemble_rf_confusion_matrix_summary[2]
  ensemble_rf_true_negative_rate_mean <- mean(ensemble_rf_true_negative_rate)
  ensemble_rf_false_negative_rate[i] <- 1 -  ensemble_rf_true_positive_rate[i]
  ensemble_rf_false_negative_rate_mean <- mean(ensemble_rf_false_negative_rate)
  ensemble_rf_false_positive_rate[i] <- 1 - ensemble_rf_true_negative_rate[i]
  ensemble_rf_false_positive_rate_mean <- mean(ensemble_rf_false_positive_rate)
  ensemble_rf_positive_pred_value[i] <- ensemble_rf_confusion_matrix_summary[3]
  ensemble_rf_positive_pred_value_mean <- mean(ensemble_rf_positive_pred_value)
  ensemble_rf_negative_pred_value[i] <- ensemble_rf_confusion_matrix_summary[4]
  ensemble_rf_negative_pred_value_mean <- mean(ensemble_rf_negative_pred_value)
  ensemble_rf_prevalence[i] <- ensemble_rf_confusion_matrix_summary[8]
  ensemble_rf_prevalence_mean <- mean(ensemble_rf_prevalence)
  ensemble_rf_detection_rate[i] <- ensemble_rf_confusion_matrix_summary[9]
  ensemble_rf_detection_rate_mean <- mean(ensemble_rf_detection_rate)
  ensemble_rf_detection_prevalence[i] <- ensemble_rf_confusion_matrix_summary[10]
  ensemble_rf_detection_prevalence_mean <- mean(ensemble_rf_detection_prevalence)
  ensemble_rf_classification_error[i] <- 1 - ensemble_rf_holdout[i]
  ensemble_rf_classification_error_mean <- mean(ensemble_rf_classification_error)
  ensemble_rf_F1_score[i] <- ensemble_rf_confusion_matrix_summary[7]
  ensemble_rf_F1_score_mean <- mean(ensemble_rf_F1_score[i])

  ensemble_rf_end <- Sys.time()
  ensemble_rf_duration[i] <- ensemble_rf_end - ensemble_rf_start
  ensemble_rf_duration_mean <- mean(ensemble_rf_duration)
  ensemble_rf_duration_sd <- sd(ensemble_rf_duration)

  #### 19. Ensemble Support Vector Machines ####
  ensemble_svm_start <- Sys.time()
  message("Working on Ensembles using Support Vector Machines analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_svm_train_fit <- e1071::svm(ensemble_y_train ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)
  }
  if(set_seed == "N"){
    ensemble_svm_train_fit <- e1071::svm(ensemble_y_train ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)
  }
  ensemble_svm_train_pred <- predict(ensemble_svm_train_fit, ensemble_train, type = "class")
  ensemble_svm_train_table <- table(ensemble_svm_train_pred, ensemble_y_train)
  ensemble_svm_train_accuracy[i] <- sum(diag(ensemble_svm_train_table)) / sum(ensemble_svm_train_table)
  ensemble_svm_train_accuracy_mean <- mean(ensemble_svm_train_accuracy)
  ensemble_svm_train_diag <- sum(diag(ensemble_svm_train_table))
  ensemble_svm_train_mean <- mean(diag(ensemble_svm_train_table)) / mean(ensemble_svm_train_table)
  ensemble_svm_train_sd <- sd(diag(ensemble_svm_train_table)) / sd(ensemble_svm_train_table)
  sum_ensemble_train_svm <- sum(diag(ensemble_svm_train_table))
  ensemble_svm_train_prop <- diag(prop.table(ensemble_svm_train_table, margin = 1))

  ensemble_svm_test_fit <- e1071::svm(ensemble_y_train ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)
  ensemble_svm_test_pred <- predict(ensemble_svm_test_fit, ensemble_test, type = "class")
  ensemble_svm_test_table <- table(ensemble_svm_test_pred, ensemble_y_test)
  ensemble_svm_test_accuracy[i] <- sum(diag(ensemble_svm_test_table)) / sum(ensemble_svm_test_table)
  ensemble_svm_test_accuracy_mean <- mean(ensemble_svm_test_accuracy)
  ensemble_svm_test_diag <- sum(diag(ensemble_svm_test_table))
  ensemble_svm_test_mean <- mean(diag(ensemble_svm_test_table)) / mean(ensemble_svm_test_table)
  ensemble_svm_test_sd <- sd(diag(ensemble_svm_test_table)) / sd(ensemble_svm_test_table)
  sum_ensemble_test_svm <- sum(diag(ensemble_svm_test_table))
  ensemble_svm_test_prop <- diag(prop.table(ensemble_svm_test_table, margin = 1))

  ensemble_svm_validation_fit <- e1071::svm(ensemble_y_train ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)
  ensemble_svm_validation_pred <- predict(ensemble_svm_validation_fit, ensemble_validation, type = "class")
  ensemble_svm_validation_table <- table(ensemble_svm_validation_pred, ensemble_y_validation)
  ensemble_svm_validation_accuracy[i] <- sum(diag(ensemble_svm_validation_table)) / sum(ensemble_svm_validation_table)
  ensemble_svm_validation_accuracy_mean <- mean(ensemble_svm_validation_accuracy)
  ensemble_svm_validation_diag <- sum(diag(ensemble_svm_validation_table))
  ensemble_svm_validation_mean <- mean(diag(ensemble_svm_validation_table)) / mean(ensemble_svm_validation_table)
  ensemble_svm_validation_sd <- sd(diag(ensemble_svm_validation_table)) / sd(ensemble_svm_validation_table)
  sum_ensemble_validation_svm <- sum(diag(ensemble_svm_validation_table))
  ensemble_svm_validation_prop <- diag(prop.table(ensemble_svm_validation_table, margin = 1))

  ensemble_svm_holdout[i] <- mean(c(ensemble_svm_test_accuracy_mean, ensemble_svm_validation_accuracy_mean))
  ensemble_svm_holdout_mean <- mean(ensemble_svm_holdout)
  ensemble_svm_residuals[i] <- 1 - ensemble_svm_holdout[i]
  ensemble_svm_residuals_mean <- mean(ensemble_svm_residuals)
  ensemble_svm_holdout_sd <- sd(ensemble_svm_holdout)
  ensemble_svm_holdout_vs_train[i] <- ensemble_svm_holdout_mean / ensemble_svm_train_accuracy_mean
  ensemble_svm_holdout_vs_train_mean <- mean(ensemble_svm_holdout_vs_train)
  ensemble_svm_holdout_vs_train_range <- range(ensemble_svm_holdout_vs_train)
  ensemble_svm_holdout_vs_train_sd <- sd(ensemble_svm_holdout_vs_train)

  ensemble_svm_table <- ensemble_svm_test_table + ensemble_svm_validation_table
  ensemble_svm_table_total <- ensemble_svm_table_total + ensemble_svm_table
  ensemble_svm_table_sum_diag <- sum(diag(ensemble_svm_table))

  ensemble_svm_confusion_matrix <- caret::confusionMatrix(ensemble_svm_table_total)
  ensemble_svm_confusion_matrix_summary <- colMeans(ensemble_svm_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_svm_true_positive_rate[i] <- ensemble_svm_confusion_matrix_summary[1]
  ensemble_svm_true_positive_rate_mean <- mean(ensemble_svm_true_positive_rate[i])
  ensemble_svm_true_negative_rate[i] <- ensemble_svm_confusion_matrix_summary[2]
  ensemble_svm_true_negative_rate_mean <- mean(ensemble_svm_true_negative_rate)
  ensemble_svm_false_negative_rate[i] <- 1 -  ensemble_svm_true_positive_rate[i]
  ensemble_svm_false_negative_rate_mean <- mean(ensemble_svm_false_negative_rate)
  ensemble_svm_false_positive_rate[i] <- 1 - ensemble_svm_true_negative_rate[i]
  ensemble_svm_false_positive_rate_mean <- mean(ensemble_svm_false_positive_rate)
  ensemble_svm_positive_pred_value[i] <- ensemble_svm_confusion_matrix_summary[3]
  ensemble_svm_positive_pred_value_mean <- mean(ensemble_svm_positive_pred_value)
  ensemble_svm_negative_pred_value[i] <- ensemble_svm_confusion_matrix_summary[4]
  ensemble_svm_negative_pred_value_mean <- mean(ensemble_svm_negative_pred_value)
  ensemble_svm_prevalence[i] <- ensemble_svm_confusion_matrix_summary[8]
  ensemble_svm_prevalence_mean <- mean(ensemble_svm_prevalence)
  ensemble_svm_detection_rate[i] <- ensemble_svm_confusion_matrix_summary[9]
  ensemble_svm_detection_rate_mean <- mean(ensemble_svm_detection_rate)
  ensemble_svm_detection_prevalence[i] <- ensemble_svm_confusion_matrix_summary[10]
  ensemble_svm_detection_prevalence_mean <- mean(ensemble_svm_detection_prevalence)
  ensemble_svm_classification_error[i] <- 1 - ensemble_svm_holdout[i]
  ensemble_svm_classification_error_mean <- mean(ensemble_svm_classification_error)
  ensemble_svm_F1_score[i] <- ensemble_svm_confusion_matrix_summary[7]
  ensemble_svm_F1_score_mean <- mean(ensemble_svm_F1_score[i])

  ensemble_svm_end <- Sys.time()
  ensemble_svm_duration[i] <- ensemble_svm_end - ensemble_svm_start
  ensemble_svm_duration_mean <- mean(ensemble_svm_duration)
  ensemble_svm_duration_sd <- sd(ensemble_svm_duration)

  #### 20. Ensemble Trees ####
  ensemble_tree_start <- Sys.time()
  message("Working on Ensembles using Trees analysis")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_tree_train_fit <- tree::tree(y ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_tree_train_fit <- tree::tree(y ~ ., data = ensemble_train)
  }
  ensemble_tree_train_pred <- predict(ensemble_tree_train_fit, ensemble_train, type = "class")
  ensemble_tree_train_table <- table(ensemble_tree_train_pred, ensemble_y_train)
  ensemble_tree_train_accuracy[i] <- sum(diag(ensemble_tree_train_table)) / sum(ensemble_tree_train_table)
  ensemble_tree_train_accuracy_mean <- mean(ensemble_tree_train_accuracy)
  ensemble_tree_train_diag <- sum(diag(ensemble_tree_train_table))
  ensemble_tree_train_mean <- mean(diag(ensemble_tree_train_table)) / mean(ensemble_tree_train_table)
  ensemble_tree_train_sd <- sd(diag(ensemble_tree_train_table)) / sd(ensemble_tree_train_table)
  sum_ensemble_train_tree <- sum(diag(ensemble_tree_train_table))
  ensemble_tree_train_prop <- diag(prop.table(ensemble_tree_train_table, margin = 1))

  ensemble_tree_test_pred <- predict(ensemble_tree_train_fit, ensemble_test, type = "class")
  ensemble_tree_test_table <- table(ensemble_tree_test_pred, ensemble_y_test)
  ensemble_tree_test_accuracy[i] <- sum(diag(ensemble_tree_test_table)) / sum(ensemble_tree_test_table)
  ensemble_tree_test_accuracy_mean <- mean(ensemble_tree_test_accuracy)
  ensemble_tree_test_diag <- sum(diag(ensemble_tree_test_table))
  ensemble_tree_test_mean <- mean(diag(ensemble_tree_test_table)) / mean(ensemble_tree_test_table)
  ensemble_tree_test_sd <- sd(diag(ensemble_tree_test_table)) / sd(ensemble_tree_test_table)
  sum_ensemble_test_tree <- sum(diag(ensemble_tree_test_table))
  ensemble_tree_test_prop <- diag(prop.table(ensemble_tree_test_table, margin = 1))

  ensemble_tree_validation_pred <- predict(ensemble_tree_train_fit, ensemble_validation, type = "class")
  ensemble_tree_validation_table <- table(ensemble_tree_validation_pred, ensemble_y_validation)
  ensemble_tree_validation_accuracy[i] <- sum(diag(ensemble_tree_validation_table)) / sum(ensemble_tree_validation_table)
  ensemble_tree_validation_accuracy_mean <- mean(ensemble_tree_validation_accuracy)
  ensemble_tree_validation_diag <- sum(diag(ensemble_tree_validation_table))
  ensemble_tree_validation_mean <- mean(diag(ensemble_tree_validation_table)) / mean(ensemble_tree_validation_table)
  ensemble_tree_validation_sd <- sd(diag(ensemble_tree_validation_table)) / sd(ensemble_tree_validation_table)
  sum_ensemble_validation_tree <- sum(diag(ensemble_tree_validation_table))
  ensemble_tree_validation_prop <- diag(prop.table(ensemble_tree_validation_table, margin = 1))

  ensemble_tree_holdout[i] <- mean(c(ensemble_tree_test_accuracy_mean, ensemble_tree_validation_accuracy_mean))
  ensemble_tree_holdout_mean <- mean(ensemble_tree_holdout)
  ensemble_tree_residuals[i] <- 1 - ensemble_tree_holdout[i]
  ensemble_tree_residuals_mean <- mean(ensemble_tree_residuals)
  ensemble_tree_holdout_sd <- sd(ensemble_tree_holdout)
  ensemble_tree_holdout_vs_train[i] <- ensemble_tree_holdout_mean / ensemble_tree_train_accuracy_mean
  ensemble_tree_holdout_vs_train_mean <- mean(ensemble_tree_holdout_vs_train)
  ensemble_tree_holdout_vs_train_range <- range(ensemble_tree_holdout_vs_train)
  ensemble_tree_holdout_vs_train_sd <- sd(ensemble_tree_holdout_vs_train)

  ensemble_tree_table <- ensemble_tree_test_table + ensemble_tree_validation_table
  ensemble_tree_table_total <- ensemble_tree_table_total + ensemble_tree_table
  ensemble_tree_table_sum_diag <- sum(diag(ensemble_tree_table))

  ensemble_tree_confusion_matrix <- caret::confusionMatrix(ensemble_tree_table_total)
  ensemble_tree_confusion_matrix_summary <- colMeans(ensemble_tree_confusion_matrix$byClass, na.rm = TRUE)

  ensemble_tree_true_positive_rate[i] <- ensemble_tree_confusion_matrix_summary[1]
  ensemble_tree_true_positive_rate_mean <- mean(ensemble_tree_true_positive_rate[i])
  ensemble_tree_true_negative_rate[i] <- ensemble_tree_confusion_matrix_summary[2]
  ensemble_tree_true_negative_rate_mean <- mean(ensemble_tree_true_negative_rate)
  ensemble_tree_false_negative_rate[i] <- 1 -  ensemble_tree_true_positive_rate[i]
  ensemble_tree_false_negative_rate_mean <- mean(ensemble_tree_false_negative_rate)
  ensemble_tree_false_positive_rate[i] <- 1 - ensemble_tree_true_negative_rate[i]
  ensemble_tree_false_positive_rate_mean <- mean(ensemble_tree_false_positive_rate)
  ensemble_tree_positive_pred_value[i] <- ensemble_tree_confusion_matrix_summary[3]
  ensemble_tree_positive_pred_value_mean <- mean(ensemble_tree_positive_pred_value)
  ensemble_tree_negative_pred_value[i] <- ensemble_tree_confusion_matrix_summary[4]
  ensemble_tree_negative_pred_value_mean <- mean(ensemble_tree_negative_pred_value)
  ensemble_tree_prevalence[i] <- ensemble_tree_confusion_matrix_summary[8]
  ensemble_tree_prevalence_mean <- mean(ensemble_tree_prevalence)
  ensemble_tree_detection_rate[i] <- ensemble_tree_confusion_matrix_summary[9]
  ensemble_tree_detection_rate_mean <- mean(ensemble_tree_detection_rate)
  ensemble_tree_detection_prevalence[i] <- ensemble_tree_confusion_matrix_summary[10]
  ensemble_tree_detection_prevalence_mean <- mean(ensemble_tree_detection_prevalence)
  ensemble_tree_classification_error[i] <- 1 - ensemble_tree_holdout[i]
  ensemble_tree_classification_error_mean <- mean(ensemble_tree_classification_error)
  ensemble_tree_F1_score[i] <- ensemble_tree_confusion_matrix_summary[7]
  ensemble_tree_F1_score_mean <- mean(ensemble_tree_F1_score[i])

  ensemble_tree_end <- Sys.time()
  ensemble_tree_duration[i] <- ensemble_tree_end - ensemble_tree_start
  ensemble_tree_duration_mean <- mean(ensemble_tree_duration)
  ensemble_tree_duration_sd <- sd(ensemble_tree_duration)
}
#### Ensembles end here ####

#### Build results tables starts here ####

Results <- data.frame(
  "Model" = c(
    "Bagging", "Bagged Random Forest", "C50",
    "Linear", "Naive Bayes",
    "Partial Least Squares", "Penalized Discriminant Analysis", "Random Forest", "Ranger",
    "RPart", "Support Vector Machines", "Trees",
    "Ensemble Bagged Cart", "Ensemble Bagged Random Forest", "Ensemble C50",
    "Ensemble Naive Bayes", "Ensemble Ranger", "Ensemble Random Forest", "Ensemble Support Vector Machines",
    "Ensemble Trees"
  ),
  "Mean_Holdout_Accuracy" = round(c(
    bagging_holdout_mean, bag_rf_holdout_mean,
    C50_holdout_mean, linear_holdout_mean,
    n_bayes_holdout_mean, pls_holdout_mean, pda_holdout_mean, rf_holdout_mean, ranger_holdout_mean,
    rpart_holdout_mean, svm_holdout_mean, tree_holdout_mean,
    ensemble_bag_cart_holdout_mean, ensemble_bag_rf_holdout_mean, ensemble_C50_holdout_mean,
    ensemble_n_bayes_holdout_mean, ensemble_ranger_holdout_mean, ensemble_rf_holdout_mean, ensemble_svm_holdout_mean,
    ensemble_tree_holdout_mean
  ), 4),
  "Accuracy_Holdout_Std.Dev" = round(c(
    bagging_holdout_sd, bag_rf_holdout_sd,
    C50_holdout_sd, linear_holdout_sd,
    n_bayes_holdout_sd, pls_holdout_sd, pda_holdout_sd, rf_holdout_sd, ranger_holdout_sd,
    rpart_holdout_sd, svm_holdout_sd, tree_holdout_sd,
    ensemble_bag_cart_holdout_sd, ensemble_bag_rf_holdout_sd, ensemble_C50_holdout_sd,
    ensemble_n_bayes_holdout_sd, ensemble_ranger_holdout_sd, ensemble_rf_holdout_sd, ensemble_svm_holdout_sd,
    ensemble_tree_holdout_sd
  ), 4),
  'Classification_Error_Mean' = round(c(
    bagging_classification_error_mean, bag_rf_classification_error_mean,
    C50_classification_error_mean, linear_classification_error_mean,
    n_bayes_classification_error_mean, pls_classification_error_mean, pda_classification_error_mean, rf_classification_error_mean, ranger_classification_error_mean,
    rpart_classification_error_mean, svm_classification_error_mean, tree_classification_error_mean,
    ensemble_bag_cart_classification_error_mean, ensemble_bag_rf_classification_error_mean, ensemble_C50_classification_error_mean,
    ensemble_n_bayes_classification_error_mean, ensemble_ranger_classification_error_mean, ensemble_rf_classification_error_mean, ensemble_svm_classification_error_mean,
    ensemble_tree_classification_error_mean
  ), 4),
  "Duration" = round(c(
    bagging_duration_mean, bag_rf_duration_mean,
    C50_duration_mean, linear_duration_mean,
    n_bayes_duration_mean, pls_duration_mean, pda_duration_mean, rf_duration_mean, ranger_duration_mean,
    rpart_duration_mean, svm_duration_mean, tree_duration_mean,
    ensemble_bag_cart_duration_mean, ensemble_bag_rf_duration_mean, ensemble_C50_duration_mean,
    ensemble_n_bayes_duration_mean, ensemble_ranger_duration_mean, ensemble_rf_duration_mean, ensemble_svm_duration_mean,
    ensemble_tree_duration_mean
  ), 4),
  "Duration_St_Dev" = round(c(
    bagging_duration_sd, bag_rf_duration_sd,
    C50_duration_sd, linear_duration_sd,
    n_bayes_duration_sd, pls_duration_sd, pda_duration_sd, rf_duration_sd, ranger_duration_sd,
    rpart_duration_sd, svm_duration_sd, tree_duration_sd,
    ensemble_bag_cart_duration_sd, ensemble_bag_rf_duration_sd, ensemble_C50_duration_sd,
    ensemble_n_bayes_duration_sd, ensemble_ranger_duration_sd, ensemble_rf_duration_sd, ensemble_svm_duration_sd,
    ensemble_tree_duration_sd
  ), 4),
  "True_Positive_Rate" = round(c(
    bagging_true_positive_rate_mean, bag_rf_true_positive_rate_mean,
    C50_true_positive_rate_mean, linear_true_positive_rate_mean,
    n_bayes_true_positive_rate_mean, pls_true_positive_rate_mean, pda_true_positive_rate_mean, rf_true_positive_rate_mean, ranger_true_positive_rate_mean,
    rpart_true_positive_rate_mean, svm_true_positive_rate_mean, tree_true_positive_rate_mean,
    ensemble_bag_cart_true_positive_rate_mean, ensemble_bag_rf_true_positive_rate_mean, ensemble_C50_true_positive_rate_mean,
    ensemble_n_bayes_true_positive_rate_mean, ensemble_ranger_true_positive_rate_mean, ensemble_rf_true_positive_rate_mean, ensemble_svm_true_positive_rate_mean,
    ensemble_tree_true_positive_rate_mean
  ), 4),
  "True_Negative_Rate" = round(c(
    bagging_true_negative_rate_mean, bag_rf_true_negative_rate_mean,
    C50_true_negative_rate_mean, linear_true_negative_rate_mean,
    n_bayes_true_negative_rate_mean, pls_true_negative_rate_mean, pda_true_negative_rate_mean, rf_true_negative_rate_mean, ranger_true_negative_rate_mean,
    rpart_true_negative_rate_mean, svm_true_negative_rate_mean, tree_true_negative_rate_mean,
    ensemble_bag_cart_true_negative_rate_mean, ensemble_bag_rf_true_negative_rate_mean, ensemble_C50_true_negative_rate_mean,
    ensemble_n_bayes_true_negative_rate_mean, ensemble_ranger_true_negative_rate_mean, ensemble_rf_true_negative_rate_mean, ensemble_svm_true_negative_rate_mean,
    ensemble_tree_true_negative_rate_mean
  ), 4),
  "False_Positive_Rate" = round(c(
    bagging_false_positive_rate_mean, bag_rf_false_positive_rate_mean,
    C50_false_positive_rate_mean, linear_false_positive_rate_mean,
    n_bayes_false_positive_rate_mean, pls_false_positive_rate_mean, pda_false_positive_rate_mean, rf_false_positive_rate_mean, ranger_false_positive_rate_mean,
    rpart_false_positive_rate_mean, svm_false_positive_rate_mean, tree_false_positive_rate_mean,
    ensemble_bag_cart_false_positive_rate_mean, ensemble_bag_rf_false_positive_rate_mean, ensemble_C50_false_positive_rate_mean,
    ensemble_n_bayes_false_positive_rate_mean, ensemble_ranger_false_positive_rate_mean, ensemble_rf_false_positive_rate_mean, ensemble_svm_false_positive_rate_mean,
    ensemble_tree_false_positive_rate_mean
  ), 4),
  "False_Negative_Rate" = round(c(
    bagging_false_negative_rate_mean, bag_rf_false_negative_rate_mean,
    C50_false_negative_rate_mean, linear_false_negative_rate_mean,
    n_bayes_false_negative_rate_mean, pls_false_negative_rate_mean, pda_false_negative_rate_mean, rf_false_negative_rate_mean, ranger_false_negative_rate_mean,
    rpart_false_negative_rate_mean, svm_false_negative_rate_mean, tree_false_negative_rate_mean,
    ensemble_bag_cart_false_negative_rate_mean, ensemble_bag_rf_false_negative_rate_mean, ensemble_C50_false_negative_rate_mean,
    ensemble_n_bayes_false_negative_rate_mean, ensemble_ranger_false_negative_rate_mean, ensemble_rf_false_negative_rate_mean, ensemble_svm_false_negative_rate_mean,
    ensemble_tree_false_negative_rate_mean
  ), 4),
  "Positive_Pred_Value" = round(c(
    bagging_positive_pred_value_mean, bag_rf_positive_pred_value_mean,
    C50_positive_pred_value_mean, linear_positive_pred_value_mean,
    n_bayes_positive_pred_value_mean, pls_positive_pred_value_mean, pda_positive_pred_value_mean, rf_positive_pred_value_mean, ranger_positive_pred_value_mean,
    rpart_positive_pred_value_mean, svm_positive_pred_value_mean, tree_positive_pred_value_mean,
    ensemble_bag_cart_positive_pred_value_mean, ensemble_bag_rf_positive_pred_value_mean, ensemble_C50_positive_pred_value_mean,
    ensemble_n_bayes_positive_pred_value_mean, ensemble_ranger_positive_pred_value_mean, ensemble_rf_positive_pred_value_mean, ensemble_svm_positive_pred_value_mean,
    ensemble_tree_positive_pred_value_mean
  ), 4),
  "Negative_Pred_Value" = round(c(
    bagging_negative_pred_value_mean, bag_rf_negative_pred_value_mean,
    C50_negative_pred_value_mean, linear_negative_pred_value_mean,
    n_bayes_negative_pred_value_mean, pls_negative_pred_value_mean, pda_negative_pred_value_mean, rf_negative_pred_value_mean, ranger_negative_pred_value_mean,
    rpart_negative_pred_value_mean, svm_negative_pred_value_mean, tree_negative_pred_value_mean,
    ensemble_bag_cart_negative_pred_value_mean, ensemble_bag_rf_negative_pred_value_mean, ensemble_C50_negative_pred_value_mean,
    ensemble_n_bayes_negative_pred_value_mean, ensemble_ranger_negative_pred_value_mean, ensemble_rf_negative_pred_value_mean, ensemble_svm_negative_pred_value_mean,
    ensemble_tree_negative_pred_value_mean
  ), 4),
  "Prevalence" = round(c(
    bagging_prevalence_mean, bag_rf_prevalence_mean,
    C50_prevalence_mean, linear_prevalence_mean,
    n_bayes_prevalence_mean, pls_prevalence_mean, pda_prevalence_mean, rf_prevalence_mean, ranger_prevalence_mean,
    rpart_prevalence_mean, svm_prevalence_mean, tree_prevalence_mean,
    ensemble_bag_cart_prevalence_mean, ensemble_bag_rf_prevalence_mean, ensemble_C50_prevalence_mean,
    ensemble_n_bayes_prevalence_mean, ensemble_ranger_prevalence_mean, ensemble_rf_prevalence_mean, ensemble_svm_prevalence_mean,
    ensemble_tree_prevalence_mean
  ), 4),
  "Detection_Rate" = round(c(
    bagging_detection_rate_mean, bag_rf_detection_rate_mean,
    C50_detection_rate_mean, linear_detection_rate_mean,
    n_bayes_detection_rate_mean, pls_detection_rate_mean, pda_detection_rate_mean, rf_detection_rate_mean, ranger_detection_rate_mean,
    rpart_detection_rate_mean, svm_detection_rate_mean, tree_detection_rate_mean,
    ensemble_bag_cart_detection_rate_mean, ensemble_bag_rf_detection_rate_mean, ensemble_C50_detection_rate_mean,
    ensemble_n_bayes_detection_rate_mean, ensemble_ranger_detection_rate_mean, ensemble_rf_detection_rate_mean, ensemble_svm_detection_rate_mean,
    ensemble_tree_detection_rate_mean
  ), 4),
  "Detection_Prevalence" = round(c(
    bagging_detection_prevalence_mean, bag_rf_detection_prevalence_mean,
    C50_detection_prevalence_mean, linear_detection_prevalence_mean,
    n_bayes_detection_prevalence_mean, pls_detection_prevalence_mean, pda_detection_prevalence_mean, rf_detection_prevalence_mean, ranger_detection_prevalence_mean,
    rpart_detection_prevalence_mean, svm_detection_prevalence_mean, tree_detection_prevalence_mean,
    ensemble_bag_cart_detection_prevalence_mean, ensemble_bag_rf_detection_prevalence_mean, ensemble_C50_detection_prevalence_mean,
    ensemble_n_bayes_detection_prevalence_mean, ensemble_ranger_detection_prevalence_mean, ensemble_rf_detection_prevalence_mean, ensemble_svm_detection_prevalence_mean,
    ensemble_tree_detection_prevalence_mean
  ), 4),
  "F1_Score" = round(c(
    bagging_F1_score_mean, bag_rf_F1_score_mean,
    C50_F1_score_mean, linear_F1_score_mean,
    n_bayes_F1_score_mean, pls_F1_score_mean, pda_F1_score_mean, rf_F1_score_mean, ranger_F1_score_mean,
    rpart_F1_score_mean, svm_F1_score_mean, tree_F1_score_mean,
    ensemble_bag_cart_F1_score_mean, ensemble_bag_rf_F1_score_mean, ensemble_C50_F1_score_mean,
    ensemble_n_bayes_F1_score_mean, ensemble_ranger_F1_score_mean, ensemble_rf_F1_score_mean, ensemble_svm_F1_score_mean,
    ensemble_tree_F1_score_mean
  ), 4),
  "Train_Accuracy" = round(c(
    bagging_train_accuracy_mean, bag_rf_train_accuracy_mean,
    C50_train_accuracy_mean, linear_train_accuracy_mean,
    n_bayes_train_accuracy_mean, pls_train_accuracy_mean, pda_train_accuracy_mean, rf_train_accuracy_mean, ranger_train_accuracy_mean,
    rpart_train_accuracy_mean, svm_train_accuracy_mean, tree_train_accuracy_mean,
    ensemble_bag_cart_train_accuracy_mean, ensemble_bag_rf_train_accuracy_mean,
    ensemble_C50_train_accuracy_mean, ensemble_n_bayes_train_accuracy_mean,
    ensemble_ranger_train_accuracy_mean, ensemble_rf_train_accuracy_mean, ensemble_svm_train_accuracy_mean, ensemble_tree_train_accuracy_mean
  ), 4),
  "Test_Accuracy" = round(c(
    bagging_test_accuracy_mean, bag_rf_test_accuracy_mean,
    C50_test_accuracy_mean, linear_test_accuracy_mean,
    n_bayes_test_accuracy_mean, pls_test_accuracy_mean, pda_test_accuracy_mean, rf_test_accuracy_mean, ranger_test_accuracy_mean,
    rpart_test_accuracy_mean, svm_test_accuracy_mean, tree_test_accuracy_mean,
    ensemble_bag_cart_test_accuracy_mean, ensemble_bag_rf_test_accuracy_mean,
    ensemble_C50_test_accuracy_mean, ensemble_n_bayes_test_accuracy_mean,
    ensemble_ranger_test_accuracy_mean, ensemble_rf_test_accuracy_mean, ensemble_svm_test_accuracy_mean, ensemble_tree_test_accuracy_mean
  ), 4),
  "Validation_Accuracy" = round(c(
    bagging_validation_accuracy_mean, bag_rf_validation_accuracy_mean,
    C50_validation_accuracy_mean,
    linear_validation_accuracy_mean, n_bayes_validation_accuracy_mean, pls_validation_accuracy_mean,
    pda_validation_accuracy_mean, rf_validation_accuracy_mean, ranger_validation_accuracy_mean, rpart_validation_accuracy_mean,
    svm_validation_accuracy_mean, tree_validation_accuracy_mean,
    ensemble_bag_cart_validation_accuracy_mean, ensemble_bag_rf_validation_accuracy_mean, ensemble_C50_validation_accuracy_mean,
    ensemble_n_bayes_validation_accuracy_mean, ensemble_ranger_validation_accuracy_mean,
    ensemble_rf_validation_accuracy_mean, ensemble_svm_validation_accuracy_mean, ensemble_tree_validation_accuracy_mean
  ), 4),
  "Holdout_vs_train" = round(c(
    bagging_holdout_vs_train_mean, bag_rf_holdout_vs_train_mean,
    C50_holdout_vs_train_mean, linear_holdout_vs_train_mean,
    n_bayes_holdout_vs_train_mean, pls_holdout_vs_train_mean, pda_holdout_vs_train_mean, rf_holdout_vs_train_mean, ranger_holdout_vs_train_mean,
    rpart_holdout_vs_train_mean, svm_holdout_vs_train_mean, tree_holdout_vs_train_mean,
    ensemble_bag_cart_holdout_vs_train_mean, ensemble_bag_rf_holdout_vs_train_mean, ensemble_C50_holdout_vs_train_mean,
    ensemble_n_bayes_holdout_vs_train_mean, ensemble_ranger_holdout_vs_train_mean, ensemble_rf_holdout_vs_train_mean,
    ensemble_svm_holdout_vs_train_mean, ensemble_tree_holdout_vs_train_mean
  ), 4),
  "Holdout_vs_train_St_Dev" = round(c(
    bagging_holdout_vs_train_sd, bag_rf_holdout_vs_train_sd,
    C50_holdout_vs_train_sd, linear_holdout_vs_train_sd,
    n_bayes_holdout_vs_train_sd, pls_holdout_vs_train_sd, pda_holdout_vs_train_sd, rf_holdout_vs_train_sd, ranger_holdout_vs_train_sd,
    rpart_holdout_vs_train_sd, svm_holdout_vs_train_sd, tree_holdout_vs_train_sd,
    ensemble_bag_cart_holdout_vs_train_sd, ensemble_bag_rf_holdout_vs_train_sd, ensemble_C50_holdout_vs_train_sd,
    ensemble_n_bayes_holdout_vs_train_sd, ensemble_ranger_holdout_vs_train_sd, ensemble_rf_holdout_vs_train_sd,
    ensemble_svm_holdout_vs_train_sd, ensemble_tree_holdout_vs_train_sd
  ), 4),
  "Diagonal_Sum" = round(c(
    bagging_table_sum_diag, bag_rf_table_sum_diag,
    C50_table_sum_diag, linear_table_sum_diag,
    n_bayes_table_sum_diag, pls_table_sum_diag, pda_table_sum_diag, rf_table_sum_diag, ranger_table_sum_diag,
    rpart_table_sum_diag, svm_table_sum_diag, tree_table_sum_diag,
    ensemble_bag_cart_table_sum_diag, ensemble_bag_rf_table_sum_diag, ensemble_C50_table_sum_diag,
    ensemble_n_bayes_table_sum_diag, ensemble_ranger_table_sum_diag, ensemble_rf_table_sum_diag,
    ensemble_svm_table_sum_diag, ensemble_tree_table_sum_diag
  ), 4)
)

Results <- Results %>% dplyr::arrange(dplyr::desc(Mean_Holdout_Accuracy))

Final_results <- reactable::reactable(Results,
                                      searchable = TRUE, pagination = FALSE, wrap = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                      striped = TRUE, highlight = TRUE, rownames = TRUE, resizable = TRUE
) %>%
  reactablefmtr::add_title("Classification analysis, accuracy, duration, holdout_vs_train, sum of diagonals")

if(save_all_plots == "Y"){
  reactablefmtr::save_reactable_test(Final_results, "Final_results.html")
}

summary_tables <- list(
  "Bagging" = bagging_table_total, "Bagged Random Forest" = bag_rf_table_total, "C50" = C50_table_total,
  "Linear" = linear_table_total, "Naive Bayes" = n_bayes_table_total,
  "Partial Least Sqaures" = pls_table_total, "Penalized Discriminant Ananysis" = pda_table_total, "Random Forest" = rf_table_total,
  "Ranger" = ranger_table_total, "RPart" = rpart_table_total, "Support Vector Machines" = svm_table_total,
  "Trees" = tree_table_total,
  "Ensemble Bagged Cart" = ensemble_bag_cart_table_total,
  "Ensemble Bagged Random Forest" = ensemble_bag_rf_table_total, "Ensemble C50" = ensemble_C50_table_total, "Ensemble Naive Bayes" = ensemble_n_bayes_table_total,
  "Ensemble Ranger" = ensemble_ranger_table_total, "Ensemble Random Forest" = ensemble_rf_table_total,
  "Ensemble Support Vector Machines" = ensemble_svm_table_total, "Ensemble Trees" = ensemble_tree_table_total
)

accuracy_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_holdout, bag_rf_holdout, C50_holdout, linear_holdout,
    n_bayes_holdout, pls_holdout, pda_holdout, rf_holdout, ranger_holdout, rpart_holdout, svm_holdout, tree_holdout,
    ensemble_bag_cart_holdout, ensemble_bag_rf_holdout, ensemble_C50_holdout,
    ensemble_n_bayes_holdout,
    ensemble_ranger_holdout, ensemble_rf_holdout, ensemble_svm_holdout, ensemble_tree_holdout
  ),
  "mean" = rep(c(
    bagging_holdout_mean, bag_rf_holdout_mean, C50_holdout_mean,
    linear_holdout_mean, n_bayes_holdout_mean, pls_holdout_mean,
    pda_holdout_mean, rf_holdout_mean, ranger_holdout_mean, rpart_holdout_mean, svm_holdout_mean, tree_holdout_mean,
    ensemble_bag_cart_holdout_mean, ensemble_bag_rf_holdout_mean, ensemble_C50_holdout_mean,
    ensemble_n_bayes_holdout_mean, ensemble_ranger_holdout_mean, ensemble_rf_holdout_mean, ensemble_svm_holdout_mean,
    ensemble_tree_holdout_mean
  ), each = numresamples)
)


#### Accuracy plot ####
accuracy_plot <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Accuracy by model, higher is better, 1 is best, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Accuracy by model, higher is better, 1 is best. \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

accuracy_plot2 <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Accuracy by model, higher is better, 1 is best. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Accuracy by model, higher is better, 1 is best. \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Residuals plot ####
residuals_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_residuals, bag_rf_residuals, C50_residuals, linear_residuals,
    n_bayes_residuals, pls_residuals, pda_residuals, rf_residuals, ranger_residuals, rpart_residuals, svm_residuals, tree_residuals,
    ensemble_bag_cart_residuals, ensemble_bag_rf_residuals, ensemble_C50_residuals,
    ensemble_n_bayes_residuals,
    ensemble_ranger_residuals, ensemble_rf_residuals, ensemble_svm_residuals, ensemble_tree_residuals
  ),
  "mean" = rep(c(
    bagging_residuals_mean, bag_rf_residuals_mean, C50_residuals_mean,
    linear_residuals_mean, n_bayes_residuals_mean, pls_residuals_mean,
    pda_residuals_mean, rf_residuals_mean, ranger_residuals_mean, rpart_residuals_mean, svm_residuals_mean, tree_residuals_mean,
    ensemble_bag_cart_residuals_mean, ensemble_bag_rf_residuals_mean, ensemble_C50_residuals_mean,
    ensemble_n_bayes_residuals_mean, ensemble_ranger_residuals_mean, ensemble_rf_residuals_mean, ensemble_svm_residuals_mean,
    ensemble_tree_residuals_mean
  ), each = numresamples)
)

residuals_plot <- ggplot2::ggplot(data = residuals_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Residuals by model, lower is better, 0 is best, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "Residuals by model, lower is better, 0 is best. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("residuals_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("residuals_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("residuals_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("residuals_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("residuals_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("residuals_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

residuals_plot2 <- ggplot2::ggplot(data = residuals_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Residuals by model, lower is better, 0 is best, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Residuals by model, lower is better, 0 is best. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("residuals_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("residuals_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("residuals_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("residuals_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("residuals_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("residuals_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Holdout vs train plot ####

holdout_vs_train_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_holdout_vs_train, bag_rf_holdout_vs_train, C50_holdout_vs_train, linear_holdout_vs_train,
    n_bayes_holdout_vs_train, pls_holdout_vs_train, pda_holdout_vs_train, rf_holdout_vs_train, ranger_holdout_vs_train, rpart_holdout_vs_train, svm_holdout_vs_train, tree_holdout_vs_train,
    ensemble_bag_cart_holdout_vs_train, ensemble_bag_rf_holdout_vs_train, ensemble_C50_holdout_vs_train,
    ensemble_n_bayes_holdout_vs_train,
    ensemble_ranger_holdout_vs_train, ensemble_rf_holdout_vs_train, ensemble_svm_holdout_vs_train, ensemble_tree_holdout_vs_train
  ),
  "mean" = rep(c(
    bagging_holdout_vs_train_mean, bag_rf_holdout_vs_train_mean, C50_holdout_vs_train_mean,
    linear_holdout_vs_train_mean, n_bayes_holdout_vs_train_mean, pls_holdout_vs_train_mean,
    pda_holdout_vs_train_mean, rf_holdout_vs_train_mean, ranger_holdout_vs_train_mean, rpart_holdout_vs_train_mean, svm_holdout_vs_train_mean, tree_holdout_vs_train_mean,
    ensemble_bag_cart_holdout_vs_train_mean, ensemble_bag_rf_holdout_vs_train_mean, ensemble_C50_holdout_vs_train_mean,
    ensemble_n_bayes_holdout_vs_train_mean, ensemble_ranger_holdout_vs_train_mean, ensemble_rf_holdout_vs_train_mean, ensemble_svm_holdout_vs_train_mean,
    ensemble_tree_holdout_vs_train_mean
  ), each = numresamples)
)

#### True positive rate plot ####
true_positive_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_true_positive_rate, bag_rf_true_positive_rate, C50_true_positive_rate, linear_true_positive_rate,
    n_bayes_true_positive_rate, pls_true_positive_rate, pda_true_positive_rate, rf_true_positive_rate, ranger_true_positive_rate, rpart_true_positive_rate, svm_true_positive_rate, tree_true_positive_rate,
    ensemble_bag_cart_true_positive_rate, ensemble_bag_rf_true_positive_rate, ensemble_C50_true_positive_rate,
    ensemble_n_bayes_true_positive_rate,
    ensemble_ranger_true_positive_rate, ensemble_rf_true_positive_rate, ensemble_svm_true_positive_rate, ensemble_tree_true_positive_rate
  ),
  "mean" = rep(c(
    bagging_true_positive_rate_mean, bag_rf_true_positive_rate_mean, C50_true_positive_rate_mean,
    linear_true_positive_rate_mean, n_bayes_true_positive_rate_mean, pls_true_positive_rate_mean,
    pda_true_positive_rate_mean, rf_true_positive_rate_mean, ranger_true_positive_rate_mean, rpart_true_positive_rate_mean, svm_true_positive_rate_mean, tree_true_positive_rate_mean,
    ensemble_bag_cart_true_positive_rate_mean, ensemble_bag_rf_true_positive_rate_mean, ensemble_C50_true_positive_rate_mean,
    ensemble_n_bayes_true_positive_rate_mean, ensemble_ranger_true_positive_rate_mean, ensemble_rf_true_positive_rate_mean, ensemble_svm_true_positive_rate_mean,
    ensemble_tree_true_positive_rate_mean
  ), each = numresamples)
)

true_positive_rate_plot <- ggplot2::ggplot(data = true_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("True positive rate, closer to one is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True positive rate, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_positive_rate_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_positive_rate_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_positive_rate_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_positive_rate_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_positive_rate_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_positive_rate_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_positive_rate_plot2 <- ggplot2::ggplot(data = true_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("True positive rate, closer to one is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True positive rate, closer to one is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_positive_rate_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_positive_rate_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_positive_rate_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_positive_rate_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_positive_rate_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_positive_rate_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### True negative rate plot ####
true_negative_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_true_negative_rate, bag_rf_true_negative_rate, C50_true_negative_rate, linear_true_negative_rate,
    n_bayes_true_negative_rate, pls_true_negative_rate, pda_true_negative_rate, rf_true_negative_rate, ranger_true_negative_rate, rpart_true_negative_rate, svm_true_negative_rate, tree_true_negative_rate,
    ensemble_bag_cart_true_negative_rate, ensemble_bag_rf_true_negative_rate, ensemble_C50_true_negative_rate,
    ensemble_n_bayes_true_negative_rate,
    ensemble_ranger_true_negative_rate, ensemble_rf_true_negative_rate, ensemble_svm_true_negative_rate, ensemble_tree_true_negative_rate
  ),
  "mean" = rep(c(
    bagging_true_negative_rate_mean, bag_rf_true_negative_rate_mean, C50_true_negative_rate_mean,
    linear_true_negative_rate_mean, n_bayes_true_negative_rate_mean, pls_true_negative_rate_mean,
    pda_true_negative_rate_mean, rf_true_negative_rate_mean, ranger_true_negative_rate_mean, rpart_true_negative_rate_mean, svm_true_negative_rate_mean, tree_true_negative_rate_mean,
    ensemble_bag_cart_true_negative_rate_mean, ensemble_bag_rf_true_negative_rate_mean, ensemble_C50_true_negative_rate_mean,
    ensemble_n_bayes_true_negative_rate_mean, ensemble_ranger_true_negative_rate_mean, ensemble_rf_true_negative_rate_mean, ensemble_svm_true_negative_rate_mean,
    ensemble_tree_true_negative_rate_mean
  ), each = numresamples)
)

true_negative_rate_plot <- ggplot2::ggplot(data = true_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("True negative rate, closer to one is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True negative rate, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_negative_rate_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_negative_rate_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_negative_rate_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_negative_rate_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_negative_rate_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_negative_rate_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_negative_rate_plot2 <- ggplot2::ggplot(data = true_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("True negative rate, closer to one is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True negative rate, closer to one is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_negative_rate_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_negative_rate_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_negative_rate_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_negative_rate_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_negative_rate_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_negative_rate_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### False positive rate plot ####
false_positive_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_false_positive_rate, bag_rf_false_positive_rate, C50_false_positive_rate, linear_false_positive_rate,
    n_bayes_false_positive_rate, pls_false_positive_rate, pda_false_positive_rate, rf_false_positive_rate, ranger_false_positive_rate, rpart_false_positive_rate, svm_false_positive_rate, tree_false_positive_rate,
    ensemble_bag_cart_false_positive_rate, ensemble_bag_rf_false_positive_rate, ensemble_C50_false_positive_rate,
    ensemble_n_bayes_false_positive_rate,
    ensemble_ranger_false_positive_rate, ensemble_rf_false_positive_rate, ensemble_svm_false_positive_rate, ensemble_tree_false_positive_rate
  ),
  "mean" = rep(c(
    bagging_false_positive_rate_mean, bag_rf_false_positive_rate_mean, C50_false_positive_rate_mean,
    linear_false_positive_rate_mean, n_bayes_false_positive_rate_mean, pls_false_positive_rate_mean,
    pda_false_positive_rate_mean, rf_false_positive_rate_mean, ranger_false_positive_rate_mean, rpart_false_positive_rate_mean, svm_false_positive_rate_mean, tree_false_positive_rate_mean,
    ensemble_bag_cart_false_positive_rate_mean, ensemble_bag_rf_false_positive_rate_mean, ensemble_C50_false_positive_rate_mean,
    ensemble_n_bayes_false_positive_rate_mean, ensemble_ranger_false_positive_rate_mean, ensemble_rf_false_positive_rate_mean, ensemble_svm_false_positive_rate_mean,
    ensemble_tree_false_positive_rate_mean
  ), each = numresamples)
)

false_positive_rate_plot <- ggplot2::ggplot(data = false_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("False positive rate, closer to zero is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "False positive rate, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_positive_rate_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_positive_rate_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_positive_rate_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_positive_rate_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_positive_rate_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_positive_rate_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

false_positive_rate_plot2 <- ggplot2::ggplot(data = false_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("False positive rate, closer to zero is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "False positive rate, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_positive_rate_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_positive_rate_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_positive_rate_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_positive_rate_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_positive_rate_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_positive_rate_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### False negative rate plot ####
false_negative_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_false_negative_rate, bag_rf_false_negative_rate, C50_false_negative_rate, linear_false_negative_rate,
    n_bayes_false_negative_rate, pls_false_negative_rate, pda_false_negative_rate, rf_false_negative_rate, ranger_false_negative_rate, rpart_false_negative_rate, svm_false_negative_rate, tree_false_negative_rate,
    ensemble_bag_cart_false_negative_rate, ensemble_bag_rf_false_negative_rate, ensemble_C50_false_negative_rate,
    ensemble_n_bayes_false_negative_rate,
    ensemble_ranger_false_negative_rate, ensemble_rf_false_negative_rate, ensemble_svm_false_negative_rate, ensemble_tree_false_negative_rate
  ),
  "mean" = rep(c(
    bagging_false_negative_rate_mean, bag_rf_false_negative_rate_mean, C50_false_negative_rate_mean,
    linear_false_negative_rate_mean, n_bayes_false_negative_rate_mean, pls_false_negative_rate_mean,
    pda_false_negative_rate_mean, rf_false_negative_rate_mean, ranger_false_negative_rate_mean, rpart_false_negative_rate_mean, svm_false_negative_rate_mean, tree_false_negative_rate_mean,
    ensemble_bag_cart_false_negative_rate_mean, ensemble_bag_rf_false_negative_rate_mean, ensemble_C50_false_negative_rate_mean,
    ensemble_n_bayes_false_negative_rate_mean, ensemble_ranger_false_negative_rate_mean, ensemble_rf_false_negative_rate_mean, ensemble_svm_false_negative_rate_mean,
    ensemble_tree_false_negative_rate_mean
  ), each = numresamples)
)

false_negative_rate_plot <- ggplot2::ggplot(data = false_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("False negative rate, closer to zero is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "False negative rate, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_negative_rate_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_negative_rate_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_negative_rate_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_negative_rate_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_negative_rate_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_negative_rate_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

false_negative_rate_plot2 <- ggplot2::ggplot(data = false_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("False negative rate, closer to zero is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "False negative rate, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_negative_rate_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_negative_rate_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_negative_rate_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_negative_rate_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_negative_rate_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_negative_rate_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Positive predictive value plot ####
positive_pred_value_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_positive_pred_value, bag_rf_positive_pred_value, C50_positive_pred_value, linear_positive_pred_value,
    n_bayes_positive_pred_value, pls_positive_pred_value, pda_positive_pred_value, rf_positive_pred_value, ranger_positive_pred_value, rpart_positive_pred_value, svm_positive_pred_value, tree_positive_pred_value,
    ensemble_bag_cart_positive_pred_value, ensemble_bag_rf_positive_pred_value, ensemble_C50_positive_pred_value,
    ensemble_n_bayes_positive_pred_value,
    ensemble_ranger_positive_pred_value, ensemble_rf_positive_pred_value, ensemble_svm_positive_pred_value, ensemble_tree_positive_pred_value
  ),
  "mean" = rep(c(
    bagging_positive_pred_value_mean, bag_rf_positive_pred_value_mean, C50_positive_pred_value_mean,
    linear_positive_pred_value_mean, n_bayes_positive_pred_value_mean, pls_positive_pred_value_mean,
    pda_positive_pred_value_mean, rf_positive_pred_value_mean, ranger_positive_pred_value_mean, rpart_positive_pred_value_mean, svm_positive_pred_value_mean, tree_positive_pred_value_mean,
    ensemble_bag_cart_positive_pred_value_mean, ensemble_bag_rf_positive_pred_value_mean, ensemble_C50_positive_pred_value_mean,
    ensemble_n_bayes_positive_pred_value_mean, ensemble_ranger_positive_pred_value_mean, ensemble_rf_positive_pred_value_mean, ensemble_svm_positive_pred_value_mean,
    ensemble_tree_positive_pred_value_mean
  ), each = numresamples)
)

positive_pred_value_plot <- ggplot2::ggplot(data = positive_pred_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Positive Pred Value, closer to one is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Positive Pred Value, closer to one is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("positive_pred_value_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("positive_pred_value_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("positive_pred_value_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("positive_pred_value_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("positive_pred_value_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("positive_pred_value_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

positive_pred_value_plot2 <- ggplot2::ggplot(data = positive_pred_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Positive Pred Value, closer to one is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Positive Pred Value, closer to one is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("positive_pred_value_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("positive_pred_value_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("positive_pred_value_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("positive_pred_value_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("positive_pred_value_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("positive_pred_value_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Negative predictive value plot ####
negative_pred_value_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_negative_pred_value, bag_rf_negative_pred_value, C50_negative_pred_value, linear_negative_pred_value,
    n_bayes_negative_pred_value, pls_negative_pred_value, pda_negative_pred_value, rf_negative_pred_value, ranger_negative_pred_value, rpart_negative_pred_value, svm_negative_pred_value, tree_negative_pred_value,
    ensemble_bag_cart_negative_pred_value, ensemble_bag_rf_negative_pred_value, ensemble_C50_negative_pred_value,
    ensemble_n_bayes_negative_pred_value,
    ensemble_ranger_negative_pred_value, ensemble_rf_negative_pred_value, ensemble_svm_negative_pred_value, ensemble_tree_negative_pred_value
  ),
  "mean" = rep(c(
    bagging_negative_pred_value_mean, bag_rf_negative_pred_value_mean, C50_negative_pred_value_mean,
    linear_negative_pred_value_mean, n_bayes_negative_pred_value_mean, pls_negative_pred_value_mean,
    pda_negative_pred_value_mean, rf_negative_pred_value_mean, ranger_negative_pred_value_mean, rpart_negative_pred_value_mean, svm_negative_pred_value_mean, tree_negative_pred_value_mean,
    ensemble_bag_cart_negative_pred_value_mean, ensemble_bag_rf_negative_pred_value_mean, ensemble_C50_negative_pred_value_mean,
    ensemble_n_bayes_negative_pred_value_mean, ensemble_ranger_negative_pred_value_mean, ensemble_rf_negative_pred_value_mean, ensemble_svm_negative_pred_value_mean,
    ensemble_tree_negative_pred_value_mean
  ), each = numresamples)
)

negative_pred_value_plot <- ggplot2::ggplot(data = negative_pred_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Negative Pred Value, closer to one is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Negative Pred Value, closer to one is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("negative_pred_value_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("negative_pred_value_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("negative_pred_value_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("negative_pred_value_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("negative_pred_value_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("negative_pred_value_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

negative_pred_value_plot2 <- ggplot2::ggplot(data = negative_pred_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Negative Pred Value, closer to one is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Negative Pred Value, closer to one is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("negative_pred_value_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("negative_pred_value_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("negative_pred_value_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("negative_pred_value_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("negative_pred_value_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("negative_pred_value_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Holdout vs train plot ####
holdout_vs_train_plot <- ggplot2::ggplot(data = holdout_vs_train_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Holdout accuracy / train accuracy by model, closer to one is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Holdout accuracy / train accuracy by model, closer to one is better. \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("holdout_vs_train_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("holdout_vs_train_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("holdout_vs_train_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("holdout_vs_train_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("holdout_vs_train_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("holdout_vs_train_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

holdout_vs_train_plot2 <- ggplot2::ggplot(data = holdout_vs_train_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Holdout accuracy / train accuracy by model, closer to one is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Holdout accuracy / train accuracy by model, closer to one is better. \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("holdout_vs_train_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("holdout_vs_train_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("holdout_vs_train_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("holdout_vs_train_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("holdout_vs_train_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("holdout_vs_train_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Classification error plot ####
classification_error_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "data" = c(
    bagging_classification_error, bag_rf_classification_error, C50_classification_error, linear_classification_error,
    n_bayes_classification_error, pls_classification_error, pda_classification_error, rf_classification_error, ranger_classification_error, rpart_classification_error, svm_classification_error, tree_classification_error,
    ensemble_bag_cart_classification_error, ensemble_bag_rf_classification_error, ensemble_C50_classification_error,
    ensemble_n_bayes_classification_error,
    ensemble_ranger_classification_error, ensemble_rf_classification_error, ensemble_svm_classification_error, ensemble_tree_classification_error
  ),
  "mean" = rep(c(
    bagging_classification_error_mean, bag_rf_classification_error_mean, C50_classification_error_mean,
    linear_classification_error_mean, n_bayes_classification_error_mean, pls_classification_error_mean,
    pda_classification_error_mean, rf_classification_error_mean, ranger_classification_error_mean, rpart_classification_error_mean, svm_classification_error_mean, tree_classification_error_mean,
    ensemble_bag_cart_classification_error_mean, ensemble_bag_rf_classification_error_mean, ensemble_C50_classification_error_mean,
    ensemble_n_bayes_classification_error_mean, ensemble_ranger_classification_error_mean, ensemble_rf_classification_error_mean, ensemble_svm_classification_error_mean,
    ensemble_tree_classification_error_mean
  ), each = numresamples)
)

classification_error_plot <- ggplot2::ggplot(data = classification_error_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Classification error, closer to zero is better, fixed scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Classification error, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("classification_error_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("classification_error_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("classification_error_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("classification_error_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("classification_error_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("classification_error_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

classification_error_plot2 <- ggplot2::ggplot(data = classification_error_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Classification error, closer to zero is better, free scales. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Classification error, closer to zero is better. \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("classification_error_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("classification_error_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("classification_error_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("classification_error_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("classification_error_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("classification_error_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


###### Total Data visualizations start here ########

total_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Bagging", numresamples), rep("Bagged Random Forest", numresamples),
    rep("C50", numresamples), rep("Linear", numresamples),
    rep("Naive Bayes", numresamples), rep("Partial Least Squares", numresamples),
    rep("Penalized Discriminant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples), rep("Trees", numresamples),
    rep("Ensemble Bagged Cart", numresamples), rep("Ensemble Bagged Random Forest", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Naive Bayes", numresamples), rep("Ensemble Ranger", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples)
  ),
  "train" = c(
    bagging_train_accuracy, bag_rf_train_accuracy, C50_train_accuracy,  linear_train_accuracy,
    n_bayes_train_accuracy, pls_train_accuracy, pda_train_accuracy, rf_train_accuracy, ranger_train_accuracy, rpart_train_accuracy, svm_train_accuracy, tree_train_accuracy,
    ensemble_bag_cart_train_accuracy, ensemble_bag_rf_train_accuracy, ensemble_C50_train_accuracy,
    ensemble_n_bayes_train_accuracy,
    ensemble_ranger_train_accuracy, ensemble_rf_train_accuracy, ensemble_svm_train_accuracy, ensemble_tree_train_accuracy
  ),
  "test" = c(
    bagging_test_accuracy, bag_rf_test_accuracy, C50_test_accuracy, linear_test_accuracy,
    n_bayes_test_accuracy, pls_test_accuracy, pda_test_accuracy, rf_test_accuracy, ranger_test_accuracy, rpart_test_accuracy, svm_test_accuracy, tree_test_accuracy,
    ensemble_bag_cart_test_accuracy, ensemble_bag_rf_test_accuracy, ensemble_C50_test_accuracy,
    ensemble_n_bayes_test_accuracy,
    ensemble_ranger_test_accuracy, ensemble_rf_test_accuracy, ensemble_svm_test_accuracy, ensemble_tree_test_accuracy
  ),
  "validation" = c(
    bagging_validation_accuracy, bag_rf_validation_accuracy, C50_validation_accuracy, linear_validation_accuracy,
    n_bayes_validation_accuracy, pls_validation_accuracy, pda_validation_accuracy, rf_validation_accuracy, ranger_validation_accuracy, rpart_validation_accuracy, svm_validation_accuracy, tree_validation_accuracy,
    ensemble_bag_cart_validation_accuracy, ensemble_bag_rf_validation_accuracy, ensemble_C50_validation_accuracy,
    ensemble_n_bayes_validation_accuracy,
    ensemble_ranger_validation_accuracy, ensemble_rf_validation_accuracy, ensemble_svm_validation_accuracy, ensemble_tree_validation_accuracy
  ),
  "holdout" = c(
    bagging_holdout, bag_rf_holdout, C50_holdout, linear_holdout,
    n_bayes_holdout, pls_holdout, pda_holdout, rf_holdout, ranger_holdout, rpart_holdout, svm_holdout, tree_holdout,
    ensemble_bag_cart_holdout, ensemble_bag_rf_holdout, ensemble_C50_holdout,
    ensemble_n_bayes_holdout,
    ensemble_ranger_holdout, ensemble_rf_holdout, ensemble_svm_holdout, ensemble_tree_holdout)
)

total_plot <- ggplot2::ggplot(data = total_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "fixed") +
  ggplot2::ggtitle("Accuracy data including train and holdout by model, fixed scales. \nAccuracy by model, higher is better, 1 is best.") +
  ggplot2::labs(y = "Higher is better, 1 is best.") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("train", "holdout"),
    values = c("train" = "red", "holdout" = "black")
  )

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

total_plot2 <- ggplot2::ggplot(data = total_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Accuracy data including train and holdout by model, free scales. \nAccuracy by model, higher is better, 1 is best.") +
  ggplot2::labs(y = "Higher is better, 1 is best.") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("train", "holdout"),
    values = c("train" = "red", "holdout" = "black")
  )

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot2.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot2.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot2.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot2.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot2.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot2.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Accuracy barchart ####
accuracy_barchart <- ggplot2::ggplot(Results, aes(x = reorder(Model, dplyr::desc(Mean_Holdout_Accuracy)), y = Mean_Holdout_Accuracy)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Mean Accuracy", title = "Model accuracy, closer to one is better, 1 standard deviation error bars") +
  ggplot2::geom_text(aes(label = Mean_Holdout_Accuracy), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Mean_Holdout_Accuracy - Accuracy_Holdout_Std.Dev,
                             ymax =  Mean_Holdout_Accuracy + Accuracy_Holdout_Std.Dev))

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_barchart.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_barchart.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_barchart.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_barchart.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_barchart.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_barchart.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Holdout vs train barchart ####
holdout_vs_train_barchart <- ggplot2::ggplot(Results, aes(x = reorder(Model, dplyr::desc(Holdout_vs_train)), y = Holdout_vs_train)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Holdout accuracy / train accuracy", title = "Over or Under Fitting, closer to 1 is better, 1 standard deviation error bars") +
  ggplot2::geom_text(aes(label = Holdout_vs_train), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(Results$Holdout_vs_train) +1.0) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Holdout_vs_train - Holdout_vs_train_St_Dev, ymax = Holdout_vs_train + Holdout_vs_train_St_Dev))

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("holdout_vs_train_barchart.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("holdout_vs_train_barchart.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("holdout_vs_train_barchart.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("holdout_vs_train_barchart.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("holdout_vs_train_barchart.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("holdout_vs_train_barchart.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Duration barchart ####
duration_barchart <- ggplot2::ggplot(Results, aes(x = reorder(Model, Duration), y = Duration)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Duration", title = "Duration, shorter is better, 1 standard deviation error bars") +
  ggplot2::geom_text(aes(label = Duration), vjust = 0,hjust = -0.5, angle = 90) +
  #ggplot2::ylim(0, max(Results$Duration) + 1) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Duration - Duration_St_Dev, ymax = Duration + Duration_St_Dev))

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("duration_barchart.eps", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("duration_barchart.jpeg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("duration_barchart.pdf", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("duration_barchart.png", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("duration_barchart.svg", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("duration_barchart.tiff", width = width, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Predict on new data starts here ####
if (predict_on_new_data == "Y") {
  is_num <- sapply(new_data, is.integer)
  new_data[ , is_num] <- as.data.frame(apply(new_data[, is_num], 2, as.numeric))

  Bagged_Random_Forest <- predict(object = bag_rf_train_fit, newdata = new_data)
  Bagging <- predict(object = bagging_train_fit, newdata = new_data)
  C50 <- predict(object = C50_train_fit, newdata = new_data)
  Linear <- predict(object = linear_train_fit, newdata = new_data)
  Naive_Bayes <- predict(object = n_bayes_train_fit, newdata = new_data)
  Partial_Least_Squares <- predict(object = pls_train_fit, newdata = new_data)
  Penalized_Discriminant_Analysis <- predict(object = pda_train_fit, newdata = new_data)
  Random_Forest <- predict(object = rf_train_fit, newdata = new_data)
  Ranger <- predict(object = ranger_train_fit, newdata = new_data)
  RPart <- predict(object = rpart_train_fit, newdata = new_data)
  Support_Vector_Machines <- predict(object = svm_train_fit, newdata = new_data)
  Trees <- predict(tree_train_fit, new_data, type = "class")

  new_ensemble <- data.frame(
    Bagged_Random_Forest,
    Bagging,
    C50,
    Linear,
    Naive_Bayes,
    Partial_Least_Squares,
    Penalized_Discriminant_Analysis,
    Random_Forest,
    Ranger,
    RPart,
    Support_Vector_Machines,
    Trees
  )

  new_ensemble_row_numbers <- as.numeric(row.names(new_data))
  new_ensemble$y <- new_data$y

  new_ensemble_bagged_cart <- predict(object = ensemble_bag_cart_train_fit, newdata = new_ensemble)
  new_ensemble_bag_rf <- predict(object = ensemble_bag_train_rf, newdata = new_ensemble)
  new_ensemble_C50 <- predict(object = ensemble_C50_train_fit, newdata = new_ensemble)
  new_ensemble_n_bayes <- predict(object = ensemble_n_bayes_train_fit, newdata = new_ensemble)
  new_ensemble_rf <- predict(object = ensemble_train_rf_fit, newdata = new_ensemble)
  new_ensemble_svm <- predict(object = ensemble_svm_train_fit, newdata = new_ensemble)
  new_ensemble_trees <- predict(object = ensemble_tree_train_fit, newdata = new_ensemble)

  New_Data_Results <- data.frame(
    "True_Value" = new_data$y,
    "Bagged_Random_Forest" = Bagged_Random_Forest,
    "Bagging" = Bagging,
    "C50" = C50,
    "Linear" = Linear,
    "Naive_Bayes" = Naive_Bayes,
    "Partial_Least_Squares" = Partial_Least_Squares,
    "Penalized_Discriminant_Analysis" = Penalized_Discriminant_Analysis,
    "Random_Forest" = Random_Forest,
    "Ranger" = Ranger,
    "RPart" = RPart,
    "Support_Vector_Machines" = Support_Vector_Machines,
    "Trees" = Trees,
    "Ensemble_Bagged_Cart" = new_ensemble_bagged_cart,
    "Ensemble_Bagged_Random_Forest" = new_ensemble_bag_rf,
    "Ensemble_C50" = new_ensemble_C50,
    "Ensemble_Naive_Bayes" = new_ensemble_n_bayes,
    "Ensemble_Random_Forest" = new_ensemble_rf,
    "Ensemble_Support_Vector_Machines" = new_ensemble_svm
  )

  New_Data_Results <- t(New_Data_Results)

  New_Data_Results <- reactable::reactable(New_Data_Results,
                                           searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                           striped = TRUE, highlight = TRUE, resizable = TRUE
  )

  if(save_all_plots == "Y"){
    reactablefmtr::save_reactable_test(New_Data_Results, "New_Data_Results.html")
  }

  if (save_all_trained_models == "Y") {
    bagging_train_fit <<- bagging_train_fit
    bag_rf_train_fit <<- bag_rf_train_fit
    C50_train_fit <<- C50_train_fit
    linear_train_fit <<- linear_train_fit
    n_bayes_train_fit <<- n_bayes_train_fit
    pls_train_fit <<- pls_train_fit
    pda_train_fit <<- pda_train_fit
    rf_train_fit <<- rf_train_fit
    ranger_train_fit <<- ranger_train_fit
    rpart_train_fit <<- rpart_train_fit
    svm_train_fit <<- svm_train_fit
    tree_train_fit <<- tree_train_fit

    ensemble_bag_cart_train_fit <<- ensemble_bag_cart_train_fit
    ensemble_bag_train_rf <<- ensemble_bag_train_rf
    ensemble_C50_train_fit <<- ensemble_C50_train_fit
    ensemble_n_bayes_train_fit <<- ensemble_n_bayes_train_fit
    ensemble_ranger_train_fit <<- ensemble_ranger_train_fit
    ensemble_train_rf_fit <<- ensemble_train_rf_fit
    ensemble_svm_train_fit <<- ensemble_svm_train_fit
    ensemble_tree_train_fit <<- ensemble_tree_train_fit
  }

  return(list(
    "Final_Results" = Final_results, "Barcharts" = barchart, "Accuracy_Barchart" = accuracy_barchart, "holdout_vs_train_barchart" = holdout_vs_train_barchart,
    "Duration_barchart" = duration_barchart, "Data summary" = data_summary, "Correlation_Matrix" = correlation_marix, 'VIF' = VIF, "Boxplots" = boxplots, "Histograms" = histograms, "Head of data" = head_data, "Head of Ensembles" = head_ensemble,
    "Summary_Tables" = summary_tables, "Accuracy_Plot" = accuracy_plot, "Total_Plot" = total_plot, "holdout_vs_train_plot" = holdout_vs_train_plot, "New_Data_Results" = New_Data_Results
  ))

} # Matches the { on line 3805

if (save_all_trained_models == "Y") {
  bagging_train_fit <<- bagging_train_fit
  bag_rf_train_fit <<- bag_rf_train_fit
  C50_train_fit <<- C50_train_fit
  linear_train_fit <<- linear_train_fit
  n_bayes_train_fit <<- n_bayes_train_fit
  pls_train_fit <<- pls_train_fit
  rf_train_fit <<- rf_train_fit
  ranger_train_fit <<- ranger_train_fit
  rpart_train_fit <<- rpart_train_fit
  svm_train_fit <<- svm_train_fit
  tree_train_fit <<- tree_train_fit

  ensemble_bag_cart_train_fit <<- ensemble_bag_cart_train_fit
  ensemble_bag_train_rf <<- ensemble_bag_train_rf
  ensemble_C50_train_fit <<- ensemble_C50_train_fit
  ensemble_n_bayes_train_fit <<- ensemble_n_bayes_train_fit
  ensemble_ranger_train_fit <<- ensemble_ranger_train_fit
  ensemble_train_rf_fit <<- ensemble_train_rf_fit
  ensemble_svm_train_fit <<- ensemble_svm_train_fit
  ensemble_tree_train_fit <<- ensemble_tree_train_fit
}

#### Return list of all reports ####
return(list(
  'Final_results' = Final_results, 'Barchart_values' = barchart, 'Barchart_percent' = barchart2, "Accuracy_Barchart" = accuracy_barchart, "holdout_vs_train_barchart" = holdout_vs_train_barchart,
  'True_positive_rate_fixed_scales' = true_positive_rate_plot, 'True_positive_rate_free_scales' = true_positive_rate_plot2,
  'True_negative_rate_fixed_scales' = true_negative_rate_plot, 'True_negative_rate_free_scales' = true_negative_rate_plot2,
  'False_positive_rate_fixed_scales' = false_positive_rate_plot, 'False_positive_rate_free_scales' = false_positive_rate_plot2,
  'False_negative_rate_fixed_scales' = false_negative_rate_plot, 'False_negative_rate_free_scales' = false_negative_rate_plot2,
  "Duration_barchart" = duration_barchart,  'Data_summary' = data_summary, 'Correlation_matrix' = correlation_marix,
  'VIF' = VIF, "Stratified sampling report" = stratified_sampling_report,
  'Boxplots' = boxplots, 'Histograms' = histograms, 'Head_of_data' = head_df, 'Head_of_ensemble' = head_ensemble,
  'Summary_tables' = summary_tables, 'Accuracy_plot_fixed_scales' = accuracy_plot, 'Accuracy_plot_free_scales' = accuracy_plot2,  'Total_plot_fixed_scales' = total_plot, "Total_plot_free_scales" = total_plot2,
  'Classification_error_fixed_scales' = classification_error_plot, 'Classification_error_free_scales' = classification_error_plot2,
  'Residuals_fixed_scales' = residuals_plot, 'Residuals_free_scales' = residuals_plot2, "Holdout_vs_train_plot" = holdout_vs_train_plot
)
)
}
