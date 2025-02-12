#' classification—function to perform classification analysis and return results to the user.

#' @param data a data set that includes classification data. For example, the Carseats data in the ISLR package
#' @param colnum the number of the column. For example, in the Carseats data this is column 7, ShelveLoc with three values, Good, Medium and Bad
#' @param numresamples the number of times to resample the analysis
#' @param how_to_handle_strings Converts strings to factor levels
#' @param do_you_have_new_data asks if the user has new data to be analyzed using the trained models that were just developed
#' @param save_all_trained_models Gives the user the option to save all trained models in the Environment
#' @param use_parallel "Y" or "N" for parallel processing
#' @param train_amount set the amount for the training data
#' @param test_amount set the amount for the testing data
#' @param validation_amount Set the amount for the validation data
#'
#' @returns a full analysis, including data visualizations, statistical summaries, and a full report on the results of 35 models on the data
#' @export Classification


#' @importFrom C50 C5.0
#' @importFrom class knn
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
#' @importFrom MASS lda
#' @importFrom mda gen.ridge
#' @importFrom parallel makeCluster
#' @importFrom pls plsr
#' @importFrom purrr keep
#' @importFrom randomForest randomForest
#' @importFrom ranger ranger
#' @importFrom reactable reactable
#' @importFrom reactablefmtr add_title
#' @importFrom scales percent
#' @importFrom stats complete.cases cor predict reorder sd
#' @importFrom tidyr gather pivot_longer
#' @importFrom tree tree cv.tree prune.misclass
#' @importFrom utils head read.csv str


Classification <- function(data, colnum, numresamples, do_you_have_new_data = c("Y", "N"), how_to_handle_strings = c(0("No strings"), 1("Strings as factors")), save_all_trained_models = c("Y", "N"),
                           use_parallel = c("Y", "N"), train_amount, test_amount, validation_amount) {

use_parallel <- 0
no_cores <- 0

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}

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

if (do_you_have_new_data == "Y") {
  new_data <- readline("What is the URL of the new data? ")
  new_data <- read.csv(new_data, stringsAsFactors = TRUE)

  y <- 0
  colnames(new_data)[colnum] <- "y"

  new_data <- new_data %>% dplyr::relocate(y, .after = tidyr::last_col()) # Moves the target column to the last column on the right
}


#### Set accuracy values to zero ####

bagging_train_accuracy <- 0
bagging_test_accuracy <- 0
bagging_validation_accuracy <- 0
bagging_overfitting <- 0
bagging_holdout <- 0
bagging_duration <- 0
bagging_true_positive_rate <- 0
bagging_true_negative_rate <- 0
bagging_false_positive_rate <- 0
bagging_false_negative_rate <- 0
bagging_F1_score <- 0
bagging_table_total <- 0

bag_cart_train_accuracy <- 0
bag_cart_test_accuracy <- 0
bag_cart_validation_accuracy <- 0
bag_cart_overfitting <- 0
bag_cart_holdout <- 0
bag_cart_duration <- 0
bag_cart_true_positive_rate <- 0
bag_cart_true_negative_rate <- 0
bag_cart_false_positive_rate <- 0
bag_cart_false_negative_rate <- 0
bag_cart_F1_score <- 0
bag_cart_table_total <- 0

bag_rf_train_accuracy <- 0
bag_rf_test_accuracy <- 0
bag_rf_validation_accuracy <- 0
bag_rf_overfitting <- 0
bag_rf_holdout <- 0
bag_rf_duration <- 0
bag_rf_true_positive_rate <- 0
bag_rf_true_negative_rate <- 0
bag_rf_false_positive_rate <- 0
bag_rf_false_negative_rate <- 0
bag_rf_F1_score <- 0
bag_rf_table_total <- 0

C50_train_accuracy <- 0
C50_test_accuracy <- 0
C50_validation_accuracy <- 0
C50_overfitting <- 0
C50_holdout <- 0
C50_duration <- 0
C50_true_positive_rate <- 0
C50_true_negative_rate <- 0
C50_false_positive_rate <- 0
C50_false_negative_rate <- 0
C50_F1_score <- 0
C50_table_total <- 0

linear_train_accuracy <- 0
linear_validation_accuracy <- 0
linear_test_accuracy <- 0
linear_test_accuracy_mean <- 0
linear_validation_accuracy_mean <- 0
linear_overfitting <- 0
linear_holdout <- 0
linear_duration <- 0
linear_true_positive_rate <- 0
linear_true_negative_rate <- 0
linear_false_positive_rate <- 0
linear_false_negative_rate <- 0
linear_F1_score <- 0
linear_table_total <- 0

n_bayes_train_accuracy <- 0
n_bayes_test_accuracy <- 0
n_bayes_validation_accuracy <- 0
n_bayes_accuracy <- 0
n_bayes_test_accuracy_mean <- 0
n_bayes_validation_accuracy_mean <- 0
n_bayes_overfitting <- 0
n_bayes_holdout <- 0
n_bayes_duration <- 0
n_bayes_true_positive_rate <- 0
n_bayes_true_negative_rate <- 0
n_bayes_false_positive_rate <- 0
n_bayes_false_negative_rate <- 0
n_bayes_F1_score <- 0
n_bayes_table_total <- 0

pls_train_accuracy <- 0
pls_test_accuracy <- 0
pls_test_accuracy_mean <- 0
pls_validation_accuracy <- 0
pls_validation_accuracy_mean <- 0
pls_overfitting <- 0
pls_holdout <- 0
pls_duration <- 0
pls_true_positive_rate <- 0
pls_true_negative_rate <- 0
pls_false_positive_rate <- 0
pls_false_negative_rate <- 0
pls_F1_score <- 0
pls_table_total <- 0

pda_train_accuracy <- 0
pda_test_accuracy <- 0
pda_test_accuracy_mean <- 0
pda_validation_accuracy <- 0
pda_validation_accuracy_mean <- 0
pda_overfitting <- 0
pda_holdout <- 0
pda_duration <- 0
pda_true_positive_rate <- 0
pda_true_negative_rate <- 0
pda_false_positive_rate <- 0
pda_false_negative_rate <- 0
pda_F1_score <- 0
pda_table_total <- 0

rf_train_accuracy <- 0
rf_test_accuracy <- 0
rf_test_accuracy_mean <- 0
rf_validation_accuracy <- 0
rf_validation_accuracy_mean <- 0
rf_overfitting <- 0
rf_overfitting_holdout <- 0
rf_holdout <- 0
rf_duration <- 0
rf_true_positive_rate <- 0
rf_true_negative_rate <- 0
rf_false_positive_rate <- 0
rf_false_negative_rate <- 0
rf_F1_score <- 0
rf_table_total <- 0

ranger_train_accuracy <- 0
ranger_test_accuracy <- 0
ranger_test_accuracy_mean <- 0
ranger_validation_accuracy <-
  ranger_validation_accuracy_mean <- 0
ranger_overfitting <- 0
ranger_holdout <- 0
ranger_duration <- 0
ranger_true_positive_rate <- 0
ranger_true_negative_rate <- 0
ranger_false_positive_rate <- 0
ranger_false_negative_rate <- 0
ranger_F1_score <- 0
ranger_table_total <- 0

rpart_train_accuracy <- 0
rpart_test_accuracy <- 0
rpart_test_accuracy_mean <- 0
rpart_validation_accuracy <- 0
rpart_validation_accuracy_mean <- 0
rpart_overfitting <- 0
rpart_holdout <- 0
rpart_duration <- 0
rpart_true_positive_rate <- 0
rpart_true_negative_rate <- 0
rpart_false_positive_rate <- 0
rpart_false_negative_rate <- 0
rpart_F1_score <- 0
rpart_table_total <- 0

svm_train_accuracy <- 0
svm_test_accuracy <- 0
svm_test_accuracy_mean <- 0
svm_validation_accuracy <- 0
svm_validation_accuracy_mean <- 0
svm_overfitting <- 0
svm_holdout <- 0
svm_duration <- 0
svm_true_positive_rate <- 0
svm_true_negative_rate <- 0
svm_false_positive_rate <- 0
svm_false_negative_rate <- 0
svm_F1_score <- 0
svm_table_total <- 0

tree_train_accuracy <- 0
tree_test_accuracy <- 0
tree_test_accuracy_mean <- 0
tree_validation_accuracy <- 0
tree_validation_accuracy_mean <- 0
tree_overfitting <- 0
tree_holdout <- 0
tree_duration <- 0
tree_true_positive_rate <- 0
tree_true_negative_rate <- 0
tree_false_positive_rate <- 0
tree_false_negative_rate <- 0
tree_F1_score <- 0
tree_table_total <- 0

ensemble_adaboost_train_accuracy <- 0
ensemble_adaboost_train_accuracy_mean <- 0
ensemble_adaboost_test_accuracy <- 0
ensemble_adaboost_test_accuracy_mean <- 0
ensemble_adaboost_validation_accuracy <- 0
ensemble_adaboost_validation_accuracy_mean <- 0
ensemble_adaboost_overfitting <- 0
ensemble_adaboost_holdout <- 0
ensemble_adaboost_duration <- 0
ensemble_adaboost_true_positive_rate <- 0
ensemble_adaboost_true_negative_rate <- 0
ensemble_adaboost_false_positive_rate <- 0
ensemble_adaboost_false_negative_rate <- 0
ensemble_adaboost_F1_score <- 0
ensemble_adaboost_table_total <- 0

ensemble_bag_cart_train_accuracy <- 0
ensemble_bag_cart_train_accuracy_mean <- 0
ensemble_bag_cart_test_accuracy <- 0
ensemble_bag_cart_test_accuracy_mean <- 0
ensemble_bag_cart_validation_accuracy <- 0
ensemble_bag_cart_validation_accuracy_mean <- 0
ensemble_bag_cart_overfitting <- 0
ensemble_bag_cart_holdout <- 0
ensemble_bag_cart_duration <- 0
ensemble_bag_cart_true_positive_rate <- 0
ensemble_bag_cart_true_negative_rate <- 0
ensemble_bag_cart_false_positive_rate <- 0
ensemble_bag_cart_false_negative_rate <- 0
ensemble_bag_cart_F1_score <- 0
ensemble_bag_cart_table_total <- 0

ensemble_bag_rf_train_accuracy <- 0
ensemble_bag_rf_train_accuracy_mean <- 0
ensemble_bag_rf_test_accuracy <- 0
ensemble_bag_rf_test_accuracy_mean <- 0
ensemble_bag_rf_validation_accuracy <- 0
ensemble_bag_rf_validation_accuracy_mean <- 0
ensemble_bag_rf_overfitting <- 0
ensemble_bag_rf_holdout <- 0
ensemble_bag_rf_duration <- 0
ensemble_bag_rf_true_positive_rate <- 0
ensemble_bag_rf_true_negative_rate <- 0
ensemble_bag_rf_false_positive_rate <- 0
ensemble_bag_rf_false_negative_rate <- 0
ensemble_bag_rf_F1_score <- 0
ensemble_bag_rf_table_total <- 0

ensemble_C50_train_accuracy <- 0
ensemble_C50_train_accuracy_mean <- 0
ensemble_C50_test_accuracy <- 0
ensemble_C50_test_accuracy_mean <- 0
ensemble_C50_validation_accuracy <- 0
ensemble_C50_validation_accuracy_mean <- 0
ensemble_C50_overfitting <- 0
ensemble_C50_holdout <- 0
ensemble_C50_duration <- 0
ensemble_C50_true_positive_rate <- 0
ensemble_C50_true_negative_rate <- 0
ensemble_C50_false_positive_rate <- 0
ensemble_C50_false_negative_rate <- 0
ensemble_C50_F1_score <- 0
ensemble_C50_table_total <- 0

ensemble_n_bayes_train_accuracy <- 0
ensemble_n_bayes_train_accuracy_mean <- 0
ensemble_n_bayes_test_accuracy <- 0
ensemble_n_bayes_test_accuracy_mean <- 0
ensemble_n_bayes_validation_accuracy <- 0
ensemble_n_bayes_validation_accuracy_mean <- 0
ensemble_n_bayes_overfitting <- 0
ensemble_n_bayes_holdout <- 0
ensemble_n_bayes_duration <- 0
ensemble_n_bayes_true_positive_rate <- 0
ensemble_n_bayes_true_negative_rate <- 0
ensemble_n_bayes_false_positive_rate <- 0
ensemble_n_bayes_false_negative_rate <- 0
ensemble_n_bayes_F1_score <- 0
ensemble_n_bayes_table_total <- 0

ensemble_ranger_train_accuracy <- 0
ensemble_ranger_train_accuracy_mean <- 0
ensemble_ranger_test_accuracy <- 0
ensemble_ranger_test_accuracy_mean <- 0
ensemble_ranger_validation_accuracy <- 0
ensemble_ranger_validation_accuracy_mean <- 0
ensemble_ranger_overfitting <- 0
ensemble_ranger_holdout <- 0
ensemble_ranger_duration <- 0
ensemble_ranger_true_positive_rate <- 0
ensemble_ranger_true_negative_rate <- 0
ensemble_ranger_false_positive_rate <- 0
ensemble_ranger_false_negative_rate <- 0
ensemble_ranger_F1_score <- 0
ensemble_ranger_table_total <- 0

ensemble_rf_train_accuracy <- 0
ensemble_rf_train_accuracy_mean <- 0
ensemble_rf_test_accuracy <- 0
ensemble_rf_test_accuracy_mean <- 0
ensemble_rf_validation_accuracy <- 0
ensemble_rf_validation_accuracy_mean <- 0
ensemble_rf_overfitting <- 0
ensemble_rf_holdout <- 0
ensemble_rf_duration <- 0
ensemble_rf_true_positive_rate <- 0
ensemble_rf_true_negative_rate <- 0
ensemble_rf_false_positive_rate <- 0
ensemble_rf_false_negative_rate <- 0
ensemble_rf_F1_score <- 0
ensemble_rf_table_total <- 0

ensemble_svm_train_accuracy <- 0
ensemble_svm_train_accuracy_mean <- 0
ensemble_svm_test_accuracy <- 0
ensemble_svm_test_accuracy_mean <- 0
ensemble_svm_validation_accuracy <- 0
ensemble_svm_validation_accuracy_mean <- 0
ensemble_svm_overfitting <- 0
ensemble_svm_holdout <- 0
ensemble_svm_duration <- 0
ensemble_svm_true_positive_rate <- 0
ensemble_svm_true_negative_rate <- 0
ensemble_svm_false_positive_rate <- 0
ensemble_svm_false_negative_rate <- 0
ensemble_svm_F1_score <- 0
ensemble_svm_table_total <- 0

ensemble_tree_train_accuracy <- 0
ensemble_tree_train_accuracy_mean <- 0
ensemble_tree_test_accuracy <- 0
ensemble_tree_test_accuracy_mean <- 0
ensemble_tree_validation_accuracy <- 0
ensemble_tree_validation_accuracy_mean <- 0
ensemble_tree_overfitting <- 0
ensemble_tree_holdout <- 0
ensemble_tree_duration <- 0
ensemble_tree_true_positive_rate <- 0
ensemble_tree_true_negative_rate <- 0
ensemble_tree_false_positive_rate <- 0
ensemble_tree_false_negative_rate <- 0
ensemble_tree_F1_score <- 0
ensemble_tree_table_total <- 0

value <- 0
cols <- 0
Mean_Holdout_Accuracy <- 0
count <- 0
model <- 0
holdout <- 0
barchart <- 0
name <- 0
perc <- 0
Model <- 0
Overfitting <- 0
Duration <- 0
Accuracy_Holdout_Std.Dev <- 0
Overfitting_St_Dev <- 0
Duration_St_Dev <- 0


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
  ggplot2::geom_text(aes(label = scales::percent(perc),
                         vjust = 1.5),
                     col = "white") +
  ggplot2::facet_wrap(~ name, scales = "free") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.25)))

data_summary <- summary(df)

head_data <- reactable::reactable(head(df),
                                  searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                  striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Head of the data frame")

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


#### Pariwise scatter plot ####

panel.hist <- function(x, ...) {
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(df, panel = graphics::panel.smooth, main = "Pairwise scatter plots and histograms of the numerical data",
      lower.panel = graphics::panel.smooth, diag.panel = panel.hist
)

#### Boxplots of the numeric data ####
boxplots <- df1 %>%
  tidyr::gather(key = "var", value = "value") %>%
  ggplot2::ggplot(ggplot2::aes(x = "", y = value)) +
  ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggplot2::facet_wrap(~var, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Boxplots of the numeric data")
# Thanks to https://rstudio-pubs-static.s3.amazonaws.com/388596_e21196f1adf04e0ea7cd68edd9eba966.html

#### Histograms of the numeric data ####
histograms <- ggplot2::ggplot(tidyr::gather(df1, cols, value), ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = round(nrow(df1) / 10)) +
  ggplot2::facet_wrap(. ~ cols, scales = "free") +
  ggplot2::labs(title = "Histograms of each numeric column. Each bar = 10 rows of data")

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


  #### 3. Bagging ####
  bagging_start <- Sys.time()
  print("Working on Bagging analysis")
  bagging_train_fit <- ipred::bagging(y ~ ., data = train01, coob = TRUE)
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
  bagging_holdout_sd <- sd(bagging_holdout)
  bagging_overfitting[i] <- bagging_holdout_mean / bagging_train_accuracy_mean
  bagging_overfitting_mean <- mean(bagging_overfitting)
  bagging_overfitting_range <- range(bagging_overfitting)
  bagging_overfitting_sd <- sd(bagging_overfitting)

  bagging_table <- bagging_test_table + bagging_validation_table
  bagging_table_total <- bagging_table_total + bagging_table
  bagging_table_sum_diag <- sum(diag(bagging_table))

  bagging_true_positive_rate[i] <- sum(diag(bagging_table_total)) / sum(bagging_table_total)
  bagging_true_positive_rate_mean <- mean(bagging_true_positive_rate[i])
  bagging_true_negative_rate[i] <- 0.5 * (sum(diag(bagging_table_total))) / sum(bagging_table_total)
  bagging_true_negative_rate_mean <- mean(bagging_true_negative_rate)
  bagging_false_negative_rate[i] <- 1 - bagging_true_positive_rate[i]
  bagging_false_negative_rate_mean <- mean(bagging_false_negative_rate)
  bagging_false_positive_rate[i] <- 1 - bagging_true_negative_rate[i]
  bagging_false_positive_rate_mean <- mean(bagging_false_positive_rate)
  bagging_F1_score[i] <- 2 * bagging_true_positive_rate[i] / (2 * bagging_true_positive_rate[i] + bagging_false_positive_rate[i] + bagging_false_negative_rate[i])
  bagging_F1_score_mean <- mean(bagging_F1_score[i])

  bagging_end <- Sys.time()
  bagging_duration[i] <- bagging_end - bagging_start
  bagging_duration_mean <- mean(bagging_duration)
  bagging_duration_sd <- sd(bagging_duration)

  #### 4. Bagged Random Forest ####
  bag_rf_start <- Sys.time()
  print("Working on Bagged Random Forest analysis")
  bag_rf_train_fit <- randomForest::randomForest(y ~ ., data = train01, mtry = ncol(train))
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
  bag_rf_holdout_sd <- sd(bag_rf_holdout)
  bag_rf_overfitting[i] <- bag_rf_holdout_mean / bag_rf_train_accuracy_mean
  bag_rf_overfitting_mean <- mean(bag_rf_overfitting)
  bag_rf_overfitting_range <- range(bag_rf_overfitting)
  bag_rf_overfitting_sd <- sd(bag_rf_overfitting)

  bag_rf_table <- bag_rf_test_table + bag_rf_validation_table
  bag_rf_table_total <- bag_rf_table_total + bag_rf_table
  bag_rf_table_sum_diag <- sum(diag(bag_rf_table))

  bag_rf_true_positive_rate[i] <- sum(diag(bag_rf_table_total)) / sum(bag_rf_table_total)
  bag_rf_true_positive_rate_mean <- mean(bag_rf_true_positive_rate[i])
  bag_rf_true_negative_rate[i] <- 0.5 * (sum(diag(bag_rf_table_total))) / sum(bag_rf_table_total)
  bag_rf_true_negative_rate_mean <- mean(bag_rf_true_negative_rate)
  bag_rf_false_negative_rate[i] <- 1 - bag_rf_true_positive_rate[i]
  bag_rf_false_negative_rate_mean <- mean(bag_rf_false_negative_rate)
  bag_rf_false_positive_rate[i] <- 1 - bag_rf_true_negative_rate[i]
  bag_rf_false_positive_rate_mean <- mean(bag_rf_false_positive_rate)
  bag_rf_F1_score[i] <- 2 * bag_rf_true_positive_rate[i] / (2 * bag_rf_true_positive_rate[i] + bag_rf_false_positive_rate[i] + bag_rf_false_negative_rate[i])
  bag_rf_F1_score_mean <- mean(bag_rf_F1_score[i])

  bag_rf_end <- Sys.time()
  bag_rf_duration[i] <- bag_rf_end - bag_rf_start
  bag_rf_duration_mean <- mean(bag_rf_duration)
  bag_rf_duration_sd <- sd(bag_rf_duration)

  #### 5. C50 ####
  C50_start <- Sys.time()
  print("Working on C50 analysis")
  C50_train_fit <- C50::C5.0(as.factor(y_train) ~ ., data = train)
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
  C50_holdout_sd <- sd(C50_holdout)
  C50_overfitting[i] <- C50_holdout_mean / C50_train_accuracy_mean
  C50_overfitting_mean <- mean(C50_overfitting)
  C50_overfitting_range <- range(C50_overfitting)
  C50_overfitting_sd <- sd(C50_overfitting)

  C50_table <- C50_test_table + C50_validation_table
  C50_table_total <- C50_table_total + C50_table
  C50_table_sum_diag <- sum(diag(C50_table))

  C50_true_positive_rate[i] <- sum(diag(C50_table_total)) / sum(C50_table_total)
  C50_true_positive_rate_mean <- mean(C50_true_positive_rate[i])
  C50_true_negative_rate[i] <- 0.5 * (sum(diag(C50_table_total))) / sum(C50_table_total)
  C50_true_negative_rate_mean <- mean(C50_true_negative_rate)
  C50_false_negative_rate[i] <- 1 - C50_true_positive_rate[i]
  C50_false_negative_rate_mean <- mean(C50_false_negative_rate)
  C50_false_positive_rate[i] <- 1 - C50_true_negative_rate[i]
  C50_false_positive_rate_mean <- mean(C50_false_positive_rate)
  C50_F1_score[i] <- 2 * C50_true_positive_rate[i] / (2 * C50_true_positive_rate[i] + C50_false_positive_rate[i] + C50_false_negative_rate[i])
  C50_F1_score_mean <- mean(C50_F1_score[i])

  C50_end <- Sys.time()
  C50_duration[i] <- C50_end - C50_start
  C50_duration_mean <- mean(C50_duration)
  C50_duration_sd <- sd(C50_duration)


  #### 10. Linear Model ####
  linear_start <- Sys.time()
  print("Working on Linear analysis")
  linear_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "LMModel")
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
  linear_holdout_sd <- sd(linear_holdout)
  linear_overfitting[i] <- linear_holdout_mean / linear_train_accuracy_mean
  linear_overfitting_mean <- mean(linear_overfitting)
  linear_overfitting_range <- range(linear_overfitting)
  linear_overfitting_sd <- sd(linear_overfitting)

  linear_table <- linear_test_table + linear_validation_table
  linear_table_total <- linear_table_total + linear_table
  linear_table_sum_diag <- sum(diag(linear_table))

  linear_true_positive_rate[i] <- sum(diag(linear_table_total)) / sum(linear_table_total)
  linear_true_positive_rate_mean <- mean(linear_true_positive_rate[i])
  linear_true_negative_rate[i] <- 0.5 * (sum(diag(linear_table_total))) / sum(linear_table_total)
  linear_true_negative_rate_mean <- mean(linear_true_negative_rate)
  linear_false_negative_rate[i] <- 1 - linear_true_positive_rate[i]
  linear_false_negative_rate_mean <- mean(linear_false_negative_rate)
  linear_false_positive_rate[i] <- 1 - linear_true_negative_rate[i]
  linear_false_positive_rate_mean <- mean(linear_false_positive_rate)
  linear_F1_score[i] <- 2 * linear_true_positive_rate[i] / (2 * linear_true_positive_rate[i] + linear_false_positive_rate[i] + linear_false_negative_rate[i])
  linear_F1_score_mean <- mean(linear_F1_score[i])

  linear_end <- Sys.time()
  linear_duration[i] <- linear_end - linear_start
  linear_duration_mean <- mean(linear_duration)
  linear_duration_sd <- sd(linear_duration)


  #### Naive Bayes ####
  n_bayes_start <- Sys.time()
  print("Working on Naive Bayes analysis")
  n_bayes_train_fit <- e1071::naiveBayes(y_train ~ ., data = train)
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
  n_bayes_holdout_sd <- sd(n_bayes_holdout)
  n_bayes_overfitting[i] <- n_bayes_holdout_mean / n_bayes_train_accuracy_mean
  n_bayes_overfitting_mean <- mean(n_bayes_overfitting)
  n_bayes_overfitting_range <- range(n_bayes_overfitting)
  n_bayes_overfitting_sd <- sd(n_bayes_overfitting)

  n_bayes_table <- n_bayes_test_table + n_bayes_validation_table
  n_bayes_table_total <- n_bayes_table_total + n_bayes_table
  n_bayes_table_sum_diag <- sum(diag(n_bayes_table))

  n_bayes_true_positive_rate[i] <- sum(diag(n_bayes_table_total)) / sum(n_bayes_table_total)
  n_bayes_true_positive_rate_mean <- mean(n_bayes_true_positive_rate[i])
  n_bayes_true_negative_rate[i] <- 0.5 * (sum(diag(n_bayes_table_total))) / sum(n_bayes_table_total)
  n_bayes_true_negative_rate_mean <- mean(n_bayes_true_negative_rate)
  n_bayes_false_negative_rate[i] <- 1 - n_bayes_true_positive_rate[i]
  n_bayes_false_negative_rate_mean <- mean(n_bayes_false_negative_rate)
  n_bayes_false_positive_rate[i] <- 1 - n_bayes_true_negative_rate[i]
  n_bayes_false_positive_rate_mean <- mean(n_bayes_false_positive_rate)
  n_bayes_F1_score[i] <- 2 * n_bayes_true_positive_rate[i] / (2 * n_bayes_true_positive_rate[i] + n_bayes_false_positive_rate[i] + n_bayes_false_negative_rate[i])
  n_bayes_F1_score_mean <- mean(n_bayes_F1_score[i])

  n_bayes_end <- Sys.time()
  n_bayes_duration[i] <- n_bayes_end - n_bayes_start
  n_bayes_duration_mean <- mean(n_bayes_duration)
  n_bayes_duration_sd <- sd(n_bayes_duration)

  #### 13. Partial Least Squares ####
  pls_start <- Sys.time()
  print("Working on Partial Least Squares analysis")
  pls_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "PLSModel")
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
  pls_holdout_sd <- sd(pls_holdout)
  pls_overfitting[i] <- pls_holdout_mean / pls_train_accuracy_mean
  pls_overfitting_mean <- mean(pls_overfitting)
  pls_overfitting_range <- range(pls_overfitting)
  pls_overfitting_sd <- sd(pls_overfitting)

  pls_table <- pls_test_table + pls_validation_table
  pls_table_total <- pls_table_total + pls_table
  pls_table_sum_diag <- sum(diag(pls_table))

  pls_true_positive_rate[i] <- sum(diag(pls_table_total)) / sum(pls_table_total)
  pls_true_positive_rate_mean <- mean(pls_true_positive_rate[i])
  pls_true_negative_rate[i] <- 0.5 * (sum(diag(pls_table_total))) / sum(pls_table_total)
  pls_true_negative_rate_mean <- mean(pls_true_negative_rate)
  pls_false_negative_rate[i] <- 1 - pls_true_positive_rate[i]
  pls_false_negative_rate_mean <- mean(pls_false_negative_rate)
  pls_false_positive_rate[i] <- 1 - pls_true_negative_rate[i]
  pls_false_positive_rate_mean <- mean(pls_false_positive_rate)
  pls_F1_score[i] <- 2 * pls_true_positive_rate[i] / (2 * pls_true_positive_rate[i] + pls_false_positive_rate[i] + pls_false_negative_rate[i])
  pls_F1_score_mean <- mean(pls_F1_score[i])

  pls_end <- Sys.time()
  pls_duration[i] <- pls_end - pls_start
  pls_duration_mean <- mean(pls_duration)
  pls_duration_sd <- sd(pls_duration)

  #### 14. Penalized Discriminant Analysis Model ####
  pda_start <- Sys.time()
  print("Working on Penalized Discriminant analysis")
  pda_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "PDAModel")
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
  pda_holdout_sd <- sd(pda_holdout)
  pda_overfitting[i] <- pda_holdout_mean / pda_train_accuracy_mean
  pda_overfitting_mean <- mean(pda_overfitting)
  pda_overfitting_range <- range(pda_overfitting)
  pda_overfitting_sd <- sd(pda_overfitting)

  pda_table <- pda_test_table + pda_validation_table
  pda_table_total <- pda_table_total + pda_table
  pda_table_sum_diag <- sum(diag(pda_table))

  pda_true_positive_rate[i] <- sum(diag(pda_table_total)) / sum(pda_table_total)
  pda_true_positive_rate_mean <- mean(pda_true_positive_rate[i])
  pda_true_negative_rate[i] <- 0.5 * (sum(diag(pda_table_total))) / sum(pda_table_total)
  pda_true_negative_rate_mean <- mean(pda_true_negative_rate)
  pda_false_negative_rate[i] <- 1 - pda_true_positive_rate[i]
  pda_false_negative_rate_mean <- mean(pda_false_negative_rate)
  pda_false_positive_rate[i] <- 1 - pda_true_negative_rate[i]
  pda_false_positive_rate_mean <- mean(pda_false_positive_rate)
  pda_F1_score[i] <- 2 * pda_true_positive_rate[i] / (2 * pda_true_positive_rate[i] + pda_false_positive_rate[i] + pda_false_negative_rate[i])
  pda_F1_score_mean <- mean(pda_F1_score[i])

  pda_end <- Sys.time()
  pda_duration[i] <- pda_end - pda_start
  pda_duration_mean <- mean(pda_duration)
  pda_duration_sd <- sd(pda_duration)

  #### 15. Random Forest ####
  rf_start <- Sys.time()
  print("Working on Random Forest analysis")
  rf_train_fit <- randomForest::randomForest(x = train, y = y_train, data = df)
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
  rf_holdout_sd <- sd(rf_holdout)
  rf_overfitting[i] <- rf_holdout_mean / rf_train_accuracy_mean
  rf_overfitting_mean <- mean(rf_overfitting)
  rf_overfitting_range <- range(rf_overfitting)
  rf_overfitting_sd <- sd(rf_overfitting)

  rf_table <- rf_test_table + rf_validation_table
  rf_table_total <- rf_table_total + rf_table
  rf_table_sum_diag <- sum(diag(rf_table))

  rf_true_positive_rate[i] <- sum(diag(rf_table_total)) / sum(rf_table_total)
  rf_true_positive_rate_mean <- mean(rf_true_positive_rate[i])
  rf_true_negative_rate[i] <- 0.5 * (sum(diag(rf_table_total))) / sum(rf_table_total)
  rf_true_negative_rate_mean <- mean(rf_true_negative_rate)
  rf_false_negative_rate[i] <- 1 - rf_true_positive_rate[i]
  rf_false_negative_rate_mean <- mean(rf_false_negative_rate)
  rf_false_positive_rate[i] <- 1 - rf_true_negative_rate[i]
  rf_false_positive_rate_mean <- mean(rf_false_positive_rate)
  rf_F1_score[i] <- 2 * rf_true_positive_rate[i] / (2 * rf_true_positive_rate[i] + rf_false_positive_rate[i] + rf_false_negative_rate[i])
  rf_F1_score_mean <- mean(rf_F1_score[i])

  rf_end <- Sys.time()
  rf_duration[i] <- rf_end - rf_start
  rf_duration_mean <- mean(rf_duration)
  rf_duration_sd <- sd(rf_duration)

  #### 16. Ranger Model ####
  ranger_start <- Sys.time()
  print("Working on Ranger analysis")
  ranger_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "RangerModel")
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
  ranger_holdout_sd <- sd(ranger_holdout)
  ranger_overfitting[i] <- ranger_holdout_mean / ranger_train_accuracy_mean
  ranger_overfitting_mean <- mean(ranger_overfitting)
  ranger_overfitting_range <- range(ranger_overfitting)
  ranger_overfitting_sd <- sd(ranger_overfitting)

  ranger_table <- ranger_test_table + ranger_validation_table
  ranger_table_total <- ranger_table_total + ranger_table
  ranger_table_sum_diag <- sum(diag(ranger_table))

  ranger_true_positive_rate[i] <- sum(diag(ranger_table_total)) / sum(ranger_table_total)
  ranger_true_positive_rate_mean <- mean(ranger_true_positive_rate[i])
  ranger_true_negative_rate[i] <- 0.5 * (sum(diag(ranger_table_total))) / sum(ranger_table_total)
  ranger_true_negative_rate_mean <- mean(ranger_true_negative_rate)
  ranger_false_negative_rate[i] <- 1 - ranger_true_positive_rate[i]
  ranger_false_negative_rate_mean <- mean(ranger_false_negative_rate)
  ranger_false_positive_rate[i] <- 1 - ranger_true_negative_rate[i]
  ranger_false_positive_rate_mean <- mean(ranger_false_positive_rate)
  ranger_F1_score[i] <- 2 * ranger_true_positive_rate[i] / (2 * ranger_true_positive_rate[i] + ranger_false_positive_rate[i] + ranger_false_negative_rate[i])
  ranger_F1_score_mean <- mean(ranger_F1_score[i])

  ranger_end <- Sys.time()
  ranger_duration[i] <- ranger_end - ranger_start
  ranger_duration_mean <- mean(ranger_duration)
  ranger_duration_sd <- sd(ranger_duration)

  #### 18. RPart Model ####
  rpart_start <- Sys.time()
  print("Working on RPart analysis")
  rpart_train_fit <- MachineShop::fit(y ~ ., data = train01, model = "RPartModel")
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
  rpart_holdout_sd <- sd(rpart_holdout)
  rpart_overfitting[i] <- rpart_holdout_mean / rpart_train_accuracy_mean
  rpart_overfitting_mean <- mean(rpart_overfitting)
  rpart_overfitting_range <- range(rpart_overfitting)
  rpart_overfitting_sd <- sd(rpart_overfitting)

  rpart_table <- rpart_test_table + rpart_validation_table
  rpart_table_total <- rpart_table_total + rpart_table
  rpart_table_sum_diag <- sum(diag(rpart_table))

  rpart_true_positive_rate[i] <- sum(diag(rpart_table_total)) / sum(rpart_table_total)
  rpart_true_positive_rate_mean <- mean(rpart_true_positive_rate[i])
  rpart_true_negative_rate[i] <- 0.5 * (sum(diag(rpart_table_total))) / sum(rpart_table_total)
  rpart_true_negative_rate_mean <- mean(rpart_true_negative_rate)
  rpart_false_negative_rate[i] <- 1 - rpart_true_positive_rate[i]
  rpart_false_negative_rate_mean <- mean(rpart_false_negative_rate)
  rpart_false_positive_rate[i] <- 1 - rpart_true_negative_rate[i]
  rpart_false_positive_rate_mean <- mean(rpart_false_positive_rate)
  rpart_F1_score[i] <- 2 * rpart_true_positive_rate[i] / (2 * rpart_true_positive_rate[i] + rpart_false_positive_rate[i] + rpart_false_negative_rate[i])
  rpart_F1_score_mean <- mean(rpart_F1_score[i])

  rpart_end <- Sys.time()
  rpart_duration[i] <- rpart_end - rpart_start
  rpart_duration_mean <- mean(rpart_duration)
  rpart_duration_sd <- sd(rpart_duration)


  #### 19. Support Vector Machines ####
  svm_start <- Sys.time()
  print("Working on Support Vector Machine analysis")
  svm_train_fit <- e1071::svm(y_train ~ ., data = train, kernel = "radial", gamma = 1, cost = 1)
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
  svm_holdout_sd <- sd(svm_holdout)
  svm_overfitting[i] <- svm_holdout_mean / svm_train_accuracy_mean
  svm_overfitting_mean <- mean(svm_overfitting)
  svm_overfitting_range <- range(svm_overfitting)
  svm_overfitting_sd <- sd(svm_overfitting)

  svm_table <- svm_test_table + svm_validation_table
  svm_table_total <- svm_table_total + svm_table
  svm_table_sum_diag <- sum(diag(svm_table))

  svm_true_positive_rate[i] <- sum(diag(svm_table_total)) / sum(svm_table_total)
  svm_true_positive_rate_mean <- mean(svm_true_positive_rate[i])
  svm_true_negative_rate[i] <- 0.5 * (sum(diag(svm_table_total))) / sum(svm_table_total)
  svm_true_negative_rate_mean <- mean(svm_true_negative_rate)
  svm_false_negative_rate[i] <- 1 - svm_true_positive_rate[i]
  svm_false_negative_rate_mean <- mean(svm_false_negative_rate)
  svm_false_positive_rate[i] <- 1 - svm_true_negative_rate[i]
  svm_false_positive_rate_mean <- mean(svm_false_positive_rate)
  svm_F1_score[i] <- 2 * svm_true_positive_rate[i] / (2 * svm_true_positive_rate[i] + svm_false_positive_rate[i] + svm_false_negative_rate[i])
  svm_F1_score_mean <- mean(svm_F1_score[i])

  svm_end <- Sys.time()
  svm_duration[i] <- svm_end - svm_start
  svm_duration_mean <- mean(svm_duration)
  svm_duration_sd <- sd(svm_duration)


  #### 20. Trees ####
  tree_start <- Sys.time()
  print("Working on Trees analysis")
  tree_train_fit <- tree::tree(y_train ~ ., data = train)
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
  tree_holdout_sd <- sd(tree_holdout)
  tree_overfitting[i] <- tree_holdout_mean / tree_train_accuracy_mean
  tree_overfitting_mean <- mean(tree_overfitting)
  tree_overfitting_range <- range(tree_overfitting)
  tree_overfitting_sd <- sd(tree_overfitting)

  tree_table <- tree_test_table + tree_validation_table
  tree_table_total <- tree_table_total + tree_table
  tree_table_sum_diag <- sum(diag(tree_table))

  tree_true_positive_rate[i] <- sum(diag(tree_table_total)) / sum(tree_table_total)
  tree_true_positive_rate_mean <- mean(tree_true_positive_rate[i])
  tree_true_negative_rate[i] <- 0.5 * (sum(diag(tree_table_total))) / sum(tree_table_total)
  tree_true_negative_rate_mean <- mean(tree_true_negative_rate)
  tree_false_negative_rate[i] <- 1 - tree_true_positive_rate[i]
  tree_false_negative_rate_mean <- mean(tree_false_negative_rate)
  tree_false_positive_rate[i] <- 1 - tree_true_negative_rate[i]
  tree_false_positive_rate_mean <- mean(tree_false_positive_rate)
  tree_F1_score[i] <- 2 * tree_true_positive_rate[i] / (2 * tree_true_positive_rate[i] + tree_false_positive_rate[i] + tree_false_negative_rate[i])
  tree_F1_score_mean <- mean(tree_F1_score[i])

  tree_end <- Sys.time()
  tree_duration[i] <- tree_end - tree_start
  tree_duration_mean <- mean(tree_duration)
  tree_duration_sd <- sd(tree_duration)



  #### Ensembles ####
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

  #### 20. Ensemble Bagged CART ####
  ensemble_bag_cart_start <- Sys.time()
  print("Working on Ensemble Bagged CART analysis")
  ensemble_bag_cart_train_fit <- ipred::bagging(y ~ ., data = ensemble_train)
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
  ensemble_bag_cart_holdout_sd <- sd(ensemble_bag_cart_holdout)
  ensemble_bag_cart_overfitting[i] <- ensemble_bag_cart_holdout_mean / ensemble_bag_cart_train_accuracy_mean
  ensemble_bag_cart_overfitting_mean <- mean(ensemble_bag_cart_overfitting)
  ensemble_bag_cart_overfitting_range <- range(ensemble_bag_cart_overfitting)
  ensemble_bag_cart_overfitting_sd <- sd(ensemble_bag_cart_overfitting)

  ensemble_bag_cart_table <- ensemble_bag_cart_test_table + ensemble_bag_cart_validation_table
  ensemble_bag_cart_table_total <- ensemble_bag_cart_table_total + ensemble_bag_cart_table
  ensemble_bag_cart_table_sum_diag <- sum(diag(ensemble_bag_cart_table))

  ensemble_bag_cart_true_positive_rate[i] <- sum(diag(ensemble_bag_cart_table_total)) / sum(ensemble_bag_cart_table_total)
  ensemble_bag_cart_true_positive_rate_mean <- mean(ensemble_bag_cart_true_positive_rate[i])
  ensemble_bag_cart_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_bag_cart_table_total))) / sum(ensemble_bag_cart_table_total)
  ensemble_bag_cart_true_negative_rate_mean <- mean(ensemble_bag_cart_true_negative_rate)
  ensemble_bag_cart_false_negative_rate[i] <- 1 - ensemble_bag_cart_true_positive_rate[i]
  ensemble_bag_cart_false_negative_rate_mean <- mean(ensemble_bag_cart_false_negative_rate)
  ensemble_bag_cart_false_positive_rate[i] <- 1 - ensemble_bag_cart_true_negative_rate[i]
  ensemble_bag_cart_false_positive_rate_mean <- mean(ensemble_bag_cart_false_positive_rate)
  ensemble_bag_cart_F1_score[i] <- 2 * ensemble_bag_cart_true_positive_rate[i] / (2 * ensemble_bag_cart_true_positive_rate[i] + ensemble_bag_cart_false_positive_rate[i] + ensemble_bag_cart_false_negative_rate[i])
  ensemble_bag_cart_F1_score_mean <- mean(ensemble_bag_cart_F1_score[i])

  ensemble_bag_cart_end <- Sys.time()
  ensemble_bag_cart_duration[i] <- ensemble_bag_cart_end - ensemble_bag_cart_start
  ensemble_bag_cart_duration_mean <- mean(ensemble_bag_cart_duration)
  ensemble_bag_cart_duration_sd <- sd(ensemble_bag_cart_duration)

  #### 21. Ensemble Bagged Random Forest ####
  ensemble_bag_rf_start <- Sys.time()
  print("Working on Ensemble Bagged Random Forest analysis")
  ensemble_bag_train_rf <- randomForest::randomForest(ensemble_y_train ~ ., data = ensemble_train, mtry = ncol(ensemble_train) - 1)
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
  ensemble_bag_rf_holdout_sd <- sd(ensemble_bag_rf_holdout)
  ensemble_bag_rf_overfitting[i] <- ensemble_bag_rf_holdout_mean / ensemble_bag_rf_train_accuracy_mean
  ensemble_bag_rf_overfitting_mean <- mean(ensemble_bag_rf_overfitting)
  ensemble_bag_rf_overfitting_range <- range(ensemble_bag_rf_overfitting)
  ensemble_bag_rf_overfitting_sd <- sd(ensemble_bag_rf_overfitting)

  ensemble_bag_rf_table <- ensemble_bag_rf_test_table + ensemble_bag_rf_validation_table
  ensemble_bag_rf_table_total <- ensemble_bag_rf_table_total + ensemble_bag_rf_table
  ensemble_bag_rf_table_sum_diag <- sum(diag(ensemble_bag_rf_table))

  ensemble_bag_rf_true_positive_rate[i] <- sum(diag(ensemble_bag_rf_table_total)) / sum(ensemble_bag_rf_table_total)
  ensemble_bag_rf_true_positive_rate_mean <- mean(ensemble_bag_rf_true_positive_rate[i])
  ensemble_bag_rf_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_bag_rf_table_total))) / sum(ensemble_bag_rf_table_total)
  ensemble_bag_rf_true_negative_rate_mean <- mean(ensemble_bag_rf_true_negative_rate)
  ensemble_bag_rf_false_negative_rate[i] <- 1 - ensemble_bag_rf_true_positive_rate[i]
  ensemble_bag_rf_false_negative_rate_mean <- mean(ensemble_bag_rf_false_negative_rate)
  ensemble_bag_rf_false_positive_rate[i] <- 1 - ensemble_bag_rf_true_negative_rate[i]
  ensemble_bag_rf_false_positive_rate_mean <- mean(ensemble_bag_rf_false_positive_rate)
  ensemble_bag_rf_F1_score[i] <- 2 * ensemble_bag_rf_true_positive_rate[i] / (2 * ensemble_bag_rf_true_positive_rate[i] + ensemble_bag_rf_false_positive_rate[i] + ensemble_bag_rf_false_negative_rate[i])
  ensemble_bag_rf_F1_score_mean <- mean(ensemble_bag_rf_F1_score[i])

  ensemble_bag_rf_end <- Sys.time()
  ensemble_bag_rf_duration[i] <- ensemble_bag_rf_end - ensemble_bag_rf_start
  ensemble_bag_rf_duration_mean <- mean(ensemble_bag_rf_duration)
  ensemble_bag_rf_duration_sd <- sd(ensemble_bag_rf_duration)

  #### 22. Ensemble C50 ####
  ensemble_C50_start <- Sys.time()
  print("Working on Ensemble C50 analysis")
  ensemble_C50_train_fit <- C50::C5.0(ensemble_y_train ~ ., data = ensemble_train)
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
  ensemble_C50_holdout_sd <- sd(ensemble_C50_holdout)
  ensemble_C50_overfitting[i] <- ensemble_C50_holdout_mean / ensemble_C50_train_accuracy_mean
  ensemble_C50_overfitting_mean <- mean(ensemble_C50_overfitting)
  ensemble_C50_overfitting_range <- range(ensemble_C50_overfitting)
  ensemble_C50_overfitting_sd <- sd(ensemble_C50_overfitting)

  ensemble_C50_table <- ensemble_C50_test_table + ensemble_C50_validation_table
  ensemble_C50_table_total <- ensemble_C50_table_total + ensemble_C50_table
  ensemble_C50_table_sum_diag <- sum(diag(ensemble_C50_table))

  ensemble_C50_true_positive_rate[i] <- sum(diag(ensemble_C50_table_total)) / sum(ensemble_C50_table_total)
  ensemble_C50_true_positive_rate_mean <- mean(ensemble_C50_true_positive_rate[i])
  ensemble_C50_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_C50_table_total))) / sum(ensemble_C50_table_total)
  ensemble_C50_true_negative_rate_mean <- mean(ensemble_C50_true_negative_rate)
  ensemble_C50_false_negative_rate[i] <- 1 - ensemble_C50_true_positive_rate[i]
  ensemble_C50_false_negative_rate_mean <- mean(ensemble_C50_false_negative_rate)
  ensemble_C50_false_positive_rate[i] <- 1 - ensemble_C50_true_negative_rate[i]
  ensemble_C50_false_positive_rate_mean <- mean(ensemble_C50_false_positive_rate)
  ensemble_C50_F1_score[i] <- 2 * ensemble_C50_true_positive_rate[i] / (2 * ensemble_C50_true_positive_rate[i] + ensemble_C50_false_positive_rate[i] + ensemble_C50_false_negative_rate[i])
  ensemble_C50_F1_score_mean <- mean(ensemble_C50_F1_score[i])

  ensemble_C50_end <- Sys.time()
  ensemble_C50_duration[i] <- ensemble_C50_end - ensemble_C50_start
  ensemble_C50_duration_mean <- mean(ensemble_C50_duration)
  ensemble_C50_duration_sd <- sd(ensemble_C50_duration)


  #### 25. Ensemble Naive Bayes ####
  ensemble_n_bayes_start <- Sys.time()
  print("Working on Ensembles using Naive Bayes analysis")
  ensemble_n_bayes_train_fit <- e1071::naiveBayes(ensemble_y_train ~ ., data = ensemble_train)
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
  ensemble_n_bayes_holdout_sd <- sd(ensemble_n_bayes_holdout)
  ensemble_n_bayes_overfitting[i] <- ensemble_n_bayes_holdout_mean / ensemble_n_bayes_train_accuracy_mean
  ensemble_n_bayes_overfitting_mean <- mean(ensemble_n_bayes_overfitting)
  ensemble_n_bayes_overfitting_range <- range(ensemble_n_bayes_overfitting)
  ensemble_n_bayes_overfitting_sd <- sd(ensemble_n_bayes_overfitting)

  ensemble_n_bayes_table <- ensemble_n_bayes_test_table + ensemble_n_bayes_validation_table
  ensemble_n_bayes_table_total <- ensemble_n_bayes_table_total + ensemble_n_bayes_table
  ensemble_n_bayes_table_sum_diag <- sum(diag(ensemble_n_bayes_table))

  ensemble_n_bayes_true_positive_rate[i] <- sum(diag(ensemble_n_bayes_table_total)) / sum(ensemble_n_bayes_table_total)
  ensemble_n_bayes_true_positive_rate_mean <- mean(ensemble_n_bayes_true_positive_rate[i])
  ensemble_n_bayes_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_n_bayes_table_total))) / sum(ensemble_n_bayes_table_total)
  ensemble_n_bayes_true_negative_rate_mean <- mean(ensemble_n_bayes_true_negative_rate)
  ensemble_n_bayes_false_negative_rate[i] <- 1 - ensemble_n_bayes_true_positive_rate[i]
  ensemble_n_bayes_false_negative_rate_mean <- mean(ensemble_n_bayes_false_negative_rate)
  ensemble_n_bayes_false_positive_rate[i] <- 1 - ensemble_n_bayes_true_negative_rate[i]
  ensemble_n_bayes_false_positive_rate_mean <- mean(ensemble_n_bayes_false_positive_rate)
  ensemble_n_bayes_F1_score[i] <- 2 * ensemble_n_bayes_true_positive_rate[i] / (2 * ensemble_n_bayes_true_positive_rate[i] + ensemble_n_bayes_false_positive_rate[i] + ensemble_n_bayes_false_negative_rate[i])
  ensemble_n_bayes_F1_score_mean <- mean(ensemble_n_bayes_F1_score[i])

  ensemble_n_bayes_end <- Sys.time()
  ensemble_n_bayes_duration[i] <- ensemble_n_bayes_end - ensemble_n_bayes_start
  ensemble_n_bayes_duration_mean <- mean(ensemble_n_bayes_duration)
  ensemble_n_bayes_duration_sd <- sd(ensemble_n_bayes_duration)


  #### 26. Ensemble Ranger Model #####
  ensemble_ranger_start <- Sys.time()
  print("Working on Ensembles using Ranger analysis")
  ensemble_ranger_train_fit <- MachineShop::fit(y ~ ., data = ensemble_train, model = "RangerModel")
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
  ensemble_ranger_holdout_sd <- sd(ensemble_ranger_holdout)
  ensemble_ranger_overfitting[i] <- ensemble_ranger_holdout_mean / ensemble_ranger_train_accuracy_mean
  ensemble_ranger_overfitting_mean <- mean(ensemble_ranger_overfitting)
  ensemble_ranger_overfitting_range <- range(ensemble_ranger_overfitting)
  ensemble_ranger_overfitting_sd <- sd(ensemble_ranger_overfitting)

  ensemble_ranger_table <- ensemble_ranger_test_table + ensemble_ranger_validation_table
  ensemble_ranger_table_total <- ensemble_ranger_table_total + ensemble_ranger_table
  ensemble_ranger_table_sum_diag <- sum(diag(ensemble_ranger_table))

  ensemble_ranger_true_positive_rate[i] <- sum(diag(ensemble_ranger_table_total)) / sum(ensemble_ranger_table_total)
  ensemble_ranger_true_positive_rate_mean <- mean(ensemble_ranger_true_positive_rate[i])
  ensemble_ranger_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_ranger_table_total))) / sum(ensemble_ranger_table_total)
  ensemble_ranger_true_negative_rate_mean <- mean(ensemble_ranger_true_negative_rate)
  ensemble_ranger_false_negative_rate[i] <- 1 - ensemble_ranger_true_positive_rate[i]
  ensemble_ranger_false_negative_rate_mean <- mean(ensemble_ranger_false_negative_rate)
  ensemble_ranger_false_positive_rate[i] <- 1 - ensemble_ranger_true_negative_rate[i]
  ensemble_ranger_false_positive_rate_mean <- mean(ensemble_ranger_false_positive_rate)
  ensemble_ranger_F1_score[i] <- 2 * ensemble_ranger_true_positive_rate[i] / (2 * ensemble_ranger_true_positive_rate[i] + ensemble_ranger_false_positive_rate[i] + ensemble_ranger_false_negative_rate[i])
  ensemble_ranger_F1_score_mean <- mean(ensemble_ranger_F1_score[i])

  ensemble_ranger_end <- Sys.time()
  ensemble_ranger_duration[i] <- ensemble_ranger_end - ensemble_ranger_start
  ensemble_ranger_duration_mean <- mean(ensemble_ranger_duration)
  ensemble_ranger_duration_sd <- sd(ensemble_ranger_duration)

  #### 27. Ensemble Random Forest ####
  ensemble_rf_start <- Sys.time()
  print("Working on Ensembles using Random Forest analysis")
  ensemble_train_rf_fit <- randomForest::randomForest(x = ensemble_train, y = ensemble_y_train)
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
  ensemble_rf_holdout_sd <- sd(ensemble_rf_holdout)
  ensemble_rf_overfitting[i] <- ensemble_rf_holdout_mean / ensemble_rf_train_accuracy_mean
  ensemble_rf_overfitting_mean <- mean(ensemble_rf_overfitting)
  ensemble_rf_overfitting_range <- range(ensemble_rf_overfitting)
  ensemble_rf_overfitting_sd <- sd(ensemble_rf_overfitting)

  ensemble_rf_table <- ensemble_rf_test_table + ensemble_rf_validation_table
  ensemble_rf_table_total <- ensemble_rf_table_total + ensemble_rf_table
  ensemble_rf_table_sum_diag <- sum(diag(ensemble_rf_table))

  ensemble_rf_true_positive_rate[i] <- sum(diag(ensemble_rf_table_total)) / sum(ensemble_rf_table_total)
  ensemble_rf_true_positive_rate_mean <- mean(ensemble_rf_true_positive_rate[i])
  ensemble_rf_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_rf_table_total))) / sum(ensemble_rf_table_total)
  ensemble_rf_true_negative_rate_mean <- mean(ensemble_rf_true_negative_rate)
  ensemble_rf_false_negative_rate[i] <- 1 - ensemble_rf_true_positive_rate[i]
  ensemble_rf_false_negative_rate_mean <- mean(ensemble_rf_false_negative_rate)
  ensemble_rf_false_positive_rate[i] <- 1 - ensemble_rf_true_negative_rate[i]
  ensemble_rf_false_positive_rate_mean <- mean(ensemble_rf_false_positive_rate)
  ensemble_rf_F1_score[i] <- 2 * ensemble_rf_true_positive_rate[i] / (2 * ensemble_rf_true_positive_rate[i] + ensemble_rf_false_positive_rate[i] + ensemble_rf_false_negative_rate[i])
  ensemble_rf_F1_score_mean <- mean(ensemble_rf_F1_score[i])

  ensemble_rf_end <- Sys.time()
  ensemble_rf_duration[i] <- ensemble_rf_end - ensemble_rf_start
  ensemble_rf_duration_mean <- mean(ensemble_rf_duration)
  ensemble_rf_duration_sd <- sd(ensemble_rf_duration)

  #### 29. Ensemble Support Vector Machines ####
  ensemble_svm_start <- Sys.time()
  print("Working on Ensembles using Support Vector Machines analysis")
  ensemble_svm_train_fit <- e1071::svm(ensemble_y_train ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)
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
  ensemble_svm_holdout_sd <- sd(ensemble_svm_holdout)
  ensemble_svm_overfitting[i] <- ensemble_svm_holdout_mean / ensemble_svm_train_accuracy_mean
  ensemble_svm_overfitting_mean <- mean(ensemble_svm_overfitting)
  ensemble_svm_overfitting_range <- range(ensemble_svm_overfitting)
  ensemble_svm_overfitting_sd <- sd(ensemble_svm_overfitting)

  ensemble_svm_table <- ensemble_svm_test_table + ensemble_svm_validation_table
  ensemble_svm_table_total <- ensemble_svm_table_total + ensemble_svm_table
  ensemble_svm_table_sum_diag <- sum(diag(ensemble_svm_table))

  ensemble_svm_true_positive_rate[i] <- sum(diag(ensemble_svm_table_total)) / sum(ensemble_svm_table_total)
  ensemble_svm_true_positive_rate_mean <- mean(ensemble_svm_true_positive_rate[i])
  ensemble_svm_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_svm_table_total))) / sum(ensemble_svm_table_total)
  ensemble_svm_true_negative_rate_mean <- mean(ensemble_svm_true_negative_rate)
  ensemble_svm_false_negative_rate[i] <- 1 - ensemble_svm_true_positive_rate[i]
  ensemble_svm_false_negative_rate_mean <- mean(ensemble_svm_false_negative_rate)
  ensemble_svm_false_positive_rate[i] <- 1 - ensemble_svm_true_negative_rate[i]
  ensemble_svm_false_positive_rate_mean <- mean(ensemble_svm_false_positive_rate)
  ensemble_svm_F1_score[i] <- 2 * ensemble_svm_true_positive_rate[i] / (2 * ensemble_svm_true_positive_rate[i] + ensemble_svm_false_positive_rate[i] + ensemble_svm_false_negative_rate[i])
  ensemble_svm_F1_score_mean <- mean(ensemble_svm_F1_score[i])

  ensemble_svm_end <- Sys.time()
  ensemble_svm_duration[i] <- ensemble_svm_end - ensemble_svm_start
  ensemble_svm_duration_mean <- mean(ensemble_svm_duration)
  ensemble_svm_duration_sd <- sd(ensemble_svm_duration)

  #### 30. Ensemble Trees ####
  ensemble_tree_start <- Sys.time()
  print("Working on Ensembles using Trees analysis")
  ensemble_tree_train_fit <- tree::tree(y ~ ., data = ensemble_train)
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
  ensemble_tree_holdout_sd <- sd(ensemble_tree_holdout)
  ensemble_tree_overfitting[i] <- ensemble_tree_holdout_mean / ensemble_tree_train_accuracy_mean
  ensemble_tree_overfitting_mean <- mean(ensemble_tree_overfitting)
  ensemble_tree_overfitting_range <- range(ensemble_tree_overfitting)
  ensemble_tree_overfitting_sd <- sd(ensemble_tree_overfitting)

  ensemble_tree_table <- ensemble_tree_test_table + ensemble_tree_validation_table
  ensemble_tree_table_total <- ensemble_tree_table_total + ensemble_tree_table
  ensemble_tree_table_sum_diag <- sum(diag(ensemble_tree_table))

  ensemble_tree_true_positive_rate[i] <- sum(diag(ensemble_tree_table_total)) / sum(ensemble_tree_table_total)
  ensemble_tree_true_positive_rate_mean <- mean(ensemble_tree_true_positive_rate[i])
  ensemble_tree_true_negative_rate[i] <- 0.5 * (sum(diag(ensemble_tree_table_total))) / sum(ensemble_tree_table_total)
  ensemble_tree_true_negative_rate_mean <- mean(ensemble_tree_true_negative_rate)
  ensemble_tree_false_negative_rate[i] <- 1 - ensemble_tree_true_positive_rate[i]
  ensemble_tree_false_negative_rate_mean <- mean(ensemble_tree_false_negative_rate)
  ensemble_tree_false_positive_rate[i] <- 1 - ensemble_tree_true_negative_rate[i]
  ensemble_tree_false_positive_rate_mean <- mean(ensemble_tree_false_positive_rate)
  ensemble_tree_F1_score[i] <- 2 * ensemble_tree_true_positive_rate[i] / (2 * ensemble_tree_true_positive_rate[i] + ensemble_tree_false_positive_rate[i] + ensemble_tree_false_negative_rate[i])
  ensemble_tree_F1_score_mean <- mean(ensemble_tree_F1_score[i])

  ensemble_tree_end <- Sys.time()
  ensemble_tree_duration[i] <- ensemble_tree_end - ensemble_tree_start
  ensemble_tree_duration_mean <- mean(ensemble_tree_duration)
  ensemble_tree_duration_sd <- sd(ensemble_tree_duration)
}

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
  "Overfitting" = round(c(
    bagging_overfitting_mean, bag_rf_overfitting_mean,
    C50_overfitting_mean, linear_overfitting_mean,
    n_bayes_overfitting_mean, pls_overfitting_mean, pda_overfitting_mean, rf_overfitting_mean, ranger_overfitting_mean,
    rpart_overfitting_mean, svm_overfitting_mean, tree_overfitting_mean,
    ensemble_bag_cart_overfitting_mean, ensemble_bag_rf_overfitting_mean, ensemble_C50_overfitting_mean,
    ensemble_n_bayes_overfitting_mean, ensemble_ranger_overfitting_mean, ensemble_rf_overfitting_mean,
    ensemble_svm_overfitting_mean, ensemble_tree_overfitting_mean
  ), 4),
  "Overfitting_St_Dev" = round(c(
    bagging_overfitting_sd, bag_rf_overfitting_sd,
    C50_overfitting_sd, linear_overfitting_sd,
    n_bayes_overfitting_sd, pls_overfitting_sd, pda_overfitting_sd, rf_overfitting_sd, ranger_overfitting_sd,
    rpart_overfitting_sd, svm_overfitting_sd, tree_overfitting_sd,
    ensemble_bag_cart_overfitting_sd, ensemble_bag_rf_overfitting_sd, ensemble_C50_overfitting_sd,
    ensemble_n_bayes_overfitting_sd, ensemble_ranger_overfitting_sd, ensemble_rf_overfitting_sd,
    ensemble_svm_overfitting_sd, ensemble_tree_overfitting_sd
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
  reactablefmtr::add_title("Classification analysis, accuracy, duration, overfitting, sum of diagonals")

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

accuracy_plot <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5) +
  ggplot2::ggtitle("Accuracy by model, higher is better, 1 is best. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Accuracy by model, higher is better, 1 is best. \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")


overfitting_data <- data.frame(
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
    bagging_overfitting, bag_rf_overfitting, C50_overfitting, linear_overfitting,
    n_bayes_overfitting, pls_overfitting, pda_overfitting, rf_overfitting, ranger_overfitting, rpart_overfitting, svm_overfitting, tree_overfitting,
    ensemble_bag_cart_overfitting, ensemble_bag_rf_overfitting, ensemble_C50_overfitting,
    ensemble_n_bayes_overfitting,
    ensemble_ranger_overfitting, ensemble_rf_overfitting, ensemble_svm_overfitting, ensemble_tree_overfitting
  ),
  "mean" = rep(c(
    bagging_overfitting_mean, bag_rf_overfitting_mean, C50_overfitting_mean,
    linear_overfitting_mean, n_bayes_overfitting_mean, pls_overfitting_mean,
    pda_overfitting_mean, rf_overfitting_mean, ranger_overfitting_mean, rpart_overfitting_mean, svm_overfitting_mean, tree_overfitting_mean,
    ensemble_bag_cart_overfitting_mean, ensemble_bag_rf_overfitting_mean, ensemble_C50_overfitting_mean,
    ensemble_n_bayes_overfitting_mean, ensemble_ranger_overfitting_mean, ensemble_rf_overfitting_mean, ensemble_svm_overfitting_mean,
    ensemble_tree_overfitting_mean
  ), each = numresamples)
)

overfitting_plot <- ggplot2::ggplot(data = overfitting_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = count, y = data)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 5) +
  ggplot2::ggtitle("Overfitting by model, closer to one is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Overfitting by model, closer to one is better. \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")


####################################################

###### Total Data visualizations start here ########

####################################################


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
  ggplot2::ggtitle("Accuracy data including train and holdout by model, fixed scales. \nRoot Mean Squared Error by model, higher is better, 1 is best.") +
  ggplot2::labs(y = "Higher is better, 1 is best.") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("train", "holdout"),
    values = c("train" = "red", "holdout" = "black")
  )

total_plot2 <- ggplot2::ggplot(data = total_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::facet_wrap(~model, ncol = 5, scales = "free") +
  ggplot2::ggtitle("Accuracy data including train and holdout by model, free scales. \nRoot Mean Squared Error by model, higher is better, 1 is best.") +
  ggplot2::labs(y = "Higher is better, 1 is best.") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("train", "holdout"),
    values = c("train" = "red", "holdout" = "black")
  )

accuracy_barchart <- ggplot2::ggplot(Results, aes(x = reorder(Model, dplyr::desc(Mean_Holdout_Accuracy)), y = Mean_Holdout_Accuracy)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Mean Accuracy", title = "Model accuracy, closer to one is better") +
  ggplot2::geom_text(aes(label = Mean_Holdout_Accuracy), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(Results$Mean_Holdout_Accuracy) + 1) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Mean_Holdout_Accuracy - Accuracy_Holdout_Std.Dev,
                             ymax =  Mean_Holdout_Accuracy + Accuracy_Holdout_Std.Dev))

overfitting_barchart <- ggplot2::ggplot(Results, aes(x = reorder(Model, Overfitting), y = Overfitting)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Over or Under Fitting Mean", title = "Over or Under Fitting, closer to 1 is better") +
  ggplot2::geom_text(aes(label = Overfitting), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(Results$Overfitting) +2) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Overfitting - Overfitting_St_Dev, ymax = Overfitting + Overfitting_St_Dev))

duration_barchart <- ggplot2::ggplot(Results, aes(x = reorder(Model, Duration), y = Duration)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Duration", title = "Duration, shorter is better") +
  ggplot2::geom_text(aes(label = Duration), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(Results$Duration) + 2) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Duration - Duration_St_Dev, ymax = Duration + Duration_St_Dev))


if (do_you_have_new_data == "Y") {
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
    "Final_Results" = Final_results, "Barcharts" = barchart, "Accuracy_Barchart" = accuracy_barchart, "Overfitting_barchart" = overfitting_barchart,
    "Duration_barchart" = duration_barchart, "Data summary" = data_summary, "Correlation_Matrix" = correlation_marix, "Boxplots" = boxplots, "Histograms" = histograms, "Head of data" = head_data, "Head of Ensembles" = head_ensemble,
    "Summary_Tables" = summary_tables, "Accuracy_Plot" = accuracy_plot, "Total_Plot" = total_plot, "Overfitting_plot" = overfitting_plot, "New_Data_Results" = New_Data_Results
  ))

}

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

list(
  'Final_results' = Final_results, 'Barchart' = barchart, "Accuracy_Barchart" = accuracy_barchart, "Overfitting_barchart" = overfitting_barchart,
  "Duration_barchart" = duration_barchart,  'Data_summary' = data_summary, 'Correlation_matrix' = correlation_marix,
  'Boxplots' = boxplots, 'Histograms' = histograms, 'Head_of_data' = head_data, 'Head_of_ensemble' = head_ensemble,
  'Summary_tables' = summary_tables, 'Accuracy_plot' = accuracy_plot, 'Total_plot' = total_plot, "Total_plot2" = total_plot2,
  "Overfitting_plot" = overfitting_plot
)
}
