
# ClassificationEnsembles

<!-- badges: start -->
<!-- badges: end -->

The goal of ClassificationEnsembles is to automatically conduct a thorough analysis of data that includes classification data. The user only needs to provide the data and answer a few questions (such as which column to analyze). ClassificationEnsembles fits 25 models (15 individual models and 10 ensembles of models). The package also returns 13 plots, five tables and a summary report sorted by accuracy (highest to lowest)

## Installation

You can install the development version of ClassificationEnsembles like so:

``` r
devtools::install_github("InfiniteCuriosity/ClassificationEnsembles")
```

## Example

ClassificationEnsembles will model the location of a car seat (Good, Medium or Bad) based on the other features in the Carseats data set



``` r
library(ClassificationEnsembles)
Classification(data = Carseats,
  colnum = 7,
  numresamples = 2,
  do_you_have_new_data = "N",
  how_to_handle_strings = 1,
  save_all_trained_models = "N",
  use_parallel = "N",
  train_amount = 0.60,
  test_amount = 0.20,
  validation_amount = 0.20)

```

The 25 models which are build automatically are:

1. ADABag
2. Bagged Random Forest
3. Bagging
4. C50
5. Ensemble ADABag
6. Ensemble BaggedCart
7. Ensemble Bagged Random Forest
8. Ensemble C50
9. Ensemble NaiveBayes
10. Ensemble Random Forest
12. Ensemble Ranger
12. Ensemble Regularized Discrmininant Analysis
13. Ensemble Support Vector Machines
14. Ensemble Trees
15. Linear
16. Naive Bayes
17. Partial Least Squares
18. Penalized Discrmininant Analysis
19. Random Forest
20. Ranger
21. Regularized Discrmininant Analysis
22. RPart
23. Support Vector Machines
24. Trees
25. XGBoost

The 12 plots it returns automatically are:<br>
1. Overfitting by model and resample<br>
2. Accuracy by model, resample and train/holdout values<br>
3. Accuracy by model and resample<br>
4. Histogram of numeric data<br>
5. Boxplots of numeric data<br>
6. Duration barchart<br>
7. Over or underfitting barchart<br>
8. Model accuracy barchart<br>
9. Target (ShelveLoc in the demo) vs each feature in the data<br>
10. Pairwise scatterplots<br>
11. Correlation of the numeric data as circles and colors<br>
12. Correlation of the numeric data as numbers and colors<br<
<br><br>
The 5 tables the package returns automatically are:<br>
1. Head of the ensemble<br>
2. Head of the data frame<br>
3. Correlation of the data<br>
4. Data summary<br>
5. Summary report, including accuracy, duration, overfitting, sum of diagonals<br>
<br>
The package also returns 25 summary tables, one for each of the models. These can be found in the Console.
