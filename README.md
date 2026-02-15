
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
Classification(data = Cleveland_heart,
               colnum = 12,
               numresamples = 25,
               predict_on_new_data = "N",
               save_all_plots = "N",
               how_to_handle_strings = 1,
               stratified_random_column = 3,
               remove_VIF_above <- 5.00,
               set_seed = "N",
               save_all_trained_models = "N",
               scale_all_numeric_predictors_in_data = "N",
               use_parallel = "N",
               train_amount = 0.50,
               test_amount = 0.25,
               validation_amount = 0.25)
```

The 20 models which are automatically built are:

1. Bagged Random Forest
2. Bagging
3. C50
4. Ensemble BaggedCart
5. Ensemble Bagged Random Forest
6. Ensemble C50
7. Ensemble NaiveBayes
8. Ensemble Support Vector Machines
9. Ensemble Ranger
10.Ensemble Support Vector Machines
11. Ensemble Trees
12. Linear
13. Naive Bayes
14. Partial Least Squares
15. Penalized Discriminant Analysis
16. Random Forest
17. Ranger
18. RPart
19. Support Vector Machines
20. Trees


The 26 plots it returns automatically are:<br>
1. Holdout accuracy / train accuracy by model, fixed scales
2. Residuals by model, free scales
3. Residuals by model, fixed scales
4. Classification error, free scales
5. Classification error, fixed scales
6. Accuracy data, free scales
7. Accuracy data, fixed scales
8. Histograms of numeric columns
9. Boxplots of numeric columns
10. Duration barchart
11. False negative rate free scales
12. False negative rate fixed scales
13. False positive rate, free scales
14. False positive rate, fixed scales
15. True negative rate, free scales
16. True negative rate, fixed scales
17. True positive rate, free scales
18. True positive rate, fixed scales
19. Over or underfitting barchart
20. Model accuracy barchart
21. Barchart of each feature vs target by percentage
22. Barchart of each feature vs target by value
23. Correlation of numeric data as circles and colors
24. Correlation of numeric data as numbers and colors
25. Accuracy by model, free scales
26. Accuracy by model, fixed scales

<br><br>
The 5 tables the package returns automatically are:<br>
1. Head of the ensemble<br>
2. Head of the data frame<br>
3. Variance Inflation Factor of the numeric columns
4. Correlation of the data<br>
5. Summary report, including accuracy, duration, overfitting, sum of diagonals<br>
<br>
The package also returns all 12 summary tables (sometimes called confusion matrices), one for each of the models. These can be found in the Console. For example, using the dry_beams_small classification data set:

ensemble_bag_rf_test_pred BARBUNYA BOMBAY CALI DERMASON HOROZ SEKER SIRA
                 BARBUNYA       21      0    0        0     0     0    0
                 BOMBAY          0     16    0        0     0     0    0
                 CALI            0      0   35        0     0     0    0
                 DERMASON        0      0    0       76     0     0    0
                 HOROZ           0      0    0        0    36     0    0
                 SEKER           0      0    0        0     0    48    0
                 SIRA            0      0    0        0     0     0   51

A data summary is also in the Console. Using dry_beans_small as an example:
$Data_summary
  Eccentricity      ConvexArea         Extent          Solidity        roundness       ShapeFactor4   
 Min.   :0.2190   Min.   : 20825   Min.   :0.5802   Min.   :0.9551   Min.   :0.5718   Min.   :0.9550  
 1st Qu.:0.7175   1st Qu.: 37052   1st Qu.:0.7240   1st Qu.:0.9859   1st Qu.:0.8320   1st Qu.:0.9941  
 Median :0.7642   Median : 45261   Median :0.7606   Median :0.9886   Median :0.8833   Median :0.9966  
 Mean   :0.7517   Mean   : 53997   Mean   :0.7519   Mean   :0.9874   Mean   :0.8750   Mean   :0.9952  
 3rd Qu.:0.8117   3rd Qu.: 62159   3rd Qu.:0.7887   3rd Qu.:0.9903   3rd Qu.:0.9191   3rd Qu.:0.9980  
 Max.   :0.9082   Max.   :229994   Max.   :0.8325   Max.   :0.9937   Max.   :0.9879   Max.   :0.9996  
                                                                                                      
        y      
 BARBUNYA: 79  
 BOMBAY  : 31  
 CALI    : 97  
 DERMASON:212  
 HOROZ   :115  
 SEKER   :121  
 SIRA    :158  
