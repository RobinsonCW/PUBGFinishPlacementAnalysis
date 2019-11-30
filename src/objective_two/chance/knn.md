KNN
================
Chance Robinson
11/30/2019

  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Library Imports](#library-imports)
      - [Load the CSV Data](#load-the-csv-data)
      - [Remove Missing Values](#remove-missing-values)
      - [Specify Model Columns of
        Interest](#specify-model-columns-of-interest)
      - [Prepare Dataframe](#prepare-dataframe)
      - [KNN Train / Test Split](#knn-train-test-split)
      - [KNN Parameter Tuning](#knn-parameter-tuning)
      - [KNN Performance](#knn-performance)

# Exploratory Data Analysis

## Library Imports

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# KNN
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(class)
```

## Load the CSV Data

``` r
data <- read.csv("../../../data/pubg_solo_game_types.csv", stringsAsFactors=FALSE)
```

``` r
head(data)
```

    ##               Id        groupId        matchId assists boosts damageDealt DBNOs
    ## 1 315c96c26c9aac de04010b3458dd 6dc8ff871e21e6       0      0     100.000     0
    ## 2 311b84c6ff4390 eaba5fcb7fc1ae 292611730ca862       0      0       8.538     0
    ## 3 b7807186e3f679 3c08e461874749 2c30ddf481c52d       0      1     324.200     0
    ## 4 92022479b92ce7 2f2c33f548c4b9 07948d723b9c0f       0      3     254.300     0
    ## 5 47143f942503e0 e17a8867a393ec bc2faecb77e5ec       0      0     136.900     0
    ## 6 269c3fc4a26935 3c07be51998e6f ce9bc89b3ca08c       0      1     100.000     0
    ##   headshotKills heals killPlace killPoints kills killStreaks longestKill
    ## 1             0     0        45          0     1           1       58.53
    ## 2             0     0        48       1000     0           0        0.00
    ## 3             1     5         5        986     4           1       49.83
    ## 4             0    12        13          0     2           1       36.00
    ## 5             0     0        37          0     1           1       22.83
    ## 6             0     0        24          0     1           1       21.25
    ##   matchDuration matchType maxPlace numGroups rankPoints revives rideDistance
    ## 1          1424  solo-fpp       97        95       1560       0            0
    ## 2          1967  solo-fpp       96        92         -1       0         2004
    ## 3          1886  solo-fpp       97        94         -1       0         1228
    ## 4          1371  solo-fpp       96        95       1536       0         2367
    ## 5          1425  solo-fpp       96        94       1500       0            0
    ## 6          1398      solo       92        89       1509       0            0
    ##   roadKills swimDistance teamKills vehicleDestroys walkDistance weaponsAcquired
    ## 1         0         0.00         0               0        49.75               2
    ## 2         0         0.00         0               0      1089.00               6
    ## 3         0        76.84         0               0      2050.00               6
    ## 4         0        15.29         0               0      1787.00               3
    ## 5         0         0.00         0               0       270.70               1
    ## 6         0        61.00         0               0      1528.00               3
    ##   winPoints winPlacePerc top.10
    ## 1         0       0.1875      0
    ## 2      1500       0.7368      0
    ## 3      1462       0.8750      0
    ## 4         0       0.8211      0
    ## 5         0       0.3474      0
    ## 6         0       0.8462      0

## Remove Missing Values

``` r
# remove the row with no winPlacePerc   
data <- data[!data$Id == 'f70c74418bb064',]
```

## Specify Model Columns of Interest

``` r
cols_to_keep = c("walkDistance", "killPlace", "boosts", "weaponsAcquired", "damageDealt", "heals", "kills", "top.10")

head(data[cols_to_keep])
```

    ##   walkDistance killPlace boosts weaponsAcquired damageDealt heals kills top.10
    ## 1        49.75        45      0               2     100.000     0     1      0
    ## 2      1089.00        48      0               6       8.538     0     0      0
    ## 3      2050.00         5      1               6     324.200     5     4      0
    ## 4      1787.00        13      3               3     254.300    12     2      0
    ## 5       270.70        37      0               1     136.900     0     1      0
    ## 6      1528.00        24      1               3     100.000     0     1      0

## Prepare Dataframe

``` r
data.mod <- data %>%
  select(cols_to_keep) %>%
  mutate(top.10 = factor(top.10, labels = c("No", "Yes"))) 

summary(data.mod)
```

    ##   walkDistance       killPlace          boosts       weaponsAcquired  
    ##  Min.   :    0.0   Min.   :  1.00   Min.   : 0.000   Min.   :  0.000  
    ##  1st Qu.:  100.5   1st Qu.: 23.00   1st Qu.: 0.000   1st Qu.:  2.000  
    ##  Median :  502.7   Median : 47.00   Median : 0.000   Median :  3.000  
    ##  Mean   :  955.7   Mean   : 47.56   Mean   : 1.061   Mean   :  3.555  
    ##  3rd Qu.: 1588.0   3rd Qu.: 71.00   3rd Qu.: 2.000   3rd Qu.:  5.000  
    ##  Max.   :25780.0   Max.   :100.00   Max.   :28.000   Max.   :153.000  
    ##   damageDealt          heals           kills         top.10      
    ##  Min.   :   0.00   Min.   : 0.00   Min.   : 0.0000   No :644120  
    ##  1st Qu.:   0.00   1st Qu.: 0.00   1st Qu.: 0.0000   Yes: 74584  
    ##  Median :  73.96   Median : 0.00   Median : 0.0000               
    ##  Mean   : 117.32   Mean   : 1.01   Mean   : 0.9186               
    ##  3rd Qu.: 168.60   3rd Qu.: 1.00   3rd Qu.: 1.0000               
    ##  Max.   :2490.00   Max.   :63.00   Max.   :23.0000

``` r
# str(data.mod)
```

## KNN Train / Test Split

``` r
sample.data <- sample_frac(data.mod, 0.10)

sample.data <- downSample(sample.data, sample.data$top.10, list = FALSE)
sample.data$Class <- NULL

set.seed(1234)

train.indices <- createDataPartition(y = sample.data$top.10,p = 0.70, list = FALSE)
train <- sample.data[train.indices,]
test <- sample.data[-train.indices,]


set.seed(1234)
ctrl <- trainControl(method="cv")
knn.model <- train(top.10 ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 10)


# table(sample.data$top.10)

# dim(sample.data) # 14800     
# dim(train) # 10360     
# dim(test) # 4440   
```

## KNN Parameter Tuning

``` r
knn.model
```

    ## k-Nearest Neighbors 
    ## 
    ## 10358 samples
    ##     7 predictor
    ##     2 classes: 'No', 'Yes' 
    ## 
    ## Pre-processing: centered (7), scaled (7) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 9322, 9323, 9322, 9322, 9322, 9322, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   Accuracy   Kappa    
    ##    5  0.8734318  0.7468635
    ##    7  0.8761343  0.7522682
    ##    9  0.8765199  0.7530394
    ##   11  0.8767145  0.7534285
    ##   13  0.8790320  0.7580640
    ##   15  0.8805763  0.7611523
    ##   17  0.8820242  0.7640482
    ##   19  0.8820238  0.7640475
    ##   21  0.8818309  0.7636618
    ##   23  0.8807690  0.7615379
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was k = 17.

``` r
plot(knn.model)
```

![](knn_files/figure-gfm/knn-select-n-1.png)<!-- -->

## KNN Performance

``` r
knn.predict <- predict(knn.model, newdata = test )
confusionMatrix(knn.predict, test$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   No  Yes
    ##        No  1836  177
    ##        Yes  383 2042
    ##                                           
    ##                Accuracy : 0.8738          
    ##                  95% CI : (0.8637, 0.8835)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.7476          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.9202          
    ##             Specificity : 0.8274          
    ##          Pos Pred Value : 0.8421          
    ##          Neg Pred Value : 0.9121          
    ##              Prevalence : 0.5000          
    ##          Detection Rate : 0.4601          
    ##    Detection Prevalence : 0.5464          
    ##       Balanced Accuracy : 0.8738          
    ##                                           
    ##        'Positive' Class : Yes             
    ##
