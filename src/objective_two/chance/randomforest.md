Random Forest
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
      - [Random Forrest](#random-forrest)
      - [Train / Test Split](#train-test-split)
      - [Random Forest Performance](#random-forest-performance)
          - [Train](#train)
          - [Test](#test)

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
# Random Forest
library(randomForest)  
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(e1071)  
# downSample
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

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

cols_to_remove = c("Id", "groupId", "matchId", "matchType", "DBNOs", "winPlacePerc")

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
  select(-cols_to_remove) %>%
  mutate(top.10 = factor(top.10, labels = c("No", "Yes"))) 

summary(data.mod)
```

    ##     assists            boosts        damageDealt      headshotKills    
    ##  Min.   :0.00000   Min.   : 0.000   Min.   :   0.00   Min.   : 0.0000  
    ##  1st Qu.:0.00000   1st Qu.: 0.000   1st Qu.:   0.00   1st Qu.: 0.0000  
    ##  Median :0.00000   Median : 0.000   Median :  73.96   Median : 0.0000  
    ##  Mean   :0.05713   Mean   : 1.061   Mean   : 117.32   Mean   : 0.2445  
    ##  3rd Qu.:0.00000   3rd Qu.: 2.000   3rd Qu.: 168.60   3rd Qu.: 0.0000  
    ##  Max.   :5.00000   Max.   :28.000   Max.   :2490.00   Max.   :19.0000  
    ##      heals         killPlace        killPoints         kills        
    ##  Min.   : 0.00   Min.   :  1.00   Min.   :   0.0   Min.   : 0.0000  
    ##  1st Qu.: 0.00   1st Qu.: 23.00   1st Qu.:   0.0   1st Qu.: 0.0000  
    ##  Median : 0.00   Median : 47.00   Median :   0.0   Median : 0.0000  
    ##  Mean   : 1.01   Mean   : 47.56   Mean   : 447.3   Mean   : 0.9186  
    ##  3rd Qu.: 1.00   3rd Qu.: 71.00   3rd Qu.:1091.0   3rd Qu.: 1.0000  
    ##  Max.   :63.00   Max.   :100.00   Max.   :1970.0   Max.   :23.0000  
    ##   killStreaks       longestKill      matchDuration     maxPlace     
    ##  Min.   : 0.0000   Min.   :   0.00   Min.   : 950   Min.   :  8.00  
    ##  1st Qu.: 0.0000   1st Qu.:   0.00   1st Qu.:1384   1st Qu.: 94.00  
    ##  Median : 0.0000   Median :   0.00   Median :1456   Median : 96.00  
    ##  Mean   : 0.4724   Mean   :  21.45   Mean   :1602   Mean   : 94.11  
    ##  3rd Qu.: 1.0000   3rd Qu.:  20.69   3rd Qu.:1875   3rd Qu.: 97.00  
    ##  Max.   :18.0000   Max.   :1001.00   Max.   :2237   Max.   :100.00  
    ##    numGroups        rankPoints        revives   rideDistance    
    ##  Min.   :  1.00   Min.   :  -1.0   Min.   :0   Min.   :    0.0  
    ##  1st Qu.: 91.00   1st Qu.:  -1.0   1st Qu.:0   1st Qu.:    0.0  
    ##  Median : 93.00   Median :1496.0   Median :0   Median :    0.0  
    ##  Mean   : 91.33   Mean   : 967.2   Mean   :0   Mean   :  463.4  
    ##  3rd Qu.: 95.00   3rd Qu.:1517.0   3rd Qu.:0   3rd Qu.:    0.0  
    ##  Max.   :100.00   Max.   :2857.0   Max.   :0   Max.   :40710.0  
    ##    roadKills          swimDistance        teamKills      vehicleDestroys   
    ##  Min.   : 0.000000   Min.   :   0.000   Min.   :0.0000   Min.   :0.000000  
    ##  1st Qu.: 0.000000   1st Qu.:   0.000   1st Qu.:0.0000   1st Qu.:0.000000  
    ##  Median : 0.000000   Median :   0.000   Median :0.0000   Median :0.000000  
    ##  Mean   : 0.005123   Mean   :   5.091   Mean   :0.0114   Mean   :0.004519  
    ##  3rd Qu.: 0.000000   3rd Qu.:   0.000   3rd Qu.:0.0000   3rd Qu.:0.000000  
    ##  Max.   :18.000000   Max.   :1974.000   Max.   :1.0000   Max.   :5.000000  
    ##   walkDistance     weaponsAcquired     winPoints      top.10      
    ##  Min.   :    0.0   Min.   :  0.000   Min.   :   0.0   No :644120  
    ##  1st Qu.:  100.5   1st Qu.:  2.000   1st Qu.:   0.0   Yes: 74584  
    ##  Median :  502.7   Median :  3.000   Median :   0.0               
    ##  Mean   :  955.7   Mean   :  3.555   Mean   : 556.9               
    ##  3rd Qu.: 1588.0   3rd Qu.:  5.000   3rd Qu.:1492.0               
    ##  Max.   :25780.0   Max.   :153.000   Max.   :1922.0

``` r
# str(data.mod)
```

## Random Forrest

## Train / Test Split

``` r
set.seed(1234)

sample.data <- sample_frac(data.mod, 0.10)

sample.data <- downSample(sample.data, sample.data$top.10, list = FALSE)
sample.data$Class <- NULL

# head(sample.data)

split.perc = .70

train.indices = sample(1:dim(sample.data)[1],round(split.perc * dim(sample.data)[1]))

train = sample.data[train.indices,]
test = sample.data[-train.indices,]

# train <- downSample(train, train$top.10, list = FALSE)
# train$Class <- NULL


model.rf.train <- randomForest(top.10 ~ ., data = train, ntree = 300, mtry = 5)

print(model.rf.train)
```

    ## 
    ## Call:
    ##  randomForest(formula = top.10 ~ ., data = train, ntree = 300,      mtry = 5) 
    ##                Type of random forest: classification
    ##                      Number of trees: 300
    ## No. of variables tried at each split: 5
    ## 
    ##         OOB estimate of  error rate: 9.15%
    ## Confusion matrix:
    ##       No  Yes class.error
    ## No  4579  735   0.1383139
    ## Yes  238 5078   0.0447705

``` r
p1 <- predict(model.rf.train, train)
p2 <- predict(model.rf.train, test)


plot(model.rf.train) 
```

![](randomforest_files/figure-gfm/rf-train-test-split-1.png)<!-- -->

``` r
varImp(model.rf.train)
```

    ##                     Overall
    ## assists           20.120845
    ## boosts           681.837075
    ## damageDealt      320.903834
    ## headshotKills     41.279226
    ## heals            207.261989
    ## killPlace       1047.080032
    ## killPoints        79.210746
    ## kills            259.696263
    ## killStreaks       78.631173
    ## longestKill      311.250824
    ## matchDuration    176.311817
    ## maxPlace         112.879513
    ## numGroups        121.447438
    ## rankPoints       114.497284
    ## revives            0.000000
    ## rideDistance     110.734111
    ## roadKills          2.608494
    ## swimDistance      55.834645
    ## teamKills          3.866909
    ## vehicleDestroys    2.953464
    ## walkDistance    1293.524624
    ## weaponsAcquired  174.505103
    ## winPoints         77.586838

## Random Forest Performance

### Train

``` r
confusionMatrix(data=p1,  
                reference=train$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   No  Yes
    ##        No  5312    0
    ##        Yes    2 5316
    ##                                      
    ##                Accuracy : 0.9998     
    ##                  95% CI : (0.9993, 1)
    ##     No Information Rate : 0.5001     
    ##     P-Value [Acc > NIR] : <2e-16     
    ##                                      
    ##                   Kappa : 0.9996     
    ##                                      
    ##  Mcnemar's Test P-Value : 0.4795     
    ##                                      
    ##             Sensitivity : 1.0000     
    ##             Specificity : 0.9996     
    ##          Pos Pred Value : 0.9996     
    ##          Neg Pred Value : 1.0000     
    ##              Prevalence : 0.5001     
    ##          Detection Rate : 0.5001     
    ##    Detection Prevalence : 0.5003     
    ##       Balanced Accuracy : 0.9998     
    ##                                      
    ##        'Positive' Class : Yes        
    ## 

### Test

``` r
confusionMatrix(data=p2,  
                reference=test$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   No  Yes
    ##        No  1932   97
    ##        Yes  347 2180
    ##                                          
    ##                Accuracy : 0.9025         
    ##                  95% CI : (0.8936, 0.911)
    ##     No Information Rate : 0.5002         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.8051         
    ##                                          
    ##  Mcnemar's Test P-Value : < 2.2e-16      
    ##                                          
    ##             Sensitivity : 0.9574         
    ##             Specificity : 0.8477         
    ##          Pos Pred Value : 0.8627         
    ##          Neg Pred Value : 0.9522         
    ##              Prevalence : 0.4998         
    ##          Detection Rate : 0.4785         
    ##    Detection Prevalence : 0.5547         
    ##       Balanced Accuracy : 0.9026         
    ##                                          
    ##        'Positive' Class : Yes            
    ##
