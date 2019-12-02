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

    ## -- Attaching packages --------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
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
    ## 1 269c3fc4a26935 3c07be51998e6f ce9bc89b3ca08c       0      1      100.00     0
    ## 2 73348483a5974b 1c8e486a643207 85601fe44d519b       0      0       17.81     0
    ## 3 caa1a36afeb7b2 c653cfca3b8b06 e5e181d2da0334       0      1      100.00     0
    ## 4 5fd62798396ca8 bb19a05801d30d 9e3c46f8acde82       0      0       36.00     0
    ## 5 18d002b46b1abc 00a3f236559532 eccc44618c0442       0      1      236.00     0
    ## 6 d08ce24e7a7973 d57ed9de010a4e 1eda9747e31f1f       0      0        0.00     0
    ##   headshotKills heals killPlace killPoints kills killStreaks longestKill
    ## 1             0     0        24          0     1           1      21.250
    ## 2             0     0        79       1274     0           0       0.000
    ## 3             0     0        38       1000     1           1       7.667
    ## 4             0     0        84          0     0           0       0.000
    ## 5             0     7         7       1142     3           1      11.720
    ## 6             0     0        65          0     0           0       0.000
    ##   matchDuration matchType maxPlace numGroups rankPoints revives rideDistance
    ## 1          1398      solo       92        89       1509       0          0.0
    ## 2          1945      solo       99        95         -1       0        129.3
    ## 3          2042      solo       90        86         -1       0          0.0
    ## 4          1999      solo       94        92       1507       0          0.0
    ## 5          1423      solo       94        88         -1       0          0.0
    ## 6          1471      solo       99        94       1500       0          0.0
    ##   roadKills swimDistance teamKills vehicleDestroys walkDistance weaponsAcquired
    ## 1         0           61         0               0       1528.0               3
    ## 2         0            0         0               0        471.9               3
    ## 3         0            0         0               0        231.7               4
    ## 4         0            0         0               0        292.6               1
    ## 5         0            0         0               0       1913.0               8
    ## 6         0            0         0               0        870.9               3
    ##   winPoints winPlacePerc top.10
    ## 1         0       0.8462      0
    ## 2      1536       0.2245      0
    ## 3      1500       0.1573      0
    ## 4         0       0.1075      0
    ## 5      1557       0.9355      1
    ## 6         0       0.3878      0

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
    ## 1       1528.0        24      1               3      100.00     0     1      0
    ## 2        471.9        79      0               3       17.81     0     0      0
    ## 3        231.7        38      1               4      100.00     0     1      0
    ## 4        292.6        84      0               1       36.00     0     0      0
    ## 5       1913.0         7      1               8      236.00     7     3      1
    ## 6        870.9        65      0               3        0.00     0     0      0

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
    ##  Median :0.00000   Median : 0.000   Median :  65.73   Median : 0.0000  
    ##  Mean   :0.05562   Mean   : 1.066   Mean   : 112.62   Mean   : 0.2238  
    ##  3rd Qu.:0.00000   3rd Qu.: 2.000   3rd Qu.: 159.60   3rd Qu.: 0.0000  
    ##  Max.   :4.00000   Max.   :24.000   Max.   :2490.00   Max.   :19.0000  
    ##      heals          killPlace        killPoints         kills        
    ##  Min.   : 0.000   Min.   :  1.00   Min.   :   0.0   Min.   : 0.0000  
    ##  1st Qu.: 0.000   1st Qu.: 21.00   1st Qu.:   0.0   1st Qu.: 0.0000  
    ##  Median : 0.000   Median : 46.00   Median :   0.0   Median : 0.0000  
    ##  Mean   : 1.012   Mean   : 46.17   Mean   : 407.8   Mean   : 0.8709  
    ##  3rd Qu.: 1.000   3rd Qu.: 70.00   3rd Qu.:1032.0   3rd Qu.: 1.0000  
    ##  Max.   :49.000   Max.   :100.00   Max.   :1962.0   Max.   :21.0000  
    ##   killStreaks       longestKill      matchDuration     maxPlace     
    ##  Min.   : 0.0000   Min.   :   0.00   Min.   : 950   Min.   : 11.00  
    ##  1st Qu.: 0.0000   1st Qu.:   0.00   1st Qu.:1431   1st Qu.: 93.00  
    ##  Median : 0.0000   Median :   0.00   Median :1771   Median : 96.00  
    ##  Mean   : 0.4429   Mean   :  20.70   Mean   :1676   Mean   : 91.34  
    ##  3rd Qu.: 1.0000   3rd Qu.:  15.91   3rd Qu.:1903   3rd Qu.: 97.00  
    ##  Max.   :18.0000   Max.   :1001.00   Max.   :2237   Max.   :100.00  
    ##    numGroups       rankPoints        revives   rideDistance     
    ##  Min.   : 1.00   Min.   :  -1.0   Min.   :0   Min.   :    0.00  
    ##  1st Qu.:89.00   1st Qu.:  -1.0   1st Qu.:0   1st Qu.:    0.00  
    ##  Median :92.00   Median :1494.0   Median :0   Median :    0.00  
    ##  Mean   :87.29   Mean   : 978.5   Mean   :0   Mean   :  640.98  
    ##  3rd Qu.:94.00   3rd Qu.:1510.0   3rd Qu.:0   3rd Qu.:    1.16  
    ##  Max.   :99.00   Max.   :2857.0   Max.   :0   Max.   :33970.00  
    ##    roadKills          swimDistance        teamKills       vehicleDestroys  
    ##  Min.   : 0.000000   Min.   :   0.000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.: 0.000000   1st Qu.:   0.000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median : 0.000000   Median :   0.000   Median :0.00000   Median :0.00000  
    ##  Mean   : 0.009948   Mean   :   5.878   Mean   :0.01499   Mean   :0.00753  
    ##  3rd Qu.: 0.000000   3rd Qu.:   0.000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :18.000000   Max.   :1606.000   Max.   :1.00000   Max.   :3.00000  
    ##   walkDistance     weaponsAcquired    winPoints      top.10      
    ##  Min.   :    0.0   Min.   : 0.000   Min.   :   0.0   No :162933  
    ##  1st Qu.:  114.0   1st Qu.: 2.000   1st Qu.:   0.0   Yes: 19010  
    ##  Median :  607.7   Median : 3.000   Median :   0.0               
    ##  Mean   :  986.2   Mean   : 3.757   Mean   : 536.9               
    ##  3rd Qu.: 1616.0   3rd Qu.: 5.000   3rd Qu.:1492.0               
    ##  Max.   :15370.0   Max.   :52.000   Max.   :1892.0

``` r
# str(data.mod)
```

## Random Forrest

## Train / Test Split

``` r
set.seed(1234)

sample.data <- sample_frac(data.mod, 1)

# sample.data <- downSample(sample.data, sample.data$top.10, list = FALSE)
# sample.data$Class <- NULL

# head(sample.data)

split.perc = .70

train.indices = sample(1:dim(sample.data)[1],round(split.perc * dim(sample.data)[1]))

train = sample.data[train.indices,]
test = sample.data[-train.indices,]

train <- downSample(train, train$top.10, list = FALSE)
train$Class <- NULL


model.rf.train <- randomForest(top.10 ~ ., data = train, ntree = 300, mtry = 5, cutoff = c(0.36,1-0.36))

?randomForest
```

    ## starting httpd help server ... done

``` r
print(model.rf.train)
```

    ## 
    ## Call:
    ##  randomForest(formula = top.10 ~ ., data = train, ntree = 300,      mtry = 5, cutoff = c(0.36, 1 - 0.36)) 
    ##                Type of random forest: classification
    ##                      Number of trees: 300
    ## No. of variables tried at each split: 5
    ## 
    ##         OOB estimate of  error rate: 10.14%
    ## Confusion matrix:
    ##        No   Yes class.error
    ## No  11996  1337   0.1002775
    ## Yes  1368 11965   0.1026026

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
    ## assists           54.338518
    ## boosts          1518.743367
    ## damageDealt      819.544579
    ## headshotKills     75.054951
    ## heals            399.647631
    ## killPlace       3461.024217
    ## killPoints       171.735511
    ## kills            691.138188
    ## killStreaks      279.859420
    ## longestKill      605.845546
    ## matchDuration    462.440131
    ## maxPlace         366.007736
    ## numGroups        425.580752
    ## rankPoints       289.506244
    ## revives            0.000000
    ## rideDistance     353.727482
    ## roadKills          9.217478
    ## swimDistance     132.533992
    ## teamKills         18.047137
    ## vehicleDestroys   15.835489
    ## walkDistance    2447.567213
    ## weaponsAcquired  411.035865
    ## winPoints        182.302958

## Random Forest Performance

### Train

``` r
confusionMatrix(data=p1,  
                reference=train$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    No   Yes
    ##        No  13307    30
    ##        Yes    26 13303
    ##                                           
    ##                Accuracy : 0.9979          
    ##                  95% CI : (0.9973, 0.9984)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.9958          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.6885          
    ##                                           
    ##             Sensitivity : 0.9977          
    ##             Specificity : 0.9980          
    ##          Pos Pred Value : 0.9980          
    ##          Neg Pred Value : 0.9978          
    ##              Prevalence : 0.5000          
    ##          Detection Rate : 0.4989          
    ##    Detection Prevalence : 0.4998          
    ##       Balanced Accuracy : 0.9979          
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
    ## Prediction    No   Yes
    ##        No  44116   578
    ##        Yes  4790  5099
    ##                                           
    ##                Accuracy : 0.9017          
    ##                  95% CI : (0.8991, 0.9041)
    ##     No Information Rate : 0.896           
    ##     P-Value [Acc > NIR] : 6.528e-06       
    ##                                           
    ##                   Kappa : 0.6026          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.89819         
    ##             Specificity : 0.90206         
    ##          Pos Pred Value : 0.51562         
    ##          Neg Pred Value : 0.98707         
    ##              Prevalence : 0.10401         
    ##          Detection Rate : 0.09342         
    ##    Detection Prevalence : 0.18117         
    ##       Balanced Accuracy : 0.90012         
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
# train <- read.csv("../../../data/pubg_solo_game_types_train_downsampled.csv", stringsAsFactors=FALSE)
# 
# test <- read.csv("../../../data/pubg_solo_game_types_test_full.csv", stringsAsFactors=FALSE)
# 
# 
# train <- train %>%
#   select(-cols_to_remove) %>%
#   mutate(top.10 = factor(top.10, labels = c("No", "Yes"))) 
# 
# 
# test <- test %>%
#   select(-cols_to_remove) %>%
#   mutate(top.10 = factor(top.10, labels = c("No", "Yes"))) 
# 
# 
# model.rf.train <- randomForest(as.factor(top.10) ~ ., data = train, ntree = 300, mtry = 5)
# 
# print(model.rf.train)
# 
# 
# p1 <- predict(model.rf.train, train)
# p2 <- predict(model.rf.train, test)
# 
# 
# print(model.rf.train)
# plot(model.rf.train) 
# varImp(model.rf.train)
# 
# 
# confusionMatrix(data=p1,  
#                 reference=train$top.10, "Yes")
# 
# 
# confusionMatrix(data=p2,  
#                 reference=test$top.10, "Yes")
```
