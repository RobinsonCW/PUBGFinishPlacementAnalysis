Logistic Regression
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
      - [Logistic Regression](#logistic-regression)
      - [Train / Test Split](#train-test-split)
      - [Logistic Regression
        Performance](#logistic-regression-performance)
          - [Train](#train)
          - [Test](#test)

# Exploratory Data Analysis

## Library Imports

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# Random Forest

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
cols_to_keep = c("walkDistance", "killPlace", "boosts", "weaponsAcquired", "kills", "numGroups", "matchDuration", 
                 "rideDistance", "longestKill", "killStreaks", "assists", 
                 "top.10")

cols_to_remove = c("Id", "groupId", "matchId", "matchType", "DBNOs", "winPlacePerc")

head(data[cols_to_keep])
```

    ##   walkDistance killPlace boosts weaponsAcquired kills numGroups matchDuration
    ## 1       1528.0        24      1               3     1        89          1398
    ## 2        471.9        79      0               3     0        95          1945
    ## 3        231.7        38      1               4     1        86          2042
    ## 4        292.6        84      0               1     0        92          1999
    ## 5       1913.0         7      1               8     3        88          1423
    ## 6        870.9        65      0               3     0        94          1471
    ##   rideDistance longestKill killStreaks assists top.10
    ## 1          0.0      21.250           1       0      0
    ## 2        129.3       0.000           0       0      0
    ## 3          0.0       7.667           1       0      0
    ## 4          0.0       0.000           0       0      0
    ## 5          0.0      11.720           1       0      1
    ## 6          0.0       0.000           0       0      0

## Prepare Dataframe

``` r
data.mod <- data %>%
  select(cols_to_keep) %>%
  mutate(kills= kills * 100 / numGroups) %>% # Normalized Kills
  # mutate(matchDurationLength = as.factor(ifelse(matchDuration < mean(matchDuration), "Low", "High"))) %>%
  mutate(top.10 = factor(top.10, labels = c("No", "Yes"))) 
  #mutate(damageDealt = ifelse(damageDealt > 0, log(damageDealt), damageDealt)) %>%
  # mutate(longestKill = ifelse(longestKill > 0, log(longestKill), longestKill)) %>%
  # mutate(killPlace= killPlace * 100 / numGroups) # Normalized Kills




summary(data.mod)
```

    ##   walkDistance       killPlace          boosts       weaponsAcquired 
    ##  Min.   :    0.0   Min.   :  1.00   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.:  114.0   1st Qu.: 21.00   1st Qu.: 0.000   1st Qu.: 2.000  
    ##  Median :  607.7   Median : 46.00   Median : 0.000   Median : 3.000  
    ##  Mean   :  986.2   Mean   : 46.17   Mean   : 1.066   Mean   : 3.757  
    ##  3rd Qu.: 1616.0   3rd Qu.: 70.00   3rd Qu.: 2.000   3rd Qu.: 5.000  
    ##  Max.   :15370.0   Max.   :100.00   Max.   :24.000   Max.   :52.000  
    ##      kills            numGroups     matchDuration   rideDistance     
    ##  Min.   :  0.0000   Min.   : 1.00   Min.   : 950   Min.   :    0.00  
    ##  1st Qu.:  0.0000   1st Qu.:89.00   1st Qu.:1431   1st Qu.:    0.00  
    ##  Median :  0.0000   Median :92.00   Median :1771   Median :    0.00  
    ##  Mean   :  0.9649   Mean   :87.29   Mean   :1676   Mean   :  640.98  
    ##  3rd Qu.:  1.1236   3rd Qu.:94.00   3rd Qu.:1903   3rd Qu.:    1.16  
    ##  Max.   :100.0000   Max.   :99.00   Max.   :2237   Max.   :33970.00  
    ##   longestKill       killStreaks         assists        top.10      
    ##  Min.   :   0.00   Min.   : 0.0000   Min.   :0.00000   No :162933  
    ##  1st Qu.:   0.00   1st Qu.: 0.0000   1st Qu.:0.00000   Yes: 19010  
    ##  Median :   0.00   Median : 0.0000   Median :0.00000               
    ##  Mean   :  20.70   Mean   : 0.4429   Mean   :0.05562               
    ##  3rd Qu.:  15.91   3rd Qu.: 1.0000   3rd Qu.:0.00000               
    ##  Max.   :1001.00   Max.   :18.0000   Max.   :4.00000

``` r
# str(data.mod)
```

## Logistic Regression

## Train / Test Split

``` r
set.seed(1234)

# sample.data <- sample_frac(data.mod, 0.10)

# data.mod <- downSample(data.mod, data.mod$top.10, list = FALSE)
# data.mod$Class <- NULL

# head(sample.data)

split.perc = .70

train.indices = sample(1:dim(data.mod)[1],round(split.perc * dim(data.mod)[1]))

train = data.mod[train.indices,]
test = data.mod[-train.indices,]

train <- downSample(train, train$top.10, list = FALSE)
train$Class <- NULL


model <- glm(top.10 ~ ., data = train,  family = binomial("logit"))


# print(model.rf.train)
# 
# 
train$predict <- as.factor(ifelse(model$fitted.values >0.55, "Yes", "No"))

#test$predict <- as.factor(ifelse(model$fitted.values >0.5, "Yes", "No"))

head(train)
```

    ##   walkDistance killPlace boosts weaponsAcquired    kills numGroups
    ## 1       444.20        74      0               6 0.000000        92
    ## 2        60.38        45      0               2 1.063830        94
    ## 3      2080.00        22      5              10 1.351351        74
    ## 4       706.30        59      0               8 0.000000        96
    ## 5       427.80        71      1               2 0.000000        92
    ## 6        11.97        89      0               2 0.000000        94
    ##   matchDuration rideDistance longestKill killStreaks assists top.10 predict
    ## 1          1915          0.0       0.000           0       0     No      No
    ## 2          1394          0.0       7.848           1       0     No      No
    ## 3          1384          0.0      21.560           1       1     No     Yes
    ## 4          1891        921.5       0.000           0       0     No      No
    ## 5          1371          0.0       0.000           0       0     No      No
    ## 6          1501          0.0       0.000           0       0     No      No

``` r
head(test)
```

    ##    walkDistance killPlace boosts weaponsAcquired    kills numGroups
    ## 4         292.6        84      0               1 0.000000        92
    ## 5        1913.0         7      1               8 3.409091        88
    ## 10       2534.0        21      2               4 1.265823        79
    ## 15       1135.0        53      2               2 0.000000        93
    ## 16        622.4        37      0               3 1.052632        95
    ## 18       1726.0        46      0               3 0.000000        97
    ##    matchDuration rideDistance longestKill killStreaks assists top.10
    ## 4           1999            0       0.000           0       0     No
    ## 5           1423            0      11.720           1       0    Yes
    ## 10          1462            0      13.820           1       0     No
    ## 15          1381         1093       0.000           0       0     No
    ## 16          1882            0       7.607           1       0     No
    ## 18          1440            0       0.000           0       0     No

## Logistic Regression Performance

### Train

``` r
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = top.10 ~ ., family = binomial("logit"), data = train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.7316  -0.2445   0.0063   0.4485   6.8295  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      3.604e+00  1.875e-01  19.219  < 2e-16 ***
    ## walkDistance     8.721e-04  2.567e-05  33.976  < 2e-16 ***
    ## killPlace       -1.195e-01  2.608e-03 -45.814  < 2e-16 ***
    ## boosts           2.969e-01  1.208e-02  24.577  < 2e-16 ***
    ## weaponsAcquired  7.748e-02  8.333e-03   9.298  < 2e-16 ***
    ## kills           -8.100e-02  1.460e-02  -5.549 2.88e-08 ***
    ## numGroups        1.630e-02  1.430e-03  11.399  < 2e-16 ***
    ## matchDuration   -1.882e-03  9.854e-05 -19.093  < 2e-16 ***
    ## rideDistance     2.024e-04  1.125e-05  17.994  < 2e-16 ***
    ## longestKill     -1.701e-03  3.959e-04  -4.297 1.73e-05 ***
    ## killStreaks     -1.964e+00  7.068e-02 -27.793  < 2e-16 ***
    ## assists          6.719e-01  6.905e-02   9.731  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 36975  on 26671  degrees of freedom
    ## Residual deviance: 15589  on 26660  degrees of freedom
    ## AIC: 15613
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
# confint(model) 
```

``` r
confusionMatrix(data=train$predict,  
                reference=train$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    No   Yes
    ##        No  11559  1643
    ##        Yes  1777 11693
    ##                                           
    ##                Accuracy : 0.8718          
    ##                  95% CI : (0.8677, 0.8758)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : < 2e-16         
    ##                                           
    ##                   Kappa : 0.7436          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.02295         
    ##                                           
    ##             Sensitivity : 0.8768          
    ##             Specificity : 0.8668          
    ##          Pos Pred Value : 0.8681          
    ##          Neg Pred Value : 0.8755          
    ##              Prevalence : 0.5000          
    ##          Detection Rate : 0.4384          
    ##    Detection Prevalence : 0.5050          
    ##       Balanced Accuracy : 0.8718          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

### Test

``` r
# test$predict <- as.factor(ifelse(model$fitted.values >0.5, "Yes", "No"))

test$predict <- predict(model, test, type="response")
test$predict <- as.factor(ifelse(test$predict >0.55, "Yes", "No"))


confusionMatrix(data=test$predict,  
                reference=test$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    No   Yes
    ##        No  42353   727
    ##        Yes  6556  4947
    ##                                           
    ##                Accuracy : 0.8666          
    ##                  95% CI : (0.8637, 0.8694)
    ##     No Information Rate : 0.896           
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.5074          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.87187         
    ##             Specificity : 0.86596         
    ##          Pos Pred Value : 0.43006         
    ##          Neg Pred Value : 0.98312         
    ##              Prevalence : 0.10395         
    ##          Detection Rate : 0.09063         
    ##    Detection Prevalence : 0.21074         
    ##       Balanced Accuracy : 0.86891         
    ##                                           
    ##        'Positive' Class : Yes             
    ##
