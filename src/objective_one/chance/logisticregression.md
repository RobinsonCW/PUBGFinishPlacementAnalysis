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

    ## -- Attaching packages -------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------------------------------- tidyverse_conflicts() --
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
  select(cols_to_keep) %>%
  mutate(top.10 = factor(top.10, labels = c("No", "Yes")))


summary(data.mod)
```

    ##   walkDistance       killPlace          boosts       weaponsAcquired 
    ##  Min.   :    0.0   Min.   :  1.00   Min.   : 0.000   Min.   : 0.000  
    ##  1st Qu.:  114.0   1st Qu.: 21.00   1st Qu.: 0.000   1st Qu.: 2.000  
    ##  Median :  607.7   Median : 46.00   Median : 0.000   Median : 3.000  
    ##  Mean   :  986.2   Mean   : 46.17   Mean   : 1.066   Mean   : 3.757  
    ##  3rd Qu.: 1616.0   3rd Qu.: 70.00   3rd Qu.: 2.000   3rd Qu.: 5.000  
    ##  Max.   :15370.0   Max.   :100.00   Max.   :24.000   Max.   :52.000  
    ##   damageDealt          heals            kills         top.10      
    ##  Min.   :   0.00   Min.   : 0.000   Min.   : 0.0000   No :162933  
    ##  1st Qu.:   0.00   1st Qu.: 0.000   1st Qu.: 0.0000   Yes: 19010  
    ##  Median :  65.73   Median : 0.000   Median : 0.0000               
    ##  Mean   : 112.62   Mean   : 1.012   Mean   : 0.8709               
    ##  3rd Qu.: 159.60   3rd Qu.: 1.000   3rd Qu.: 1.0000               
    ##  Max.   :2490.00   Max.   :49.000   Max.   :21.0000

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
train$predict <- as.factor(ifelse(model$fitted.values >0.5, "Yes", "No"))

#test$predict <- as.factor(ifelse(model$fitted.values >0.5, "Yes", "No"))

head(train)
```

    ##   walkDistance killPlace boosts weaponsAcquired damageDealt heals kills top.10
    ## 1       444.20        74      0               6        0.00     0     0     No
    ## 2        60.38        45      0               2      100.00     0     1     No
    ## 3      2080.00        22      5              10      159.00     0     1     No
    ## 4       706.30        59      0               8        0.00     0     0     No
    ## 5       427.80        71      1               2       91.14     1     0     No
    ## 6        11.97        89      0               2        0.00     0     0     No
    ##   predict
    ## 1      No
    ## 2      No
    ## 3     Yes
    ## 4      No
    ## 5      No
    ## 6      No

``` r
head(test)
```

    ##    walkDistance killPlace boosts weaponsAcquired damageDealt heals kills top.10
    ## 4         292.6        84      0               1       36.00     0     0     No
    ## 5        1913.0         7      1               8      236.00     7     3    Yes
    ## 10       2534.0        21      2               4      158.00     6     1     No
    ## 15       1135.0        53      2               2       29.06     3     0     No
    ## 16        622.4        37      0               3      129.70     0     1     No
    ## 18       1726.0        46      0               3       47.73     0     0     No

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
    ## -3.8865  -0.3346  -0.0086   0.4988   2.6907  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -1.814e-01  6.986e-02  -2.597  0.00942 ** 
    ## walkDistance     8.171e-04  2.181e-05  37.461  < 2e-16 ***
    ## killPlace       -7.598e-02  1.555e-03 -48.853  < 2e-16 ***
    ## boosts           3.814e-01  1.219e-02  31.290  < 2e-16 ***
    ## weaponsAcquired  7.734e-02  7.897e-03   9.794  < 2e-16 ***
    ## damageDealt      1.444e-03  2.544e-04   5.675 1.39e-08 ***
    ## heals           -3.031e-02  6.555e-03  -4.625 3.75e-06 ***
    ## kills           -3.666e-01  2.626e-02 -13.960  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 36975  on 26671  degrees of freedom
    ## Residual deviance: 17111  on 26664  degrees of freedom
    ## AIC: 17127
    ## 
    ## Number of Fisher Scoring iterations: 6

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
    ##        No  11151  1489
    ##        Yes  2185 11847
    ##                                           
    ##                Accuracy : 0.8623          
    ##                  95% CI : (0.8581, 0.8664)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.7245          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.8883          
    ##             Specificity : 0.8362          
    ##          Pos Pred Value : 0.8443          
    ##          Neg Pred Value : 0.8822          
    ##              Prevalence : 0.5000          
    ##          Detection Rate : 0.4442          
    ##    Detection Prevalence : 0.5261          
    ##       Balanced Accuracy : 0.8623          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

### Test

``` r
# test$predict <- as.factor(ifelse(model$fitted.values >0.5, "Yes", "No"))

test$predict <- predict(model, test, type="response")
test$predict <- as.factor(ifelse(test$predict >0.5, "Yes", "No"))


confusionMatrix(data=test$predict,  
                reference=test$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    No   Yes
    ##        No  40928   680
    ##        Yes  7981  4994
    ##                                           
    ##                Accuracy : 0.8413          
    ##                  95% CI : (0.8382, 0.8444)
    ##     No Information Rate : 0.896           
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.457           
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.88016         
    ##             Specificity : 0.83682         
    ##          Pos Pred Value : 0.38489         
    ##          Neg Pred Value : 0.98366         
    ##              Prevalence : 0.10395         
    ##          Detection Rate : 0.09149         
    ##    Detection Prevalence : 0.23771         
    ##       Balanced Accuracy : 0.85849         
    ##                                           
    ##        'Positive' Class : Yes             
    ##
