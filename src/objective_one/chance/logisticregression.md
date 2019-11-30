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

    ## -- Attaching packages ----------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts -------------------------------------------------------------------------------------- tidyverse_conflicts() --
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

## Logistic Regression

## Train / Test Split

``` r
set.seed(1234)

# sample.data <- sample_frac(data.mod, 0.10)

data.mod <- downSample(data.mod, data.mod$top.10, list = FALSE)
data.mod$Class <- NULL

# head(sample.data)

split.perc = .70

train.indices = sample(1:dim(data.mod)[1],round(split.perc * dim(data.mod)[1]))

train = data.mod[train.indices,]
test = data.mod[-train.indices,]

# train <- downSample(train, train$top.10, list = FALSE)
# train$Class <- NULL


model <- glm(top.10 ~ ., data = train,  family = binomial("logit"))


# print(model.rf.train)
# 
# 
train$predict <- as.factor(ifelse(model$fitted.values >0.5, "Yes", "No"))

#test$predict <- as.factor(ifelse(model$fitted.values >0.5, "Yes", "No"))

head(train)
```

    ##        walkDistance killPlace boosts weaponsAcquired damageDealt heals kills
    ## 119850     2676.000         4      5               8      328.20     7     3
    ## 38823       418.400        39      0               3      119.10     0     1
    ## 10238         4.499        82      0               0       36.72     0     0
    ## 132124     6465.000        21      8               5       90.62    17     1
    ## 99156      2901.000         6      3               5      197.90     0     3
    ## 71526       407.500        62      0               5        0.00     0     0
    ##        top.10 predict
    ## 119850    Yes     Yes
    ## 38823      No      No
    ## 10238      No      No
    ## 132124    Yes     Yes
    ## 99156     Yes     Yes
    ## 71526      No      No

``` r
head(test)
```

    ##    walkDistance killPlace boosts weaponsAcquired damageDealt heals kills top.10
    ## 1        236.30        42      0               2       100.0     0     1     No
    ## 3       1664.00        54      0               4         0.0     0     0     No
    ## 8        837.10        32      1               3       100.0     2     1     No
    ## 11      2713.00         3      7               2       434.4     1     5     No
    ## 16       634.60        64      0               4         0.0     0     0     No
    ## 23        57.36        49      0               3       100.0     0     1     No

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
    ## -5.1913  -0.2913   0.0157   0.4435   2.8672  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -7.504e-01  4.428e-02 -16.946  < 2e-16 ***
    ## walkDistance     1.078e-03  1.218e-05  88.556  < 2e-16 ***
    ## killPlace       -6.571e-02  8.804e-04 -74.639  < 2e-16 ***
    ## boosts           3.744e-01  6.407e-03  58.426  < 2e-16 ***
    ## weaponsAcquired  6.494e-02  4.670e-03  13.908  < 2e-16 ***
    ## damageDealt      1.182e-03  1.498e-04   7.893 2.94e-15 ***
    ## heals           -3.153e-02  3.275e-03  -9.628  < 2e-16 ***
    ## kills           -3.449e-01  1.572e-02 -21.934  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 144754  on 104417  degrees of freedom
    ## Residual deviance:  61504  on 104410  degrees of freedom
    ## AIC: 61520
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
    ##        No  44569  5495
    ##        Yes  7611 46743
    ##                                           
    ##                Accuracy : 0.8745          
    ##                  95% CI : (0.8725, 0.8765)
    ##     No Information Rate : 0.5003          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.749           
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.8948          
    ##             Specificity : 0.8541          
    ##          Pos Pred Value : 0.8600          
    ##          Neg Pred Value : 0.8902          
    ##              Prevalence : 0.5003          
    ##          Detection Rate : 0.4477          
    ##    Detection Prevalence : 0.5205          
    ##       Balanced Accuracy : 0.8745          
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
    ##        No  18989  2300
    ##        Yes  3415 20046
    ##                                           
    ##                Accuracy : 0.8723          
    ##                  95% CI : (0.8692, 0.8754)
    ##     No Information Rate : 0.5006          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.7446          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.8971          
    ##             Specificity : 0.8476          
    ##          Pos Pred Value : 0.8544          
    ##          Neg Pred Value : 0.8920          
    ##              Prevalence : 0.4994          
    ##          Detection Rate : 0.4480          
    ##    Detection Prevalence : 0.5243          
    ##       Balanced Accuracy : 0.8723          
    ##                                           
    ##        'Positive' Class : Yes             
    ##
