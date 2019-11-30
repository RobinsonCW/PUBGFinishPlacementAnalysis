Naive Bayes
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
      - [Naive Bayes Train / Test Split](#naive-bayes-train-test-split)
      - [Naive Bayes Performance](#naive-bayes-performance)
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
# Naive Bayes
library(naivebayes)
```

    ## naivebayes 0.9.6 loaded

``` r
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

cols_to_remove = c("Id", "groupId", "matchId", "matchType", "DBNOs")

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
    ##   walkDistance     weaponsAcquired     winPoints       winPlacePerc   
    ##  Min.   :    0.0   Min.   :  0.000   Min.   :   0.0   Min.   :0.0000  
    ##  1st Qu.:  100.5   1st Qu.:  2.000   1st Qu.:   0.0   1st Qu.:0.2292  
    ##  Median :  502.7   Median :  3.000   Median :   0.0   Median :0.4839  
    ##  Mean   :  955.7   Mean   :  3.555   Mean   : 556.9   Mean   :0.4873  
    ##  3rd Qu.: 1588.0   3rd Qu.:  5.000   3rd Qu.:1492.0   3rd Qu.:0.7474  
    ##  Max.   :25780.0   Max.   :153.000   Max.   :1922.0   Max.   :1.0000  
    ##  top.10      
    ##  No :644120  
    ##  Yes: 74584  
    ##              
    ##              
    ##              
    ## 

``` r
# str(data.mod)
```

## Naive Bayes Train / Test Split

``` r
set.seed(1234)
split.perc = .70

train.indices = sample(1:dim(data.mod)[1],round(split.perc * dim(data.mod)[1]))

train = data.mod[train.indices,]
test = data.mod[-train.indices,]


train <- downSample(train, train$top.10, list = FALSE)
train$Class <- NULL


model.nb.train <- naive_bayes(top.10 ~ ., data = train, laplace = TRUE, usekernel = TRUE)

# p <- predict(model.nb, train, type = 'prob')
# cbind(p, train)

p1 <- predict(model.nb.train, train)
```

    ## Warning: predict.naive_bayes(): More features in the newdata are provided as
    ## there are probability tables in the object. Calculation is performed based on
    ## features to be found in the tables.

``` r
p2 <- predict(model.nb.train, test)
```

    ## Warning: predict.naive_bayes(): More features in the newdata are provided as
    ## there are probability tables in the object. Calculation is performed based on
    ## features to be found in the tables.

``` r
# (tab1 <- table(p1, data.mod.nb$Attrition))
```

## Naive Bayes Performance

### Train

``` r
confusionMatrix(data=p1,  
                reference=train$top.10, "Yes")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    No   Yes
    ##        No  50641  9619
    ##        Yes  1639 42661
    ##                                           
    ##                Accuracy : 0.8923          
    ##                  95% CI : (0.8904, 0.8942)
    ##     No Information Rate : 0.5             
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.7847          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.8160          
    ##             Specificity : 0.9686          
    ##          Pos Pred Value : 0.9630          
    ##          Neg Pred Value : 0.8404          
    ##              Prevalence : 0.5000          
    ##          Detection Rate : 0.4080          
    ##    Detection Prevalence : 0.4237          
    ##       Balanced Accuracy : 0.8923          
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
    ## Prediction     No    Yes
    ##        No  187212   4141
    ##        Yes   6095  18163
    ##                                           
    ##                Accuracy : 0.9525          
    ##                  95% CI : (0.9516, 0.9534)
    ##     No Information Rate : 0.8966          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.7536          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.81434         
    ##             Specificity : 0.96847         
    ##          Pos Pred Value : 0.74874         
    ##          Neg Pred Value : 0.97836         
    ##              Prevalence : 0.10345         
    ##          Detection Rate : 0.08424         
    ##    Detection Prevalence : 0.11251         
    ##       Balanced Accuracy : 0.89140         
    ##                                           
    ##        'Positive' Class : Yes             
    ##
