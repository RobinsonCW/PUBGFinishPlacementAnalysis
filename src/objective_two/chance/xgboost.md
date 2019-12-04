Extreme Gradient Boosting
================
Chance Robinson
12/03/2019

  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Library Imports](#library-imports)
      - [Load the CSV Data](#load-the-csv-data)
      - [Remove Missing Values](#remove-missing-values)
      - [Specify Model Columns of
        Interest](#specify-model-columns-of-interest)
      - [Prepare Dataframe](#prepare-dataframe)
      - [XG Boost](#xg-boost)
      - [Train / Test Split](#train-test-split)
          - [Test](#test)
          - [Area Under the Curve](#area-under-the-curve)

# Exploratory Data Analysis

## Library Imports

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# Gradient Boosing
library(xgboost)  
```

    ## 
    ## Attaching package: 'xgboost'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(Matrix)
```

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

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

``` r
# ROC Curves
library(ROCR)
```

    ## Loading required package: gplots

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

``` r
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

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

``` r
data <- data %>%
  mutate(winPoints = ifelse(rankPoints != -1 & winPoints == 0, mean(winPoints), winPoints )) %>%
  mutate(winPoints = ifelse(rankPoints != -1 & killPoints == 0, mean(killPoints), killPoints ))

# outliers <- data[   data$walkDistance > mean(data$walkDistance) + (sd(data$walkDistance) * 3), ]
```

## Specify Model Columns of Interest

``` r
cols_to_keep = c("walkDistance", "killPlace", "boosts", "weaponsAcquired", "damageDealt", "heals", "kills", "top.10")

cols_to_remove = c("Id", "groupId", "matchId", "matchType", "DBNOs", "revives", "winPlacePerc")

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
  select(-cols_to_remove)
  # mutate(top.10 = factor(top.10, labels = c("No", "Yes"))) 

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
    ##    numGroups       rankPoints      rideDistance        roadKills        
    ##  Min.   : 1.00   Min.   :  -1.0   Min.   :    0.00   Min.   : 0.000000  
    ##  1st Qu.:89.00   1st Qu.:  -1.0   1st Qu.:    0.00   1st Qu.: 0.000000  
    ##  Median :92.00   Median :1494.0   Median :    0.00   Median : 0.000000  
    ##  Mean   :87.29   Mean   : 978.5   Mean   :  640.98   Mean   : 0.009948  
    ##  3rd Qu.:94.00   3rd Qu.:1510.0   3rd Qu.:    1.16   3rd Qu.: 0.000000  
    ##  Max.   :99.00   Max.   :2857.0   Max.   :33970.00   Max.   :18.000000  
    ##   swimDistance        teamKills       vehicleDestroys    walkDistance    
    ##  Min.   :   0.000   Min.   :0.00000   Min.   :0.00000   Min.   :    0.0  
    ##  1st Qu.:   0.000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:  114.0  
    ##  Median :   0.000   Median :0.00000   Median :0.00000   Median :  607.7  
    ##  Mean   :   5.878   Mean   :0.01499   Mean   :0.00753   Mean   :  986.2  
    ##  3rd Qu.:   0.000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.: 1616.0  
    ##  Max.   :1606.000   Max.   :1.00000   Max.   :3.00000   Max.   :15370.0  
    ##  weaponsAcquired    winPoints          top.10      
    ##  Min.   : 0.000   Min.   : 134.0   Min.   :0.0000  
    ##  1st Qu.: 2.000   1st Qu.: 407.8   1st Qu.:0.0000  
    ##  Median : 3.000   Median : 407.8   Median :0.0000  
    ##  Mean   : 3.757   Mean   : 669.6   Mean   :0.1045  
    ##  3rd Qu.: 5.000   3rd Qu.:1032.0   3rd Qu.:0.0000  
    ##  Max.   :52.000   Max.   :1962.0   Max.   :1.0000

``` r
table(data.mod$top.10)
```

    ## 
    ##      0      1 
    ## 162933  19010

## XG Boost

## Train / Test Split

``` r
set.seed(1234)

split.perc = .70

train.indices = sample(1:dim(data.mod)[1],round(split.perc * dim(data.mod)[1]))

train = data.mod[train.indices,]
test = data.mod[-train.indices,]

train <- downSample(train, as.factor(train$top.10), list = FALSE)
train$Class <- NULL
```

``` r
trainm <- sparse.model.matrix(top.10 ~ ., data = train)

# trainm
train_label <- train[, "top.10"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

?xgb.DMatrix
```

    ## starting httpd help server ... done

``` r
testm <- sparse.model.matrix(top.10 ~ ., data = test)
# trainm
test_label <- test[, "top.10"]

# train_label
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
```

``` r
nc <- length(unique(train_label))


xgb_params <- list("objective" = "binary:logistic",
                  "eval_metric" = "auc")

watchlist <- list(train = train_matrix, test = test_matrix)
```

``` r
# bst_model <- xgb.train(params = xgb_params,
#                        data = train_matrix,
#                        nrounds = 100,
#                        watchlist = watchlist,
#                        eta = 0.02,
#                        max.depth = 15,
#                        gamma = 0,
#                        subsample = 0.7,
#                        missing = NA,
#                        seed = 1234
#                        )

# 
# scale_pos_weight <- 48909 / 5674
# scale_pos_weight


bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 200,
                       watchlist = watchlist,
                       scale_pos_weight = 0.7,
                       eta = 0.075,
                       max.depth = 10,
                       gamma = 7,
                       colsample_bytree = .9,
                       subsample = 0.7,
                       missing = NA,
                       seed = 1234
                       )
```

    ## [1]  train-auc:0.929110  test-auc:0.920954 
    ## [2]  train-auc:0.954766  test-auc:0.945466 
    ## [3]  train-auc:0.963115  test-auc:0.956133 
    ## [4]  train-auc:0.965606  test-auc:0.958621 
    ## [5]  train-auc:0.966267  test-auc:0.959363 
    ## [6]  train-auc:0.967195  test-auc:0.960232 
    ## [7]  train-auc:0.967653  test-auc:0.961024 
    ## [8]  train-auc:0.967403  test-auc:0.960440 
    ## [9]  train-auc:0.967701  test-auc:0.960646 
    ## [10] train-auc:0.968146  test-auc:0.961090 
    ## [11] train-auc:0.968330  test-auc:0.961349 
    ## [12] train-auc:0.968706  test-auc:0.961676 
    ## [13] train-auc:0.968657  test-auc:0.961430 
    ## [14] train-auc:0.969100  test-auc:0.961789 
    ## [15] train-auc:0.969489  test-auc:0.962045 
    ## [16] train-auc:0.969839  test-auc:0.962302 
    ## [17] train-auc:0.970070  test-auc:0.962527 
    ## [18] train-auc:0.970444  test-auc:0.962559 
    ## [19] train-auc:0.970599  test-auc:0.962617 
    ## [20] train-auc:0.970605  test-auc:0.962556 
    ## [21] train-auc:0.970763  test-auc:0.962710 
    ## [22] train-auc:0.970898  test-auc:0.962674 
    ## [23] train-auc:0.971098  test-auc:0.962768 
    ## [24] train-auc:0.971264  test-auc:0.962903 
    ## [25] train-auc:0.971165  test-auc:0.962796 
    ## [26] train-auc:0.971455  test-auc:0.963059 
    ## [27] train-auc:0.971624  test-auc:0.963234 
    ## [28] train-auc:0.971628  test-auc:0.963204 
    ## [29] train-auc:0.971960  test-auc:0.963475 
    ## [30] train-auc:0.972067  test-auc:0.963572 
    ## [31] train-auc:0.972121  test-auc:0.963626 
    ## [32] train-auc:0.972454  test-auc:0.963826 
    ## [33] train-auc:0.972501  test-auc:0.963888 
    ## [34] train-auc:0.972729  test-auc:0.964152 
    ## [35] train-auc:0.972779  test-auc:0.964207 
    ## [36] train-auc:0.972946  test-auc:0.964304 
    ## [37] train-auc:0.973032  test-auc:0.964267 
    ## [38] train-auc:0.973129  test-auc:0.964290 
    ## [39] train-auc:0.973156  test-auc:0.964257 
    ## [40] train-auc:0.973240  test-auc:0.964290 
    ## [41] train-auc:0.973340  test-auc:0.964353 
    ## [42] train-auc:0.973444  test-auc:0.964440 
    ## [43] train-auc:0.973656  test-auc:0.964659 
    ## [44] train-auc:0.973923  test-auc:0.964916 
    ## [45] train-auc:0.974014  test-auc:0.964909 
    ## [46] train-auc:0.974145  test-auc:0.965027 
    ## [47] train-auc:0.974213  test-auc:0.965053 
    ## [48] train-auc:0.974235  test-auc:0.965090 
    ## [49] train-auc:0.974334  test-auc:0.965095 
    ## [50] train-auc:0.974354  test-auc:0.965106 
    ## [51] train-auc:0.974420  test-auc:0.965106 
    ## [52] train-auc:0.974450  test-auc:0.965098 
    ## [53] train-auc:0.974517  test-auc:0.965097 
    ## [54] train-auc:0.974569  test-auc:0.965156 
    ## [55] train-auc:0.974775  test-auc:0.965200 
    ## [56] train-auc:0.974911  test-auc:0.965255 
    ## [57] train-auc:0.975015  test-auc:0.965277 
    ## [58] train-auc:0.975066  test-auc:0.965324 
    ## [59] train-auc:0.975162  test-auc:0.965347 
    ## [60] train-auc:0.975219  test-auc:0.965401 
    ## [61] train-auc:0.975240  test-auc:0.965414 
    ## [62] train-auc:0.975347  test-auc:0.965482 
    ## [63] train-auc:0.975382  test-auc:0.965531 
    ## [64] train-auc:0.975466  test-auc:0.965549 
    ## [65] train-auc:0.975606  test-auc:0.965535 
    ## [66] train-auc:0.975651  test-auc:0.965600 
    ## [67] train-auc:0.975711  test-auc:0.965667 
    ## [68] train-auc:0.975754  test-auc:0.965708 
    ## [69] train-auc:0.975887  test-auc:0.965745 
    ## [70] train-auc:0.976023  test-auc:0.965822 
    ## [71] train-auc:0.976065  test-auc:0.965821 
    ## [72] train-auc:0.976118  test-auc:0.965851 
    ## [73] train-auc:0.976118  test-auc:0.965851 
    ## [74] train-auc:0.976210  test-auc:0.965973 
    ## [75] train-auc:0.976283  test-auc:0.965940 
    ## [76] train-auc:0.976350  test-auc:0.965950 
    ## [77] train-auc:0.976372  test-auc:0.965952 
    ## [78] train-auc:0.976488  test-auc:0.965991 
    ## [79] train-auc:0.976518  test-auc:0.965988 
    ## [80] train-auc:0.976590  test-auc:0.966011 
    ## [81] train-auc:0.976668  test-auc:0.966131 
    ## [82] train-auc:0.976728  test-auc:0.966138 
    ## [83] train-auc:0.976779  test-auc:0.966115 
    ## [84] train-auc:0.976883  test-auc:0.966156 
    ## [85] train-auc:0.977008  test-auc:0.966250 
    ## [86] train-auc:0.977056  test-auc:0.966290 
    ## [87] train-auc:0.977133  test-auc:0.966277 
    ## [88] train-auc:0.977199  test-auc:0.966280 
    ## [89] train-auc:0.977279  test-auc:0.966308 
    ## [90] train-auc:0.977333  test-auc:0.966323 
    ## [91] train-auc:0.977395  test-auc:0.966302 
    ## [92] train-auc:0.977402  test-auc:0.966304 
    ## [93] train-auc:0.977442  test-auc:0.966327 
    ## [94] train-auc:0.977528  test-auc:0.966386 
    ## [95] train-auc:0.977528  test-auc:0.966386 
    ## [96] train-auc:0.977573  test-auc:0.966403 
    ## [97] train-auc:0.977611  test-auc:0.966405 
    ## [98] train-auc:0.977640  test-auc:0.966405 
    ## [99] train-auc:0.977640  test-auc:0.966405 
    ## [100]    train-auc:0.977700  test-auc:0.966412 
    ## [101]    train-auc:0.977700  test-auc:0.966412 
    ## [102]    train-auc:0.977778  test-auc:0.966410 
    ## [103]    train-auc:0.977861  test-auc:0.966425 
    ## [104]    train-auc:0.977888  test-auc:0.966418 
    ## [105]    train-auc:0.977888  test-auc:0.966418 
    ## [106]    train-auc:0.977943  test-auc:0.966430 
    ## [107]    train-auc:0.977995  test-auc:0.966471 
    ## [108]    train-auc:0.977994  test-auc:0.966490 
    ## [109]    train-auc:0.978056  test-auc:0.966485 
    ## [110]    train-auc:0.978067  test-auc:0.966490 
    ## [111]    train-auc:0.978159  test-auc:0.966529 
    ## [112]    train-auc:0.978228  test-auc:0.966534 
    ## [113]    train-auc:0.978244  test-auc:0.966525 
    ## [114]    train-auc:0.978305  test-auc:0.966542 
    ## [115]    train-auc:0.978401  test-auc:0.966558 
    ## [116]    train-auc:0.978435  test-auc:0.966563 
    ## [117]    train-auc:0.978479  test-auc:0.966569 
    ## [118]    train-auc:0.978530  test-auc:0.966576 
    ## [119]    train-auc:0.978611  test-auc:0.966573 
    ## [120]    train-auc:0.978611  test-auc:0.966573 
    ## [121]    train-auc:0.978611  test-auc:0.966573 
    ## [122]    train-auc:0.978611  test-auc:0.966573 
    ## [123]    train-auc:0.978637  test-auc:0.966599 
    ## [124]    train-auc:0.978639  test-auc:0.966612 
    ## [125]    train-auc:0.978639  test-auc:0.966612 
    ## [126]    train-auc:0.978639  test-auc:0.966612 
    ## [127]    train-auc:0.978721  test-auc:0.966608 
    ## [128]    train-auc:0.978726  test-auc:0.966607 
    ## [129]    train-auc:0.978769  test-auc:0.966614 
    ## [130]    train-auc:0.978787  test-auc:0.966610 
    ## [131]    train-auc:0.978828  test-auc:0.966611 
    ## [132]    train-auc:0.978858  test-auc:0.966606 
    ## [133]    train-auc:0.978888  test-auc:0.966602 
    ## [134]    train-auc:0.978922  test-auc:0.966584 
    ## [135]    train-auc:0.978970  test-auc:0.966662 
    ## [136]    train-auc:0.978996  test-auc:0.966670 
    ## [137]    train-auc:0.978996  test-auc:0.966670 
    ## [138]    train-auc:0.979023  test-auc:0.966657 
    ## [139]    train-auc:0.979051  test-auc:0.966661 
    ## [140]    train-auc:0.979080  test-auc:0.966649 
    ## [141]    train-auc:0.979112  test-auc:0.966675 
    ## [142]    train-auc:0.979136  test-auc:0.966663 
    ## [143]    train-auc:0.979216  test-auc:0.966703 
    ## [144]    train-auc:0.979371  test-auc:0.966714 
    ## [145]    train-auc:0.979371  test-auc:0.966714 
    ## [146]    train-auc:0.979414  test-auc:0.966708 
    ## [147]    train-auc:0.979447  test-auc:0.966705 
    ## [148]    train-auc:0.979464  test-auc:0.966707 
    ## [149]    train-auc:0.979507  test-auc:0.966713 
    ## [150]    train-auc:0.979524  test-auc:0.966709 
    ## [151]    train-auc:0.979539  test-auc:0.966717 
    ## [152]    train-auc:0.979577  test-auc:0.966726 
    ## [153]    train-auc:0.979621  test-auc:0.966745 
    ## [154]    train-auc:0.979656  test-auc:0.966730 
    ## [155]    train-auc:0.979691  test-auc:0.966732 
    ## [156]    train-auc:0.979721  test-auc:0.966720 
    ## [157]    train-auc:0.979780  test-auc:0.966783 
    ## [158]    train-auc:0.979890  test-auc:0.966795 
    ## [159]    train-auc:0.979890  test-auc:0.966795 
    ## [160]    train-auc:0.979936  test-auc:0.966803 
    ## [161]    train-auc:0.979971  test-auc:0.966807 
    ## [162]    train-auc:0.980011  test-auc:0.966808 
    ## [163]    train-auc:0.980011  test-auc:0.966808 
    ## [164]    train-auc:0.980063  test-auc:0.966808 
    ## [165]    train-auc:0.980157  test-auc:0.966799 
    ## [166]    train-auc:0.980206  test-auc:0.966790 
    ## [167]    train-auc:0.980261  test-auc:0.966778 
    ## [168]    train-auc:0.980316  test-auc:0.966810 
    ## [169]    train-auc:0.980338  test-auc:0.966817 
    ## [170]    train-auc:0.980347  test-auc:0.966808 
    ## [171]    train-auc:0.980347  test-auc:0.966808 
    ## [172]    train-auc:0.980364  test-auc:0.966803 
    ## [173]    train-auc:0.980384  test-auc:0.966802 
    ## [174]    train-auc:0.980406  test-auc:0.966806 
    ## [175]    train-auc:0.980470  test-auc:0.966803 
    ## [176]    train-auc:0.980470  test-auc:0.966803 
    ## [177]    train-auc:0.980482  test-auc:0.966794 
    ## [178]    train-auc:0.980537  test-auc:0.966787 
    ## [179]    train-auc:0.980537  test-auc:0.966787 
    ## [180]    train-auc:0.980606  test-auc:0.966773 
    ## [181]    train-auc:0.980682  test-auc:0.966758 
    ## [182]    train-auc:0.980705  test-auc:0.966763 
    ## [183]    train-auc:0.980759  test-auc:0.966766 
    ## [184]    train-auc:0.980800  test-auc:0.966761 
    ## [185]    train-auc:0.980815  test-auc:0.966762 
    ## [186]    train-auc:0.980815  test-auc:0.966762 
    ## [187]    train-auc:0.980841  test-auc:0.966752 
    ## [188]    train-auc:0.980870  test-auc:0.966751 
    ## [189]    train-auc:0.980943  test-auc:0.966754 
    ## [190]    train-auc:0.980943  test-auc:0.966754 
    ## [191]    train-auc:0.980980  test-auc:0.966759 
    ## [192]    train-auc:0.981017  test-auc:0.966756 
    ## [193]    train-auc:0.981030  test-auc:0.966762 
    ## [194]    train-auc:0.981078  test-auc:0.966758 
    ## [195]    train-auc:0.981109  test-auc:0.966761 
    ## [196]    train-auc:0.981109  test-auc:0.966761 
    ## [197]    train-auc:0.981150  test-auc:0.966759 
    ## [198]    train-auc:0.981159  test-auc:0.966772 
    ## [199]    train-auc:0.981212  test-auc:0.966773 
    ## [200]    train-auc:0.981212  test-auc:0.966773

``` r
# bst_model <- xgboost(data = train_matrix, label = train_label, nround = 50, objective = "binary:logistic", eval_metric = "auc")

# print(bst_model)


prd <- predict(bst_model, test_matrix)

head(prd)
```

    ## [1] 0.0004228782 0.7835159302 0.8222096562 0.0112904962 0.0044202749
    ## [6] 0.5325421095

``` r
e <- data.frame(bst_model$evaluation_log)

head(e)
```

    ##   iter train_auc test_auc
    ## 1    1  0.929110 0.920954
    ## 2    2  0.954766 0.945466
    ## 3    3  0.963115 0.956133
    ## 4    4  0.965606 0.958621
    ## 5    5  0.966267 0.959363
    ## 6    6  0.967195 0.960232

``` r
plot(e$iter, e$train_auc, col = "blue")
lines(e$iter, e$test_auc, col = "red")
```

![](xgboost_files/figure-gfm/xb-boost-create-matrix-3-1.png)<!-- -->

``` r
max(e$test_auc)
```

    ## [1] 0.966817

``` r
e[e$test_auc ==  0.966903, ]
```

    ## [1] iter      train_auc test_auc 
    ## <0 rows> (or 0-length row.names)

``` r
# # Feature importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)

# print(imp)

xgb.plot.importance(imp)
```

![](xgboost_files/figure-gfm/xb-boost-create-matrix-3-2.png)<!-- -->

``` r
p <- predict(bst_model, newdata = test_matrix)
# head(p)
pred <- matrix(p, nrow = 1, ncol = length(p) ) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
```

### Test

``` r
p <- as.factor(ifelse(p > 0.5, "1", "0"))

head(p)
```

    ## [1] 0 1 1 0 0 1
    ## Levels: 0 1

``` r
confusionMatrix(data=as.factor(p),  
                reference=as.factor(test_label), "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0 43749   415
    ##          1  5160  5259
    ##                                           
    ##                Accuracy : 0.8979          
    ##                  95% CI : (0.8953, 0.9004)
    ##     No Information Rate : 0.896           
    ##     P-Value [Acc > NIR] : 0.08331         
    ##                                           
    ##                   Kappa : 0.5997          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2e-16         
    ##                                           
    ##             Sensitivity : 0.92686         
    ##             Specificity : 0.89450         
    ##          Pos Pred Value : 0.50475         
    ##          Neg Pred Value : 0.99060         
    ##              Prevalence : 0.10395         
    ##          Detection Rate : 0.09635         
    ##    Detection Prevalence : 0.19088         
    ##       Balanced Accuracy : 0.91068         
    ##                                           
    ##        'Positive' Class : 1               
    ## 

### Area Under the Curve

``` r
# ?pROC

auc <- roc(as.integer(test_label), as.integer(p))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
# print(auc)

# plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC of Test Set:', round(auc$auc[[1]],2)))
# abline(h=1,col='green',lwd=2)
# abline(h=0,col='red',lwd=2)

g <- ggroc(auc, alpha = 0.5, colour = "red", linetype = 2, size = 2) +
  theme_minimal() + 
  ggtitle(paste('AUC of Test Set:', round(auc$auc[[1]],4))) + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype="dashed")
  

plot(g)
```

![](xgboost_files/figure-gfm/rf-auc-plot-test-1.png)<!-- -->

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
# model.rf.train <- randomForest(as.factor(top.10) ~ ., data = train, ntree = 275, mtry = 8, cutoff = c(0.36,1-0.36))
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
