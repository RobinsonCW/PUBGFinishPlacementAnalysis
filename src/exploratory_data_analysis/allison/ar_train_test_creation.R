library(caret)
library(dplyr)

train <- read.csv("C:/Users/Allison/Desktop/SMU/19F/Stats2/Proj2/pubg-finish-placement-prediction/train_V2.csv", stringsAsFactors=FALSE)

train$top.10 <- ifelse(train$winPlacePerc > .9, 1, 0)
train$top.10 <- as.factor(train$top.10)

solo = train[which(train$matchType=="solo"),]

set.seed(1234)
indxTrain <- createDataPartition(y = solo$top.10,p = 0.7,list = FALSE)
train1 <- solo[indxTrain,]
test <- solo[-indxTrain,]

solo.down.samp <- downSample(train1,train1$top.10)

#Just a sanity check that I did this right
solo.down.samp %>% group_by(top.10) %>%
  dplyr::summarize(n=n()) %>%
  mutate(freq = n / sum(n))
# # A tibble: 2 x 3
# top.10     n  freq
# <fct>  <int> <dbl>
#   1 0      13307   0.5
#   2 1      13307   0.5

test %>% group_by(top.10) %>%
  dplyr::summarize(n=n()) %>%
  mutate(freq = n / sum(n))
# # A tibble: 2 x 3
# top.10     n  freq
# <fct>  <int> <dbl>
#   1 0      48879 0.896
#   2 1       5703 0.104

write.csv(solo.down.samp,
          "C:/Users/Allison/Desktop/SMU/19F/Stats2/Proj2/pubg-finish-placement-prediction/down sampled train.csv")
write.csv(test,
          "C:/Users/Allison/Desktop/SMU/19F/Stats2/Proj2/pubg-finish-placement-prediction/NOT down sampled test.csv")

