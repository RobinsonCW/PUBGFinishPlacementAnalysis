library(tidyverse) ## Data Wrangling
library(glmnet) ## Logistic Regression
library(car) ## VIF
library(ROCR) ## ROC Curves
library(caret) ## Confusion

setwd("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis")

## Import the data. Downsampled training data and test data.
source("src/Will/scripts/DataPrep.R")

#trainDownSampled$walkDistance <- trainDownSampled$walkDistance/1000
#test$walkDistance <- test$walkDistance/1000
## These variables were identified in eda as not suitable for modeling.
varsToDrop <- c("Id","groupId","matchId","matchDuration","matchType",
                "maxPlace","numGroups","winPlacePerc")

## Removing the variables above from the train data
train <- trainDownSampled %>% select(-varsToDrop)

## helper functions
plotRoc <- function(preds, truth) {
  pred <- prediction(preds, truth)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  auc.train <- performance(pred, measure = "auc")
  auc.train <- auc.train@y.values
  
  #Plot ROC
  par(mar=c(4,4,4,4))
  plot(roc.perf,main="Ordinary Logistic")
  abline(a=0, b= 1) #Ref line indicating poor performance
  text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
  table(test$top.10, useNA = "ifany")
}
scoreMod <- function(model.out) {
  model.pred <- predict(model.out, newdata = test, type = "response")
  plotRoc(model.pred,test$top.10)
  confusionMatrix(as.factor(as.numeric(model.pred > 0.5)), test$top.10, positive = "1")
}

## Logistic Model 1 - "The Kitchen Sink"
model.main <- glm(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                    damageDealt + headshotKills + kills + killStreaks + roadKills + vehicleDestroys + 
                    killPlace + killPoints  + rankPoints + winPoints +
                    longestKill + swimDistance + rideDistance + walkDistance,
                  data= train, family = binomial(link="logit"))
summary(model.main)
broom::tidy(model.main)
vif(model.main) ## Kill points and win points super correlated
  
scoreMod(model.main)

## Next going to look at finding best lambda for lasso, then do a lasso search for variables
set.seed(1234)
x <- model.matrix(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                    damageDealt + headshotKills + kills + killStreaks + roadKills + vehicleDestroys + 
                    killPlace + killPoints  + rankPoints + winPoints +
                    longestKill + swimDistance + rideDistance + walkDistance, data=train)
cv.lasso <- cv.glmnet(x, train$top.10, alpha = 1, family = "binomial")
plot(cv.lasso)

cv.lasso$lambda.min
coef(cv.lasso, cv.lasso$lambda.min)
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.1se)


model.lasso <- glmnet(x, train$top.10, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.1se)

coef(model.lasso)

x.test <- model.matrix(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                    damageDealt + headshotKills + kills + killStreaks + roadKills + vehicleDestroys + 
                    killPlace + killPoints  + rankPoints + winPoints +
                    longestKill + swimDistance + rideDistance + walkDistance, data=test)

lasso.pred <- predict(model.lasso, newx = x.test)
plotRoc(lasso.pred,test$top.10)
confusionMatrix(as.factor(as.numeric(lasso.pred > 0.5)), test$top.10, positive = "1")

## Fitting final model using variables from lasso

model.final.lasso <- glm(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                    kills + killStreaks + roadKills + vehicleDestroys + 
                    killPlace + rankPoints + winPoints +
                    longestKill + swimDistance + rideDistance + walkDistance,
                  data= train, family = binomial(link="logit"))
summary(model.final.lasso)
vif(model.final.lasso) ## Kill points and win points super correlated
plot(model.final.lasso, which = 4, id.n = 3)
scoreMod(model.final.lasso)


model.data %>% top_n(3, .cooksd)

## trying to remove some of the influential points and correlated vars
  #No rankpoints

model.inf1 <- glm(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                           kills + killStreaks + roadKills + vehicleDestroys + 
                           killPlace + winPoints +
                           longestKill + swimDistance + rideDistance + walkDistance,
                         data= train, family = binomial(link="logit"))
summary(model.inf1)
vif(model.inf1)
##winpoints no longer significant  
#No winpoints
model.inf2 <- glm(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                    kills + killStreaks + roadKills + vehicleDestroys + 
                    killPlace + rankPoints +
                    longestKill + swimDistance + rideDistance + walkDistance,
                  data= train, family = binomial(link="logit"))
summary(model.inf2)
  vif(model.inf2)
##rankpoints not significant

##neither

model.inf2a <- glm(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                    kills + vehicleDestroys + killPlace + killStreaks + roadKills + swimDistance +
                    longestKill + rideDistance + walkDistance,
                  data= train, family = binomial(link="logit"))
summary(model.inf2a)
vif(model.inf2a)

plot(model.inf2a, which = 4, id.n = 3)

## removing killStreaks + roadKills + swimDistance
model.inf3 <- glm(top.10~assists + boosts + heals + teamKills + weaponsAcquired +
                  kills + vehicleDestroys + killPlace + 
                  longestKill + rideDistance + walkDistance,
                data= train, family = binomial(link="logit"))
summary(model.inf3)
vif(model.inf3)
  
plot(model.inf3, which = 4, id.n = 3)

scoreMod(model.inf3)

#assists + boosts + heals + weaponsAcquired + kills + walkDistance
model.inf4 <- glm(top.10~assists + boosts + heals + weaponsAcquired + killPlace + walkDistance ,
                  data= train, family = binomial(link="logit"))
summary(model.inf4)
vif(model.inf4)
plot(model.inf4, which = 4, id.n = 3)

scoreMod(model.inf4)
