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

## Data prep
source("src/Will/scripts/DataPrep.R")
varsToDrop <- c("Id","groupId","matchId","matchDuration","matchType",
                "maxPlace","numGroups","winPlacePerc")
train <- trainDownSampled %>% select(-varsToDrop)


## Modeling
model.01 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + boostCut:killPlace ,
                  data= train, family = binomial(link="logit"))
summary(model.01)
scoreMod(model.01)

model.02 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + kpCut:killPlace,
                data= train, family = binomial(link="logit"))
summary(model.02)
scoreMod(model.02)

model.03 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + walkDistance*killPlace ,
                data= train, family = binomial(link="logit"))
summary(model.03)
scoreMod(model.03)

model.04 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + walkBin ,
                data= train, family = binomial(link="logit"))
summary(model.04)
scoreMod(model.04)

model.05 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + kpCut +  kpCut:killPlace,
                data= train, family = binomial(link="logit"))
summary(model.05)
scoreMod(model.05)

model.06 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + damageCut:streakCut,
                data= train, family = binomial(link="logit"))
summary(model.06)
scoreMod(model.06)

model.07 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + killsPK,
                data= train, family = binomial(link="logit"))
summary(model.07)
scoreMod(model.07)

model.08 <- glm(top.10~boosts + assists + weaponsAcquired + killPlace + walkDistance + damageKill + killsPK,
                data= train, family = binomial(link="logit"))
summary(model.08)
scoreMod(model.08)

# Model 08 AUC = 0.936, Sens= 0.894, Spec 0.838, errate=0.156, Error=fitted probabilities numerically 0 or 1 occurred 
# Model 07 AUC = 0.936, Sens= 0.895, Spec 0.840, errate=0.154, Error=No
# Model 06 AUC = 0.939, Sens= 0.888, Spec 0.836, errate=0.158, Error=prediction from a rank-deficient fit may be misleading
# Model 05 AUC = 0.932, Sens=0.864 , Spec 0.841, errate=0.156, Error=
# Model 04 AUC = 0.936, Sens= 0.889, Spec 0.827, errate=0.166, Error=
# Model 03 AUC = 0.932, Sens= 0.885, Spec 0.832, errate=0.162, Error=
# Model 02 AUC = 0.932, Sens= 0.862, Spec 0.841, errate=0.157, Error=
# Model 01 AUC = 0.932, Sens= 0.873, Spec 0.836, errate=0.160, Error=