model.01 <- glm(top.10~boosts + assists + weaponsAcquired + kills + walkDistance + boostCut*damageDealt,
                  data= train, family = binomial(link="logit"))
summary(model.01)
m01.pred <- predict(model.01, newdata = test, type = "response")
plotRoc(m01.pred,test$top.10)
confusionMatrix(as.factor(as.numeric(m01.pred > 0.5)), test$top.10, positive = "1")
