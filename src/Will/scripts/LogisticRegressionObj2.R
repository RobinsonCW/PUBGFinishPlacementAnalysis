model.01 <- glm(top.10~boostCut + weaponCut + killCut + walkCut + damageCut + boostCut:killCut,
                  data= train, family = binomial(link="logit"))
summary(model.01)
m01.pred <- predict(model.01, newdata = test, type = "response")
plotRoc(m01.pred,test$top.10)
confusionMatrix(as.factor(as.numeric(m01.pred > 0.5)), test$top.10, positive = "1")
