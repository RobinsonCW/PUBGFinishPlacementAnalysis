library(tidyverse) ## Data Wrangling


setwd("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis")

## Import the data. Downsampled training data and test data.
source("src/Will/scripts/DataPrep.R")


## Principle components

varsToDrop <- c("rankPoints","maxPlace","winPlacePerc")
moreDrop <- c("matchDuration","numGroups")
dataPCsolo <- solo %>% keep(is.numeric) %>% select(-varsToDrop,-moreDrop)

pc.result.solo <-prcomp(dataPCsolo,scale.=TRUE)
pc.scores.solo<-pc.result.solo$x
pc.scores.solo<-data.frame(pc.scores.solo)
pc.scores.solo$top.10 <- solo$top.10
pc.scores.solo$DurationCut <- solo$DurationCut
pc.scores.solo$kpCut <- solo$kpCut
pc.scores.solo$wpCut <- solo$wpCut
pc.scores.solo$rpCut <- solo$rpCut

pc.result.solo$rotation[,c(1:3)]
#Scree
eigenvals<-(pc.result.solo$sdev)^2
plot(1:length(eigenvals),eigenvals/sum(eigenvals),type="l",main="Scree Plot",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:length(eigenvals),cumulative.prop,lty=2)


plotPCkp <- function(mydata) {
  sampleData <- sample_frac(mydata, 0.01)
  ggplot(data = sampleData, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=kpCut), size=1, alpha = 0.5)+
    ggtitle("PCA of PUBG")
}
plotPC10 <- function(mydata) {
  sampleData <- sample_frac(mydata, 0.01)
  ggplot(data = sampleData, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=top.10), size=1, alpha = 0.5)+
    ggtitle("PCA of PUBG")
}

set.seed(314159)
plotPCkp(pc.scores.solo)
plotPC10(pc.scores.solo)


## Repeat the analysis without killPoints

dataPCsolo <- solo %>% keep(is.numeric) %>% select(-varsToDrop,-moreDrop,-c("killPoints"))

pc.result.solo <-prcomp(dataPCsolo,scale.=TRUE)
pc.scores.solo<-pc.result.solo$x
pc.scores.solo<-data.frame(pc.scores.solo)
pc.scores.solo$top.10 <- solo$top.10
pc.scores.solo$DurationCut <- solo$DurationCut
pc.scores.solo$kpCut <- solo$kpCut
pc.scores.solo$wpCut <- solo$wpCut
pc.scores.solo$rpCut <- solo$rpCut

pc.result.solo$rotation[,c(1:3)]
#Scree
eigenvals<-(pc.result.solo$sdev)^2
plot(1:length(eigenvals),eigenvals/sum(eigenvals),type="l",main="Scree Plot",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:length(eigenvals),cumulative.prop,lty=2)


plotPCkp <- function(mydata) {
  sampleData <- sample_frac(mydata, 0.01)
  ggplot(data = sampleData, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=kpCut), size=1, alpha = 0.5)+
    ggtitle("PCA of PUBG")
}
plotPC10 <- function(mydata) {
  sampleData <- sample_frac(mydata, 0.01)
  ggplot(data = sampleData, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=top.10), size=1, alpha = 0.5)+
    ggtitle("PCA of PUBG")
}

set.seed(314159)
plotPCkp(pc.scores.solo)
plotPC10(pc.scores.solo)
