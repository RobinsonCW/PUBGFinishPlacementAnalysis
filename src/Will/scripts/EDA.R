library(knitr)
library(rmdformats)
library(tidyverse)
library(kableExtra)
library(gridExtra)
library(doParallel)
library(naniar)
library(dlookr)
library(formattable)
library(GGally)
library(doParallel)

workers <- makeCluster(8L)
registerDoParallel(workers)
## Read in the data

train_V2 <- read_csv("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis/data/Will/train_V2.csv")
#test_V2 <- read_csv("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis/data/test_V2.csv")
#sample_submission_V2 <- read_csv("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis/data/sample_submission_V2.csv")

#Create Response
train_V2$Top10 <- as.factor(ifelse(train_V2$winPlacePerc > 0.9,1,0))
train_V2$DurationCut <- as.factor(ifelse(train_V2$matchDuration > 1600,1,0))
train_V2$kpCut <- as.factor(ifelse(train_V2$killPoints > 0,1,0))
train_V2$wpCut <- as.factor(ifelse(train_V2$winPoints > 0,1,0))
train_V2$rpCut <- as.factor(ifelse(train_V2$rankPoints > -1,1,0))
table(train_V2$rpCut)
#Solo only
solo <- train_V2 %>% filter(matchType == "solo")
write_csv(solo,"C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis/data/soloOutSAS.csv")
##Sumary by match type
train_V2 %>% group_by(matchType) %>% summarise(Unique_matches = n_distinct(matchId), obs = n())
hist(train_V2$winPlacePerc)


ggplot(solo,aes(x=walkDistance, y=winPlacePerc)) +geom_hex()
ggplot(solo,aes(x=swimDistance, y=winPlacePerc)) +geom_hex()
ggplot(solo,aes(x=rideDistance, y=winPlacePerc)) +geom_hex()
ggplot(solo,aes(x=longestKill, y=winPlacePerc)) +geom_hex()

ggplot(solo,aes(x=heals, y=winPlacePerc)) +geom_bin2d()
ggplot(solo,aes(x=heals, y=winPlacePerc)) +geom_bin2d()
ggplot(solo,aes(x=kills, y=winPlacePerc)) +geom_hex()
ggplot(solo,aes(x=damageDealt, y=winPlacePerc)) +geom_hex()
ggplot(solo,aes(x=headshotKills, y=winPlacePerc)) +geom_hex()
ggplot(solo,aes(x=killStreaks, y=winPlacePerc)) +geom_hex()
ggplot(solo,aes(x=DBNOs, y=winPlacePerc)) +geom_hex()


ggplot(solo,aes(x=Top10, y=walkDistance/1000, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=swimDistance, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=rideDistance, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=longestKill, fill=Top10)) + geom_boxplot()

ggplot(solo,aes(x=Top10, y=heals, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=kills, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=damageDealt, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=headshotKills, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=killPoints, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=winPoints, fill=Top10)) + geom_boxplot()
ggplot(solo %>% filter(killPoints == 0),aes(x=Top10, y=killPoints, fill=Top10)) + geom_boxplot()
ggplot(solo %>% filter(winPoints > 0),aes(x=Top10, y=winPoints, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=killStreaks, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=DBNOs, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=weaponsAcquired, fill=Top10)) + geom_boxplot()
ggplot(solo,aes(x=Top10, y=vehicleDestroys, fill=Top10)) + geom_boxplot()

ggplot(solo, aes(x = matchDuration, group = kpCut, fill = kpCut)) +
  geom_histogram(colour = "gray", binwidth = 20, position =
                   "dodge") +
  theme_bw()

dur <- solo %>% group_by(matchId) %>% summarise(matchDur2 = max(matchDuration))
ggplot(dur, aes(x = matchDur2/60)) + geom_histogram(binwidth = 1) + labs(title="Histogram of PUBG Match Durations",x="Duration in Minutes") 

  

## Check for missing data
 

## Descriptive Stats for train_V2
formattable(describe(train_V2))
formattable(describe(solo))
## Principle components

varsToDrop <- c("Id","matchId","matchType","rankPoints","groupId","maxPlace","winPlacePerc","Top10","DurationCut","kpCut","wpCut","rpCut")
varsDropSolo <- c("DBNOs","revives")
moreDrop <- c("matchDuration","numGroups")
dataPC <- train_V2 %>% select(-varsToDrop)
dataPCsolo <- solo %>% select(-varsToDrop, -varsDropSolo,-moreDrop)

pc.result<-prcomp(dataPC,scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$Top10 <- train_V2$Top10
pc.scores$DurationCut <- train_V2$DurationCut

pc.result.solo <-prcomp(dataPCsolo,scale.=TRUE)
pc.scores.solo<-pc.result.solo$x
pc.scores.solo<-data.frame(pc.scores.solo)
pc.scores.solo$Top10 <- solo$Top10
pc.scores.solo$DurationCut <- solo$DurationCut
pc.scores.solo$kpCut <- solo$kpCut
pc.scores.solo$wpCut <- solo$wpCut
pc.scores.solo$rpCut <- solo$rpCut

pc.result.solo$rotation[,c(1:3)]
#Scree
eigenvals<-(pc.result.solo$sdev)^2
plot(1:18,eigenvals/sum(eigenvals),type="l",main="Scree Plot",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:18,cumulative.prop,lty=2)


plotPCkp <- function(mydata) {
  sampleData <- sample_frac(mydata, 0.01)
  ggplot(data = sampleData, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=kpCut), size=1, alpha = 0.5)+
    ggtitle("PCA of PUBG")
}
plotPC10 <- function(mydata) {
  sampleData <- sample_frac(mydata, 0.01)
  ggplot(data = sampleData, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=Top10), size=1, alpha = 0.5)+
    ggtitle("PCA of PUBG")
}

hist(train_V2$matchDuration)

set.seed(314159)
plotPC(pc.scores)

set.seed(314159)
plotPCkp(pc.scores.solo)
plotPC10(pc.scores.solo)

ggplot(data = pc.scores.solo, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=Top10), size=1)+
  ggtitle("PCA of PUBG")


varsToDrop <- c("Id","matchId","matchType","rankPoints","groupId")

dataPlot <- solo %>% select(-varsToDrop)

my_bin <- function(data, mapping, ..., low = "#132B43", high = "#56B1F7") {
  ggplot(data = data, mapping = mapping) +
    geom_bin2d(...) +
    scale_fill_gradient(low = low, high = high)
}

pm <- ggpairs(
  dataPlot, columns = varGroupTeam,
  diag = list(mapping = aes(color = Top10)),
  lower = list(
    continuous = my_bin
    )
)
pm

ggpairs(data = dataPC,   lower = list(
  mapping = aes(color = Top10)
))

library(corrplot)
cors <- cor(dataPlot %>% keep(is.numeric) %>% select(-c("DBNOs","revives")), use = "pairwise.complete.obs")
corrplot(cors, method="circle")

solo %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()

varGroupKill <- c("damageDealt","headshotKills","kills","killStreaks","DBNOs","roadKills","vehicleDestroys","weaponsAcquired")
varGroupTeam <- c("assists","boosts","heals","revives","teamKills")
varGroupDistance <- c("longestKill","rideDistance","swimDistance","walkDistance")
varGroupPoints <- c("killPoints","winPoints","rankPoints","winPlacePerc","matchDuration")

options(scipen = 999)
formattable(describe(train_V2 %>% select(varGroupKill)))
formattable(describe(train_V2 %>% select(varGroupTeam)))
formattable(describe(train_V2 %>% select(varGroupDistance)))
formattable(describe(train_V2 %>% select(varGroupPoints)))
formattable(describe(solo %>% select(varGroupPoints)))

formattable(describe(solo %>% select(varGroupKill)))

test  <- train_V2 %>% filter(damageDealt >= 1000)

hist(solo$killPoints)



##Logistic Model
model.null<-glm(Top10 ~ 1, data=solo,family = binomial(link="logit"))
model.main<-glm(Top10 ~ kills + boosts + walkDistance, data=solo ,family = binomial(link="logit"))
summary(model.main)

step(model.null,
     scope = list(upper=model.main),
     direction="forward",
     test="Chisq",
     data=solo)


t(aggregate(heals~Top10,data=solo,summary))
t(aggregate(walkDistance~Top10,data=solo,summary))
