library(tidyverse)
library(corrplot)
library(GGally)



setwd("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis")

## Import the data. Downsampled training data and test data.
source("src/Will/scripts/DataPrep.R")

## Correlation plot
varsToDrop <- c("Id","matchId","matchType","rankPoints","groupId")
dataPlot <- solo %>% select(-varsToDrop)
cors <- cor(dataPlot %>% keep(is.numeric) , use = "pairwise.complete.obs")
corrplot(cors, method="circle")

## Binned Plots, too many points to just use scatter plots
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


ggplot(solo,aes(x=top.10, y=walkDistance/1000, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=swimDistance, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=rideDistance, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=longestKill, fill=top.10)) + geom_boxplot()

ggplot(solo,aes(x=top.10, y=killsPK, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=damageKill, fill=top.10)) + geom_boxplot()

ggplot(solo,aes(x=top.10, y=heals, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=kills, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=damageDealt, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=headshotKills, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=killPoints, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=winPoints, fill=top.10)) + geom_boxplot()
ggplot(solo %>% filter(killPoints == 0),aes(x=top.10, y=killPoints, fill=top.10)) + geom_boxplot()
ggplot(solo %>% filter(winPoints > 0),aes(x=top.10, y=winPoints, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=killStreaks, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=weaponsAcquired, fill=top.10)) + geom_boxplot()
ggplot(solo,aes(x=top.10, y=vehicleDestroys, fill=top.10)) + geom_boxplot()

ggplot(solo,aes(x=winPoints)) + geom_histogram()
ggplot(solo %>% filter(rankPoints > 0),aes(x=rankPoints)) + geom_histogram()
## Looking at some matchduration items
ggplot(solo, aes(x = matchDuration, group = kpCut, fill = kpCut)) +
  geom_histogram(colour = "gray", binwidth = 20, position =
                   "dodge") +
  theme_bw()

dur <- solo %>% group_by(matchId) %>% summarise(matchDur2 = max(matchDuration))
ggplot(dur, aes(x = matchDur2/60)) + geom_histogram(binwidth = 1) + labs(title="Histogram of PUBG Match Durations",x="Duration in Minutes") 

## Pairs plot

set.seed(1234)
sample <- sample_frac(solo, 0.01) 
pairsData <- sample %>% keep(is.numeric)
pairsData$top.10 <- sample$top.10

ggpairs(data = pairsData, mapping = aes(color = top.10),
        columns = c("assists" , "boosts" , "heals" , "weaponsAcquired" , "killPlace" , "walkDistance","killsPK","damageKill","top.10"))
ggpairs(data = pairsData, mapping = aes(color = top.10),
        columns = c(varGroupPoints,"top.10"))
ggpairs(data = pairsData, mapping = aes(color = top.10),
        columns = c(varGroupDistance,"top.10"))
ggpairs(data = pairsData, mapping = aes(color = top.10),
        columns = c(varGroupKill,"top.10"))
ggpairs(data = pairsData, mapping = aes(color = top.10),
        columns = c(varGroupTeam,"top.10"))




##Enormous Plot
pairsPlot <- ggpairs(data = pairsData, mapping = aes(color = top.10))
ggsave(filename = "BFP.png", pairsPlot,
       width = 10, height = 8, dpi = 300, units = "in", device='png')
