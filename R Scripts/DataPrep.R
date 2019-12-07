## Data Preparation 
library(tidyverse)
setwd("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis")
solo <- read_csv("data/pubg_solo_game_types.csv") %>% select(-c("DBNOs","revives"))
trainDownSampled <- read_csv("data/pubg_solo_game_types_train_downsampled.csv") %>% select(-c("DBNOs","revives"))
test <- read_csv("data/pubg_solo_game_types_test_full.csv") %>% select(-c("DBNOs","revives"))

solo$top.10 <- as.factor(solo$top.10)
test$top.10 <- as.factor(solo$top.10)
trainDownSampled$top.10 <- as.factor(solo$top.10)

solo <- solo %>% mutate(
DurationCut = as.factor(ifelse(matchDuration > 1600,1,0)),
kpCut = as.factor(ifelse(killPoints > 0,1,0)),
wpCut = as.factor(ifelse(winPoints > 0,1,0)),
rpCut = as.factor(ifelse(rankPoints > -1,1,0)),
boostCut = as.factor(ifelse(boosts >= 2,1,0)),
healCut = as.factor(ifelse(heals >= 2,1,0)),
walkCut = as.factor(ifelse(walkDistance >= 1250,1,0)),
weaponCut = as.factor(ifelse(weaponsAcquired >= 3,1,0)),
killCut = as.factor(ifelse(kills >= 2,1,0)),
assistCut = as.factor(ifelse(assists >= 2,1,0)),
damageCut = as.factor(ifelse(damageDealt >= 250,1,0)),
streakCut = as.factor(ifelse(killStreaks >= 1,1,0)),
killsPK = kills/(walkDistance+1/1000),
damageKill = kills/(damageDealt+1)
)

test <- test %>% mutate(
  DurationCut = as.factor(ifelse(matchDuration > 1600,1,0)),
  kpCut = as.factor(ifelse(killPoints > 0,1,0)),
  wpCut = as.factor(ifelse(winPoints > 0,1,0)),
  rpCut = as.factor(ifelse(rankPoints > -1,1,0)),
  boostCut = as.factor(ifelse(boosts >= 2,1,0)),
  healCut = as.factor(ifelse(heals >= 2,1,0)),
  walkCut = as.factor(ifelse(walkDistance >= 1250,1,0)),
  weaponCut = as.factor(ifelse(weaponsAcquired >= 3,1,0)),
  killCut = as.factor(ifelse(kills >= 2,1,0)),
  assistCut = as.factor(ifelse(assists >= 2,1,0)),
  damageCut = as.factor(ifelse(damageDealt >= 250,1,0)),
  streakCut = as.factor(ifelse(killStreaks >= 1,1,0)),
  killsPK = kills/(walkDistance+1/1000),
  damageKill = kills/(damageDealt+1)
)

trainDownSampled <- trainDownSampled %>% mutate(
  DurationCut = as.factor(ifelse(matchDuration > 1600,1,0)),
  kpCut = as.factor(ifelse(killPoints > 0,1,0)),
  wpCut = as.factor(ifelse(winPoints > 0,1,0)),
  rpCut = as.factor(ifelse(rankPoints > -1,1,0)),
  boostCut = as.factor(ifelse(boosts >= 2,1,0)),
  healCut = as.factor(ifelse(heals >= 2,1,0)),
  walkCut = as.factor(ifelse(walkDistance >= 1250,1,0)),
  weaponCut = as.factor(ifelse(weaponsAcquired >= 3,1,0)),
  killCut = as.factor(ifelse(kills >= 2,1,0)),
  assistCut = as.factor(ifelse(assists >= 2,1,0)),
  damageCut = as.factor(ifelse(damageDealt >= 250,1,0)),
  streakCut = as.factor(ifelse(killStreaks >= 1,1,0)),
  killsPK = kills/(walkDistance+1/1000),
  damageKill = kills/(damageDealt+1)
)
varGroupKill <- c("damageDealt","headshotKills","kills","killStreaks","roadKills","vehicleDestroys","weaponsAcquired")
varGroupTeam <- c("assists","boosts","heals","teamKills")
varGroupDistance <- c("longestKill","rideDistance","swimDistance","walkDistance")
varGroupPoints <- c("killPoints","winPoints","rankPoints","winPlacePerc","matchDuration")


