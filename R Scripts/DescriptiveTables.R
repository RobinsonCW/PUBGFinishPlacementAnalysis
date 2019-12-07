library(formattable)
library(dlookr)
formattable(describe(solo))

## good for quick bivariate tables
categ <- target_by(solo, top.10)
columnsWant <- c("n","mean","sd","p00","p05","p10","p25","p50","p75", "p95", "p99","p100")

## Basically running summary stats cut by top 10. Generally the format is
## First row not top 10
## Second row top 10
## Third row total

relate(categ, walkDistance) %>% select(columnsWant)
relate(categ, kills) %>% select(columnsWant)
relate(categ, killPoints) %>% select(columnsWant)
varGroupKill <- c("damageDealt","headshotKills","kills","killStreaks","DBNOs","roadKills","vehicleDestroys","weaponsAcquired")
## Kills
relate(categ, damageDealt) %>% select(columnsWant)
relate(categ, headshotKills) %>% select(columnsWant)
relate(categ, kills) %>% select(columnsWant)
relate(categ, killStreaks) %>% select(columnsWant)
relate(categ, roadKills) %>% select(columnsWant)
relate(categ, vehicleDestroys) %>% select(columnsWant)
relate(categ, weaponsAcquired) %>% select(columnsWant)


## Distance
relate(categ, longestKill) %>% select(columnsWant)
relate(categ, rideDistance) %>% select(columnsWant)
relate(categ, swimDistance) %>% select(columnsWant)
relate(categ, walkDistance) %>% select(columnsWant)

##
relate(categ, heals) %>% select(columnsWant)
relate(categ, boosts) %>% select(columnsWant)
relate(categ, assists) %>% select(columnsWant)

relate(categ, killsPK) %>% select(columnsWant) #kills per kilometer
relate(categ, damageKill) %>% select(columnsWant)
## RankPoints
summary(solo %>% filter(rankPoints > 0) %>% select(rankPoints))
rpTest <- solo %>% filter(rankPoints > 0)
categrp <- target_by(rpTest, top.10)
relate(categrp, rankPoints) %>% select(columnsWant)

## winPoints
summary(solo %>% filter(winPoints > 0) %>% select(winPoints))
wpTest <- solo %>% filter(winPoints > 0)
categwp <- target_by(wpTest, top.10)
relate(categwp, winPoints) %>% select(columnsWant)


## Can we combine? Seems no, when winpoints >0, rankpoints = -1
solo$test1 <- solo$winPoints+solo$rankPoints
solo$test2 <- solo$winPoints*solo$rankPoints
relate(categ, test1) %>% select(columnsWant)
relate(categ, test2) %>% select(columnsWant)

##
table(solo$assists,solo$top.10)
