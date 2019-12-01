# Dataset Readme

## Beers

Column Name      |Type           |Description
-----------------|---------------|-------------------------------------------------
DBNOs            |               |Number of enemy players knocked.
assists          |               |Number of enemy players this player damaged that were killed by teammates.
boosts           |               |Number of boost items used.
damageDealt      |               |Total damage dealt. Note: Self inflicted damage is subtracted.
headshotKills    |               |Number of enemy players killed with headshots.
heals            |               |Number of healing items used.
Id               |               |Player’s Id
killPlace        |               |Ranking in match of number of enemy players killed.
killPoints       |               |Kills-based external ranking of player.
killStreaks      |               |Max number of enemy players killed in a short amount of time.
kills            |               |Number of enemy players killed.
longestKill      |               |Longest distance between player and player killed at time of death. 
matchDuration    |               |Duration of match in seconds.
matchId          |               |ID to identify match. There are no matches that are in both the training and testing set.
matchType        |               |String identifying the game mode that the data comes from. 
rankPoints       |               |Elo-like ranking of player. 
revives          |               |Number of times this player revived teammates.
rideDistance     |               |Total distance traveled in vehicles measured in meters.
roadKills        |               |Number of kills while in a vehicle.
swimDistance     |               |Total distance traveled by swimming measured in meters.
teamKills        |               |Number of times this player killed a teammate.
vehicleDestroys  |               |Number of vehicles destroyed.
walkDistance     |               |Total distance traveled on foot measured in meters.
weaponsAcquired  |               |Number of weapons picked up.
winPoints        |               |Win-based external ranking of player. 
groupId          |               |ID to identify a group within a match. If the same group of players plays in different matches, they will have a different groupId each time.
numGroups        |               |Number of groups we have data for in the match.
maxPlace         |               |Worst placement we have data for in the match. This may not match with numGroups, as sometimes the data skips over placements.
winPlacePerc     |               |This is a percentile winning placement, where 1 corresponds to 1st place, and 0 corresponds to last place in the match. (to be removed from our binomial classfier)
top.10           |               |The target of prediction. This is a percentile winning placement, where as Yes corresponds to a top 10% placement and below that in the lower 90%.