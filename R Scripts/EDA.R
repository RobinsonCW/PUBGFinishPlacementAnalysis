library(knitr)
library(rmdformats)
library(tidyverse)
library(kableExtra)
library(gridExtra)
library(doParallel)
library(naniar)
library(dlookr)
library(formattable)

## Read in the data

train_V2 <- read_csv("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis/data/train_V2.csv")
test_V2 <- read_csv("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis/data/test_V2.csv")
sample_submission_V2 <- read_csv("C:/Users/William/OneDrive/MSDS_6372_AppliedStatistics/project2/PUBGFinishPlacementAnalysis/data/sample_submission_V2.csv")

##Sumary by match type
train_V2 %>% group_by(matchType) %>% summarise(Unique_matches = n_distinct(matchId), obs = n())
hist(train_V2$winPlacePerc)

## Create top 10 Variable
train_V2$Top10 <- as.factor(ifelse(train_V2$winPlacePerc > 0.9,1,0))


##Trying to plot, doesn't work well with so much data

#ggplot(data = train_V2, aes(x=Top10, fill=Top10, y=kills)) +
#  geom_violin(alpha=0.4, size=1,color="black") +
#  geom_boxplot( color="black", alpha = 0.7) +
#  geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)

## Check for missing data

gg_miss_var(train_V2, show_pct=TRUE) + labs(title="Percent Missing by Data Field") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.y=element_text(angle=0,vjust=1))

## attempting more plotting 

vioPlot <- function(df = train_V2, x, y) {
  ggplot(df, aes_string(x = x, y = y, fill = x)) + 
    geom_violin(show.legend = FALSE) + 
    geom_boxplot(width = 0.20, show.legend = FALSE) + 
    stat_summary(fun.y=mean, geom="point",
                 shape=5, size=4, color = "black",
                 show.legend = FALSE)
}

vioPlot(x="Top10",y="kills")

solo <- train_V2 %>% filter(matchType == "solo")

vioPlot(df = solo, x="Top10",y="kills")

## Descriptive Stats for train_V2
formattable(describe(train_V2))
formattable(describe(solo))
## Principle components

varsToDrop <- c("Id","matchId","matchType","rankPoints","groupId","maxPlace","winPlacePerc","Top10")
varsDropSolo <- c("DBNOs","revives")
dataPC <- train_V2 %>% select(-varsToDrop)
dataPCsolo <- solo %>% select(-varsToDrop, -varsDropSolo)

pc.result<-prcomp(dataPC,scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$Top10 <- train_V2$Top10

pc.result.solo <-prcomp(dataPCsolo,scale.=TRUE)
pc.scores.solo<-pc.result.solo$x
pc.scores.solo<-data.frame(pc.scores.solo)
pc.scores.solo$Top10 <- solo$Top10

#Scree
eigenvals<-(pc.result$sdev)^2
plot(1:22,eigenvals/sum(eigenvals),type="l",main="Scree Plot PC's",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:22,cumulative.prop,lty=2)


plotPC <- function(mydata) {
  sampleData <- sample_frac(mydata, 0.01)
  ggplot(data = sampleData, aes(x = PC1, y = PC2)) +
    geom_point(aes(col=Top10), size=1)+
    ggtitle("PCA of PUBG")
}

set.seed(314159)
plotPC(pc.scores)

set.seed(314159)
plotPC(pc.scores.solo)

ggplot(data = pc.scores.solo, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=Top10), size=1)+
  ggtitle("PCA of PUBG")
