
library(dplyr)
library(psych)
library(ggplot2)

train <- read.csv("C:/Users/Allison/Desktop/SMU/19F/Stats2/Proj2/pubg-finish-placement-prediction/train_V2.csv", stringsAsFactors=FALSE)
#test <- read.csv("C:/Users/Allison/Desktop/SMU/19F/Stats2/Proj2/pubg-finish-placement-prediction/test_V2.csv", stringsAsFactors=FALSE)

train$top.10 <- ifelse(train$winPlacePerc >= .9, 1, 0)
train$top.10 <- as.factor(train$top.10)

solo = train[which(train$matchType=="solo"),]

set.seed(1234)
indxTrain <- createDataPartition(y = solo$top.10,p = 0.7,list = FALSE)
train1 <- solo[indxTrain,]
test <- solo[-indxTrain,]

solo.down.samp <- downSample(train1,train1$top.10)


for (i in 1:ncol(train)) {
 # print(c(i, colnames(train[i])))
  cat(i,colnames(train[i]),'\n')
}

head(train)
str(train)
# detach(train)
#str(matchType)

train %>% 
  count(matchType)

# library(skimr)
# skim(solo)

head(solo)

solo$top.10<-as.factor(solo$top.10)

library(caret)

solo.down.samp <- downSample(solo,solo$top.10)

set.seed(123)
idx <- sample(1:nrow(solo), 1000)
solo.samp <- solo[idx, ]
 
# pairs(solo.down.samp[,c(5,6,8:15,17:19,21:30)], pch = 19,  cex = 0.5,
#       col = my_cols[solo.down.samp$top.10],
#       lower.panel=NULL)

pairs(solo.down.samp[,c(5,6,8:15,17:19)], pch = 19,  cex = 0.5,
      col = my_cols[solo.down.samp$top.10],
      lower.panel=NULL)

pairs(solo.down.samp[,c(21:30)], pch = 19,  cex = 0.5,
      col = my_cols[solo.down.samp$top.10],
      lower.panel=NULL)

set.seed(123)
idx <- sample(1:nrow(solo.down.samp), 1000)
solo.down.samp1 <- solo.down.samp[idx, ]

my_cols <- c("#e72f00", "#19D225") 

pairs(solo.down.samp1[,c(5,6,8:11, 29)], pch = 19,  cex = 0.5,
      col = my_cols[solo.down.samp1$top.10],
      lower.panel=NULL)

pairs(solo.down.samp1[,c(12:15,17:19, 29)], pch = 19,  cex = 0.5,
      col = my_cols[solo.down.samp1$top.10],
      lower.panel=NULL)

pairs(solo.down.samp1[,c(17:19, 21:23, 29)], pch = 19,  cex = 0.5,
      col = my_cols[solo.down.samp1$top.10],
      lower.panel=NULL)

pairs(solo.down.samp1[,c(24:30)], pch = 19,  cex = 0.5,
      col = my_cols[solo.down.samp1$top.10],
      lower.panel=NULL)
# linearly assoc w/ winPlacePerc: 
#weaponsAcquired, walkDistance, kills, longestKill, killPlace, heals, headshotKills, damageDealt, boosts

pairs.panels(solo.down.samp1[,c(5,6,8,9,10,12,14,26,27,29)],
             bg=c("green","red")[solo.down.samp1$top.10],
             density = TRUE)

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[solo.down.samp1$top.10])
}
# Create the plots
pairs(solo.down.samp1[,c(5,6,8,9,10,12,14,26,27,29)], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


model.main<-glm(top.10 ~ weaponsAcquired+walkDistance+kills+longestKill+killPlace+heals+headshotKills+damageDealt+boosts,
                data=solo.down.samp,family = binomial(link="logit"))

library(car)
(vif(model.main)[,3])^2

summary(model.main)

# pairs.panels(solo.samp[,c(4:29)], 
pairs.panels(solo.samp[,c(4:9)], 
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
pairs.panels(solo.samp[,c(10:14)], 
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
pairs.panels(solo.samp[,c(15:19)], 
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
pairs.panels(solo.samp[,c(20:24)], 
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
pairs.panels(solo.samp[,c(25:29)], 
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

for (i in 4:28) {
  pairs.panels(solo.samp[,c(29,i)], 
               hist.col = "#00AFBB",
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )
}

solo.short <- solo[which(solo$matchDuration<=1600),]
solo.long <- solo[which(solo$matchDuration>1600),]

str(solo.long[15])

solo1 <- solo[which(solo$rankPoints != -1),]
solo2 <- solo1[which(solo1$killPoints > 0),] # don't use this I guess


set.seed(123)
idx <- sample(1:nrow(solo1), 1000)
solo1.samp <- solo1[idx, ]

for (i in 4:28) {
  pairs.panels(solo1.samp[,c(29,i)], 
               hist.col = "#00AFBB",pch=21+as.numeric(solo1.samp$top.10),
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )
}

solo1.short <- solo1[which(solo1$matchDuration<=1600),]
solo1.long <- solo1[which(solo1$matchDuration>1600),]


pc.result<-prcomp(solo1.short[c(4:6,8:15,17:19,21:28)],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$top.10<-as.factor(solo1.short$top.10)


#Loadings for interpretation
pc.result$rotation

par(mfrow=c(1,1))
#Scree plot
pc.eigen<-(pc.result$sdev)^2
pc.prop<-pc.eigen/sum(pc.eigen)
pc.cumprop<-cumsum(pc.prop)
plot(1:22,pc.prop,type="l",main="Scree Plot",ylim=c(0,1),xlab="PC #",ylab="Proportion of Variation")
lines(1:22,pc.cumprop,lty=3)

#Use ggplot2 to plot the first few pc's
ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=top.10), size=1)+
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  ggtitle("PCA plot ")



library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(9, "Set1"))
x<-t(solo1.samp[c(4:6,8:15,17:19,21:28)])
colnames(x)<-solo1.samp$top.10
heatmap(x,annotation_col=data.frame(top.10=solo1.samp$top.10),top.10=list(top.10=c("0"="white","1"="green")),scale="row",legend=T,color=colorRampPalette(c("blue","white", "red"), space = "rgb")(100))



duo = train[which(train$matchType=="duo"),]

set.seed(123)
idx <- sample(1:nrow(duo), 1000)
duo.samp <- duo[idx, ]

par(mfrow=c(1,1))
for (i in 4:28) {
  pairs.panels(duo.samp[,c(29,i)], 
               hist.col = "#00AFBB",pch=21+as.numeric(duo.samp$top.10),
               density = TRUE,  # show density plots
               ellipses = TRUE # show correlation ellipses
  )
}


