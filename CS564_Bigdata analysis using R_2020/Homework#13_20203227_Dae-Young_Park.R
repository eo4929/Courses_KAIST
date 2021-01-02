

#1
HP <- read.csv('HappyIndex.csv', header=TRUE)
str(HP)
HD <- data.frame(Rank=HP[,1],Country=HP[,2],LifeExpectancy=HP[,4],Wellbeing=HP[,5],
                 Footprint=HP[,7],InequalityOutcome=HP[,8],HPI=HP[,11])
head(HD)

summary(HD[4])

low_bound <- 4.575 # 1st Qu.
middle_bound <- 6.225 # 3rd Qu.

HD_low_wellbeing <- HD[HD$Wellbeing < low_bound,]
HD_middle_wellbeing <- HD[ (low_bound <= HD$Wellbeing & HD$Wellbeing < middle_bound ),]
HD_high_wellbeing <- HD[HD$Wellbeing >= middle_bound,]

lowLabel <- rep("low", times = 35)
middleLabel <- rep("middle", times = 70)
highLabel <- rep("high", times = 35)
HD_low_wellbeing <- cbind( HD_low_wellbeing, wellbeing_label = lowLabel )
HD_middle_wellbeing <- cbind( HD_middle_wellbeing, wellbeing_label = middleLabel )
HD_high_wellbeing <- cbind( HD_high_wellbeing, wellbeing_label = highLabel )

new_HD <- rbind( HD_low_wellbeing,HD_middle_wellbeing,HD_high_wellbeing)

#1-a
aov.out <- aov(HPI~wellbeing_label,data=new_HD)
res_aov <- anova(lm(HPI~wellbeing_label, data=new_HD))
res_aov
#1-b
tkh <- TukeyHSD(aov.out, conf.level=0.95)
tkh
#plot(tkh, las=1)


#2
#2-a
HF <- read.csv('HappyFactor.csv', header=TRUE)
head(HF)
library(psych)
describe(HF[,3:8])

psych::pairs.panels( HF[,c(3:8)], lm=TRUE, ellipses = FALSE )

#2-b
library(dplyr)
kc <- kmeans(HF[, c(3:6)], center = 3, nstart = 10)
HF_withClusters <- data.frame(HF, Cluster = kc$cluster)
#head(HF_withClusters, 3)
#table(HF_withClusters$Cluster)


cols <- c("purple","green","magenta")
par(mfrow=c(2,2))
boxplot(Logged.GDP.per.capita~Cluster, boxwex=0.75,xlab="Cluster", 
        ylab="Logged GDP per capita",col=cols, data=HF_withClusters)
boxplot(Social.support~Cluster, boxwex=0.75, xlab="Cluster", 
        ylab="Social support",col=cols, data=HF_withClusters)
boxplot(Healthy.life.expectancy~Cluster, boxwex=0.75,xlab="Cluster", 
        ylab=" Healthy life expectancy", col=cols, data=HF_withClusters)
boxplot(Logged.GDP.per.capita~Cluster, boxwex=0.75,xlab="Cluster", 
        ylab="Freedom to make life choices",col=cols, data=HF_withClusters)

# interpretation: In terms of four features, all same ranks among different clusters so, I can know they(features) have positive correlations

