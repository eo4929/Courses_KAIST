

#1
example1 <- read.csv('CS564-Exercise-data-sleeps_PCA.csv', header=TRUE)
head(example1)

library(dplyr)
refinedExample1 <- example1 %>% select(sns_freq_zscore, dist_average_exp_zscore, sleep_average_zscore, sleep_weekprop_zscore)
head(refinedExample1)

for(i in 1:4)
  refinedExample1[,i] <- as.numeric(refinedExample1[,i])


dim(refinedExample1)
refinedExample1 = na.omit(refinedExample1) 
dim(refinedExample1)

#1-(1)
library(psych)
scree(refinedExample1, factors=FALSE, pc=TRUE) # answer: 2

#1-(2)
pcav <- principal(refinedExample1, nfactors=2, rotate='varimax')
pcav
# The principal component equation using the first set of weights: 
# Y1 = -0.7*sns_freq_zscore + 0.07*dist_average_exp_zscore + 0.86*sleep_average_zscore + (-0.1) * sleep_weekprop_zscore
# The principal component equation using the second set of weights: 
# Y2 = -0.48*sns_freq_zscore + 0.64*dist_average_exp_zscore + (-0.25)*sleep_average_zscore + (0.7) * sleep_weekprop_zscore

#1-(3)
fa.diagram(pcav, main='A diagram of the PCs')

#2 This is a optional problem

#3 
data(USArrests)
head(USArrests)

for(i in 1:4)
  USArrests[,i] <- as.numeric(USArrests[,i])

dim(USArrests)
refinedUSArrests = na.omit(USArrests) 
dim(refinedUSArrests)

pcav2 <- principal(refinedUSArrests, nfactors=2, rotate='varimax')
pcav2
#3-(1)
# The principal component equation using the first set of weights: 
# Y1 = 0.94*Murder + 0.92*Assault + 0.07*UrbanPop + 0.73 * Rape
# The principal component equation using the second set of weights: 
# Y2 = (-0.06)*Murder + 0.18*Assault + 0.97*UrbanPop + 0.48 * Rape

#3-(2)
par(mar=c(4,4,1,1))
biplot(pcav2,labels=rownames(USArrests))
# intepretation: UrbanPop strongly influences PC2, Murder strongly influences PC1
# Because Rape and Assault are relatively close, the variables they represent are relatively positively correlated compared with other relationships between vectors

#3-(3)
refinedUSArrests_withScaling = scale(refinedUSArrests)
head(refinedUSArrests_withScaling,4)

pcav3 <- principal(refinedUSArrests_withScaling, nfactors=2, rotate='varimax')
pcav3
# result: The principal component equations are same
