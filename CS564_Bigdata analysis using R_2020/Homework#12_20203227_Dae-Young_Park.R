
#install.packages('rpart')
library(rpart)


#1
library(NbClust)

head(kyphosis)
test_kyphosis <- kyphosis[-1]


ds <- dist(test_kyphosis, method="euclidean")
hcst <- hclust(ds,method="complete")

#1-a
plot(hcst, labels=rownames(test_kyphosis), cex=0.8) 
rect.hclust(hcst, 2)
plot.new()
#1-b
#install.packages('caret')
#install.packages("e1071")
library(caret)
library(e1071)

cn <- cutree(hcst, k=2)
cn


predicted_labels <- factor(cn, levels=1:2, labels=c("absent", "present"))
groundTruth <- factor(kyphosis$Kyphosis)
tab <- table(predicted_labels, groundTruth)
confusionMatrix(tab) # result: accuracy is 0.5556

#1-c
library(cluster)
pamd = pam(ds, 2)
plot(pamd)


#2
dataset <- read.csv('CS564-12-Exercise-data-sleeps_clustering.csv')

#2-a
original_scale <- dataset[,c(2,4,6,8)]
kc <- kmeans(original_scale, centers=3)

#dev.off()
clusplot(original_scale,kc$cluster,color=TRUE,shade=TRUE,labels=3)
plot.new()
#2-b
sobj <- silhouette(kc$cluster, dist(original_scale))
plot(sobj, col=2:4)
plot.new()
#2-c
ds2 <- dist(original_scale, method="euclidean")
hcst2 <- hclust(ds2,method="complete")
plot(hcst2, labels=rownames(original_scale), cex=0.8)
rect.hclust(hcst2, 3)
plot.new()
#2-d
library(dplyr)
cn2 <- cutree(hcst2, k=3)
cn2
labeled_original_scale <- mutate( original_scale, label = cn2, sleep_average = (sleep_average / 100) )

grouped <- group_by(labeled_original_scale,label )
grouped %>% group_rows()

res <-  grouped %>% summarise(mean_sns_freq_coded = mean(sns_freq_coded), mean_dist_average = mean(dist_average), mean_sleep_average = mean(sleep_average), mean_sleep_weekprop = mean(sleep_weekprop) )
res <- as.matrix(res)
rownames(res) <- c("1", "2", "3")
res <- res[,-1]

#barplot(grouped, ylab="Frequency", xlab="Gears", main="Side-By-Side Bar Chart", col=c("turquoise4", "turquoise2", "turquoise" ), beside=TRUE, width=.3)

barplot(res, beside=T, col=4:6, names.arg=c('mean_sns_freq_coded','mean_dist_average','mean_sleep_average','mean_sleep_weekprop'), xlab="four features",ylab="mean values", cex.names = 0.7)
legend('topleft', title='group(labels)', legend=1:3, fill=4:6)
# For mean_sns_freq_coded, mean_dist_average, and mean_sleep_average, there are significance differences among means per each group(cluster)


