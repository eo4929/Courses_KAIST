
#1
library(data.table)
DT <- data.table(
  counts = c(4,2,5,3,7,5,3,6,3,4,8,6,4,7,8,6,5,9,3,4,5),
  Therapy = c("Cognitive","Cognitive","Cognitive","Cognitive","Cognitive","Cognitive","Cognitive",
              "Behavioral","Behavioral","Behavioral","Behavioral","Behavioral","Behavioral","Behavioral",
              "Medication","Medication","Medication","Medication","Medication","Medication","Medication")
)
DT_df <- as.data.frame(DT)
head(DT_df)
new_DT_df <- unstack(DT_df)
round(sapply(new_DT_df,mean),1)
#1-1
aov.out <- aov(counts~Therapy,data=DT_df) # answer: fail to reject null hypothesis  => therapy are not equally effective
summary(aov.out)

library(ggplot2)

#1-2
d1=density(DT_df$counts[DT_df$Therapy=="Cognitive"],na.rm=TRUE)
d2=density(DT_df$counts[DT_df$Therapy=="Behavioral"],na.rm=TRUE)
d3=density(DT_df$counts[DT_df$Therapy=="Medication"],na.rm=TRUE)

plot(d1,col="green",lwd=2,lty=1,main="Density Plot")
lines(d2,col="blue",lwd=2,lty=2)
lines(d3,col="red",lwd=2,lty=3)
legend("topright",legend=c("Cognitive","Behavioral","Medication"), col=c("green","blue","red"),lty=1:3)
plot.new()

#2
DT2 <- data.table(
  Gender = c("Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Male","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female","Female"),
  age = c("Young_adult","Young_adult","Young_adult","Young_adult","Young_adult","Young_adult","Young_adult","Young_adult","Young_adult","Young_adult","Middle_adult","Middle_adult","Middle_adult","Middle_adult","Middle_adult","Middle_adult","Middle_adult","Middle_adult","Middle_adult",
          "Middle_adult","Older_adult","Older_adult","Older_adult","Older_adult","Older_adult","Older_adult","Older_adult","Older_adult","Older_adult","Older_adult"),
  score = c(4,2,3,4,2,7,4,3,6,5,7,5,7,5,6,8,10,7,7,8,10,7,9,8,11,10,9,12,11,13)
)

DT_df2 <- as.data.frame(DT2)
head(DT_df2)
#2-1
aov2 <- aov(score ~ Gender+age, data=DT_df2)
summary(aov2)
#I reject the null hypothesis that the means of score evaluated according to the Gender are equal.
#I reject the null hypothesis that the means of score evaluated according to the age are equal.

aov3 <- aov(score ~ Gender:age, data=DT_df2)
summary(aov3)
#I retain the null hypothesis that the means of score evaluated according to the age are equal.


#2-2
boxplot(formula=score~Gender,data=DT_df2)
plot.new()
boxplot(formula=score~age,data=DT_df2)


