
#1
#install.packages('MASS') # comment out because already installed
#data(package="MASS")
library(MASS)
#1-(1)
x <- log(Animals$brain); y <- log(Animals$body)
plot(x,y, xlab='brain', ylab='body', main='Animals')
text(x,y, labels=row.names(Animals))
#1-(2)
ggplot(data=Animals,aes(x=brain,y=body,label=row.names(Animals))) + geom_text(size=3) + geom_point(aes(x=brain,y=body)) + theme_bw() + scale_x_log10() + scale_y_log10()

#2
A <- matrix(c(652,36,218,1537,46,327,598,38,106,242,21,67),nrow=3)
colnames(A) <- c("0","1-150","151-300",">300")
rownames(A) <- c("Married","Prev.married","Single")

barplot(A,beside=TRUE,legend=TRUE,col=1:3,xlab="Caffeine Consumption",ylab="Martial State")

#3
#3-(1)
ggplot(iris, aes(x=Sepal.Length, fill=Species)) +
  geom_histogram(colour="black") + theme_bw() +
  facet_grid(Species ~ .) + xlab('Sepal.Length')

#3-(2)
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length ,color=Species)) +
  geom_point() + theme_bw() + xlab('Sepal.Length') + ylab('Petal.Length') + geom_smooth(method = 'lm')

#4
ggplot(airquality, aes(Day,Ozone,size=Wind,col=factor(Month,labels= c('May','June','July','August','Semptember'))),
       ) + geom_point() + 
  xlab("Day of the month") + ylab("Ozone(ppb)") + scale_x_continuous(breaks=seq(1,31,5)) + 
  ggtitle("Air Quality in New York by Day") + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name = 'Month')



