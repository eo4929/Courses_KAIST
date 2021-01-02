
#1

#install.packages('MASS')
library(MASS)
data(Cars93, package="MASS")
head(Cars93)
str(Cars93)

#1-1)
x <- sample(Cars93$Price, 10)
x <- mean(x)
for(i in 1:9){
  x <- c(x, mean(sample(Cars93$Price, 10)))
}
ans1_1 <- x
ans1_1 # answer with n=10

x <- sample(Cars93$Price, 10)
x <- mean(x)
for(i in 1:99){
  x <- c(x, mean(sample(Cars93$Price, 10)))
}
ans1_2 <- x
ans1_2 # answer with n=100

x <- sample(Cars93$Price, 10)
x <- mean(x)
for(i in 1:999){
  x <- c(x, mean(sample(Cars93$Price, 10)))
}
ans1_3 <- x
ans1_3 # answer with n=1000

#1-2)
mean(ans1_1)
mean(ans1_2)
mean(ans1_3)

#1-3)
true_average = mean(Cars93$Price)
true_average
# I can infer true_average(grand average) and avearages of sampled items are similar. Especially, if n has sufficient large numbers, more similar


#2
#2-1)
hist(Cars93$Horsepower,ylim=c(0,40))
#2-2)
shapiro.test(Cars93$Horsepower)
# ans: p-value = 0.0001916
#2-3)
qqnorm(Cars93$Horsepower)
# ans: Because p-value of the Shapiro-test result is less than 0.05, I can know the data don't follow normal distribution

#3
Cars93$Origin
Cars93$Weight

var.test(Weight~Origin, data=Cars93)$p.value # 두 데이터타입에 대한 var가 같다고 나오네
res <- t.test(Weight~Origin, data=Cars93, alternative="two.sided", var.equal=TRUE)
res # # two means don't be equal



