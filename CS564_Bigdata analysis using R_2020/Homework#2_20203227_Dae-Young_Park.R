#1
x <- c(1,2,3,4,5,2,4,3,5,1,2,3,4,5,1,2)
fct <- factor(x,levels=1:5) # for testing
fct # for testing

y <- c("Red","Green","Blue","Magenta")

y[x] # this is answer1 


#2
A <- matrix(c(1,0,5,2,1,2,3,4,4),nrow=3)
B <- matrix(c(2,-1,3,3,2,9,0,5,2),nrow=3)

C <- A %*% B
C # this is answer2

#3
data <- data.frame(state.x77)

income_lessthan4000 <- data[data$Income<4000,]

answer3_1 <- nrow(income_lessthan4000)
answer3_1 # this is answer3_1

answer3_2 <- subset(data, Income >= max(data$Income))
answer3_2 # this is answer3_2
