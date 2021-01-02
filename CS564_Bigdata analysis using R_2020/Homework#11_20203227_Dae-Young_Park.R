library(TH.data)
library(grid); library(libcoin)
library(mvtnorm); library(partykit)
library(rpart)

#1
dim(cu.summary)
#1-a
rfit <- rpart(Mileage~., data=cu.summary, method="anova",control=rpart.control(minsplit=10)) # Because minsplit is not specified, I select 10 as a arbitrary value 
rfit
#1-b
plot(rfit, uniform=TRUE, main="Regression Tree")
text(rfit, use.n=TRUE, all=TRUE, cex=.8)
#ans: Price, Country, and Type are used ,but not Reliability

#1-c
#ans: when Price < 9446, Mileage corresponds to the higher values than remaining conditions and they are twelve and the mean is 32.08

#2
#2-a
head(kyphosis)
cfit <- rpart(Kyphosis ~ ., data=kyphosis, method="class")
cfit
#2-b
plot(as.party(cfit), tp_args=list(id=FALSE))
#ans: Start, Age
#2-c
#ans: 29, 12, 14, 7, 19


#3
library(quint)
data(bcrp); head(bcrp,4)

form3 <- I(physt3-physt1) ~ cond | cesdt1+negsoct1+uncomt1+disopt1+comorbid+age+wcht1+nationality+marital+trext

set.seed(48)

#3-a
control <- quint.control(crit='dm') # In the problem, Because values of maxl and B are not specified, I choose default values.
quint3 <- quint(form3, data=subset(bcrp,cond<3), control=control)
#3-b
control2 <- quint.control(maxl=2)
quint4 <- quint(form3, data=subset(bcrp,cond<3), control=control2)
plot(quint4)
round(quint4$li, digits=2)
#ans: In Leaf1, the mean improvement was 3.28 for the nutrition intervention and 6.88 for the education intervention.
# In Leaf 2, the mean improvement was 4.33 for the nutrition intervention and 1.53 for the education intervention.
