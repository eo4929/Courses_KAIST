
#CS564 Final Exam Answer Sheet Code Part
#2020.12.16
#Code submission is optional. 
#This is only used when partial grade can be applied. 


#Question 1 ######################
data_state <- data.frame(state.x77)
nrow(data_state)
ncol(data_state)
income_lessthan4000 <- data[data$Income<4000,]
nrow(income_lessthan4000)
answer1_3 <- subset(data_state, Income <= min(data_state$Income))
answer1_4 <- subset(data_state, Life.Exp >= max(data_state$Life.Exp))

#Question 2 ######################
dt1 <- array(c(12,31,8,33,16,35), dim=c(2,3),
             dimnames=list("Mental Illness?"=c("Yes","No"),
                           "Substance Abused"=c("Drugs","Alcohol","Drugs and Alcohol")))
chisq.test(dt1)
# There is no significant correlation(=no strong relationship) between the “Mental Illness” and “Substance Abused”.
df=2; a = 0.05
qchisq(1-a,df=2)



#Question 3 ######################
library(ISwR)
data_q3 <- data.frame(juul2)
sel <- data_q3[data_q3$age>=25,]

form <- igf1 ~ age + height
mf1 = lm(form, data=sel)
summary(mf1)

#Question 4 ######################
data_q4 <- nycflights13::flights


data_q4 %>% arrange(desc(dep_delay))
answer1_4 <- subset(data_q4, distance <= min(data_q4$distance))




#Question 5 ######################
library(rpart)
head(kyphosis)
cfit <- rpart(Kyphosis ~ ., data=kyphosis, method="class")
cfit
library(grid); library(partykit)
plot(as.party(cfit), tp_args=list(id=FALSE))

#Question 6 ######################
library(arules); library(arulesViz)
inspect(head(Adult,2))
itemFrequencyPlot(Adult, topN=12, type="absolute")
itemFrequencyPlot(Adult, topN=12) 

rules <- apriori(Adult)
sortedRules <- sort(rules, by="support")
sortedRules <- sortedRules[1:12]
inspect(sortedRules)

tab <- crossTable(Adult)
tab['workclass=Local-gov','age=Young']
tab['age=Young','age=Young']
300/9627

#Question 7 ######################
library(psych)
scree(USArrests, factors=FALSE, pc=TRUE) # answer: 2
pcav <- principal(USArrests, nfactors=2, rotate='none')
pcav

par(mar=c(4,4,1,1))
biplot(pcav,labels=rownames(USArrests))

#Question 8 ######################
library(XML)
library(httr)
download.file('https://thefourthrevolution.org/wordpress/')
web_link = GET('https://thefourthrevolution.org/wordpress/')

Rform_web_link = htmlParse(web_link)


#Question 9 ######################



