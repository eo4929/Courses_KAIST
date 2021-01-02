
#1
X <- c(3.3,4.3,3.5,4.0,3.0,3.1,3.7,3.9,3.5,2.7,2.6,4.0)
Y	<- c(3.7,3.9,3.2,4.1,2.8,3.8,4.2,3.4,3.9,2.6,2.1,4.3)
df_1 <- data.frame(X,Y)

#1-a
ur1 <- lm(Y ~ X, data=df_1)
beta  = coef(ur1)
# answer
beta[1] # Y-intercept
beta[2] # slope

#1-b
with(df_1,plot(X,Y,pch=21,bg='cyan'))
lines(df_1$X,ur1$fitted.values,col='red')

#1-c
predict(ur1, list(X=3.75))

#1-d
summary(ur1)
# the coefficient of determination(R-squared) = 0.6329 # if this value is close to 1, more Y can be explained by X (by the fitted model)
# the coefficient of non-determination = 1 - R^2 = 0.3671

#1-e
summary(ur1) # p-value = 0.001972 # reference: http://www.r-tutor.com/elementary-statistics/simple-linear-regression/significance-test-linear-regression
anova(ur1) # p-value = 0.001972 # when the number of populations is 2, this is similar to t.test
# answer: Because p-value is less than 0.05, there is a significance between the variables in the linear regression model

#1-f
library(ggplot2)
ggplot(df_1, aes(x = X, y = Y)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = X, yend = ur1$fitted.values), alpha = .2) + 
  geom_point() +
  geom_point(aes(y = ur1$fitted.values), shape = 1) +
  theme_bw()  # Add theme for cleaner look



#2
#install.packages("datarium")
library("datarium")
data(marketing, package="datarium")
head(marketing)

form <- sales ~ youtube + facebook + newspaper
mf1 = lm(form, data=marketing)
summary(mf1)

library(coefplot)
coefplot(mf1, intercept=FALSE)

library(psych)
cordat <- marketing
pairs.panels(cordat) # a scatter plot, histograms and the Pearson correlation for a data matrix


# answer
#2-a) yes
#2-b) yes
#2-c) no
#2-d) considering coefficient and correlation, they should invest on youtube and facebook(Especially, facebook) ,but not newspaper


#3
library(data.table)
DT <- data.table(
  Temp = c(27.2,27.2,27.2,27.7,27.7,27.7,28.3,28.3,28.3,28.4,28.4,28.4,29.9,29.9,29.9),
  #male = c(1,0,1,7,4,6,13,6,7,7,5,7,10,8,9),
  #female = c(9,8,8,3,2,2,0,3,1,3,3,2,1,0,0),
  percent_male = c(0.1,0.0,0.11,0.7,0.67,0.75,1.0,0.67,0.88,0.7,0.63,0.78,0.91,1.0,1.0)
)
DT_df <- as.data.frame(DT)

#3-a
ulr <- glm(percent_male~Temp,family=quasibinomial(logit),data=DT_df)
summary(ulr) 
# logit(P) = -61.4343 + 2.2152*Temp

#3-b
beta = coef(ulr)
exp(beta[2]) # odds ratio -> For every 1-pt increase in Temp, Prob of male increases by 9.162 times higher than previous stage
# p-value=0.00252 < 0.05 => Temp is significantly associated with probability of male
a=0.05; (tc=qt(1-a/2,df=13))
tc

#3-c
prob_of_male <- DT_df$percent_male
temp <- DT_df$Temp
plot(prob_of_male~temp, pch=20,col="blue", main='Fitted Logistic Regression Line with Observed Data')
lines(temp, ulr$fitted, type="l", col="red")


#4
ex4_dt <- read.csv('ex4_data.csv', header=T)

pairs.panels(ex4_dt)

#4-a + 4-b
mlr <- glm(vacc~belief+exposure,family=binomial(logit),data=ex4_dt)
summary(mlr)
#ans of a: because p-value < 0.05, belief is statistically significant to the probability of willingness of vaccine
#ans of b: because p-value > 0.05, exposure is not significant to the probability of willingness of vaccine

#4-c
mlr_beta = coef(mlr)
exp(mlr_beta[2]) # odds ratio: For every 1-pt increase in belief, the odds of having reached willingness increases by 0.5294746 higher than previous stage


