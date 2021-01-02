library(dplyr)
library(BBmisc)


data_DY <- read.csv("output.csv", header=T)
data_shin <- read.csv("output_shin.csv", header=T)
data_HS <- read.csv("output_HS.csv", header=T)
data_JY <- read.csv("output_JY.csv", header=T)
data_sjy <- read.csv("output_sjy.csv", header=T)


data_DY <- read.csv("output.csv", header=T)

data_DY_2 <- data_DY[, c('emoticon_ratio', 'chat_type2')]
data_shin_2 <- data_shin[, c('emoticon_ratio', 'chat_type2')]
data_HS_2 <- data_HS[, c('emoticon_ratio', 'chat_type2')]
data_JY_2 <- data_JY[, c('emoticon_ratio', 'chat_type2')]
data_sjy_2 <- data_sjy[, c('emoticon_ratio', 'chat_type2')]

data_DY_2 <- bind_rows(data_DY_2, data_shin_2, data_HS_2, data_JY_2, data_sjy_2)
head(data_DY_2)

# boxplots of different chat types
boxplot(emoticon_ratio~chat_type2, data=data_DY_2,
        xlab='Chat types (Private/Public)',
        ylab='Emoticon Ratio', col=2:3)
abline(h=mean(data_DY_2$emoticon_ratio),col='gray')

#Conducting One-Way ANOVA 
aov.out <- aov(emoticon_ratio~chat_type2,data=data_DY_2)
summary(aov.out) # 
anova(lm(emoticon_ratio~chat_type2,data=data_DY_2)) # aov와 결과 거의 같음음 근데 사실 이건 여러 분산 모델들에 대한 분석을 하기 위함임

# 결과 해석
# 1) alpha level은 0.05 또는 0.01 로 두고, 이거보다 작으면 기각 -> 귀무가설: chat types(Private/Public) are significantly equally effective 
# 2) 기각하면 chat types(Private/Public) are not significantly equally effective -> they are significantly different 
# 3) F-value 값도 알려주기
# 4) ch8 에 있는 순서로 해석해서 설명하기


# 개인별 친밀도 점수 표준화



