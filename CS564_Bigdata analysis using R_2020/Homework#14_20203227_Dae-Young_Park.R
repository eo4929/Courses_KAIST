library(arules); library(arulesViz)

data(Adult)
head(Adult)
str(Adult)
inspect(head(Adult,2))

#1-(a)
itemFrequencyPlot(Adult, topN=12, type="absolute")
itemFrequencyPlot(Adult, topN=12) 

#1-(b)
rules <- apriori(Adult)
sortedRules <- sort(rules, by="support")
sortedRules_top12 <- sortedRules[1:12]

inspect(sortedRules_top12)
plot(sortedRules_top12, method="graph", measure='confidence', shading='lift', control = list(type="items"))

#answer: 'capital-loss=None' and 'capital-gain=None' are the most frequent items

#fItem <- eclat(Adult)
#sort_fItem <- sort(fItem, by='support')
#sort_fItem_top12 <- sort_fItem[1:12]
#inspect(sort_fItem_top12)

#1-(c)
tab <- crossTable(Adult)
tab[1:5,1:5]

tab['workclass=Private','age=Middle-aged']
tab['age=Middle-aged','age=Middle-aged']
17860 / 24671
# answer: 17860 / 24671 = 0.7239