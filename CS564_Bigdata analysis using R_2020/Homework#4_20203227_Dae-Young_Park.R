#install.packages('data.table')
#install.packages('xml2')
#install.packages('rvest')
#install.packages('XML')
#install.packages('dplyr')
#install.packages('httr') # for crawling


library(XML)
library(httr)

#1
web_link = GET('https://en.wikipedia.org/wiki/Gender_Inequality_Index')

Rform_web_link = htmlParse(web_link)
Rform_web_link.table = readHTMLTable(Rform_web_link, stringsAsFactors=FALSE)
str(Rform_web_link.table)
str(Rform_web_link.table[[1]])
class(Rform_web_link.table[[1]])
head(Rform_web_link.table[[1]])
tail(Rform_web_link.table[[1]])

extracted_table = Rform_web_link.table[[1]][2:159,]
extracted_table <- extracted_table[,-1]
extracted_table <- extracted_table[,-2]

extracted_table[[1]] <- as.numeric(extracted_table[[1]])
extracted_table[[2]] <- as.numeric(extracted_table[[2]])

colnames(extracted_table) <- c('HDI_rank','Gll_value')
rownames(extracted_table) <- c(1:nrow(extracted_table))
head(extracted_table)

ggplot(extracted_table,aes(x=Gll_value, y=HDI_rank)) + geom_point() + geom_smooth(method='lm',formula=y~x,level=0.9) + theme_bw()


#2
library(gtrendsR)
library(reshape2)
#2-a
hs_trend1 <- gtrends(keyword=c("CORONA","ONLINE","DELIVERY"), time="today 12-m")
plot(hs_trend1)
# my answer: yes and I can find two keywords all are related to CORONA


#2-b
corona_trends <- gtrends(keyword=c("CORONA"), geo=c("CA","US"), time="today 12-m")
corona_trend <- gtrends(keyword=c("CORONA"), geo=c("CA","US"), time="today 12-m")[[1]]

ggplot(corona_trend, aes(x=date, y=hits)) +
  geom_point(aes(color=geo)) + 
  facet_grid( geo ~ .) +
  theme_bw() + ggtitle("first: “CORONA”")

covid_trends <- gtrends(keyword=c("COVID"), geo=c("CA","US"), time="today 12-m")
covid_trend <- gtrends(keyword=c("COVID"), geo=c("CA","US"), time="today 12-m")[[1]]

ggplot(covid_trend, aes(x=date, y=hits)) +
  geom_point(aes(color=geo)) + 
  facet_grid( geo ~ .) +
  theme_bw() + ggtitle("second: “COVID”")

# my answer: patterns are similar in terms of country 
#but, patterns are different a little bit in terms of each keyword(CORONA, COVID)