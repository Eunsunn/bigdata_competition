setwd("C:/Users/USER/Desktop/travel_competition/native/data")

#install.packages('arules')
#install.packages('arulesViz')
#install.packages('shinythemes')
library(arulesViz)
library(arules)
library(shinythemes)

data = read.csv("tr4.csv")

##연관분석
df =  as.data.frame(data)
df = subset(df, select=-money)
tran_df = as(df, "transactions")
rules <- apriori(tran_df, parameter = list(support=0.1, confidence=0.6, minlen=2),
                 control=list(verbose=F))

rules.sorted <- sort(rules, by='confidence') #confidence를 기준으로 정렬
inspect(rules.sorted)
ruleExplorer(rules.sorted)

##기초통계분석
#target=exp(data$tr_money)
#summary(target)
summary(df$tr_money)
boxplot(df$tr_money, main='boxplot of transformed money')


exp(11.9)
