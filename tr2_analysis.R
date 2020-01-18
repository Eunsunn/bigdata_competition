setwd("C:/Users/USER/Desktop/travel_competition/native/data")

#install.packages('arules')
#install.packages('arulesViz')
#install.packages('shinythemes')
#install.packages('deplyr')
library(arulesViz)
library(arules)
library(shinythemes)
library(dplyr)
library(ggplot2)

data = read.csv("tr4_1.csv")

##연관분석
df =  as.data.frame(data)
df = subset(df, select=-tr_money)
tran_df = as(df, "transactions")
rules <- apriori(tran_df, parameter = list(support=0.1, confidence=0.6, minlen=2),
                        control=list(verbose=F))
rules.sorted <- sort(rules, by='confidence') #confidence를 기준으로 정렬
inspect(rules.sorted)
ruleExplorer(rules.sorted)

##기초통계분석
summary(df$money)

ggplot(data = df, aes(GB2))+geom_bar()

#age = df %>% group_by(CLN_AGE_R) %>% summarise(median = median(money))
age2 = arrange(age, desc(median))
age2
