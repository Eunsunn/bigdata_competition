tr1 = read.csv("C:/Users/Kim Yuum/Desktop/공모전/tr1.csv")
str(tr1)

summary(tr1$tr_money)
summary(tr1$money)
tr1 = tr1[,-7]
tr1 = as(tr1, "transactions")
boxplot(tr1$tr_money)


####연관분석###
library(arules)
rules = apriori(tr1 , parameter=list(supp=0.1, conf=0.6,minlen=2))
inspect(rules)
# confidence(신뢰도) 기준 내림차순으로 규칙 정렬
rules = sort(rules, decreasing=T, by="confidence")
rules = subset(rules, lift >= 1.1|lift <=0.9 )
inspect(rules) 
ruleExplorer(rules)
#시각화
library(arulesViz)
plot(rules, method="graph", control=list(type="items"))


#"활동" 제거
tr1 = as.data.frame(tr1)

try = tr1[tr1$APV_TS_DL_TM_R != "활동",]
try = try[,-7]
rules.try = apriori(try , parameter=list(supp=0.1, conf=0.6,minlen=2))
rules.try = sort(rules.try, decreasing=T, by="confidence")
rules.try = subset(rules.try, lift >= 1.1|lift <=0.9 )
inspect(rules.try) 
ruleExplorer(rules.try)
