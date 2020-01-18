rm(list = ls());gc()

setwd("C:/Users/USER/Desktop/travel_competition/foreign/data")

library(haven)
library(dplyr)
library(ggplot2)
library(mice)
library(VIM)
library(xlsx)

raw1 <- read.csv('2016_raw.csv')
raw2 <- read_sav("2017_raw.sav")
raw3 <- read_sav("2018_raw.sav")
colSums(is.na(raw1))

df <- merge(raw1, raw2, all=T)


#전처리 작업 시급
df = df[colSums(is.na(df)) <= 5169]#20%
colSums(is.na(df))

str(df) #특성이 다 unknown임
#df = sapply(df, as.integer(df))

#df2 =  mutate_all(df, function(x) as.numeric(as.character(x)))
#str(df2)
#df2$CITY = as.factor(df2$CITY)
#df2$Q16 = as.factor(df2$Q16)
#df2$Q17 = as.factor(df2$Q17)
#df2 = subset(df2, select = -c(ID)) #id 삭제
#df2$COUNTRY = as.factor(df2$COUNTRY)

#d들어간거 다 factor로
#factor = select(df2, starts_with("D")) 
#factor = factor[-c(1:2)]
#factor = mutate_all(factor, function(x) as.factor(as.numeric(x)))

#무응답 제거
df = df[df$job!=99, ]
df = df[df$age!=9, ]

ls = c('chasu', 'nat', 'city', 'sex', 'edu', 'job', 'age')


#합치기
df3 = merge(df2, factor)

df4 = df3 %>% mutate_if(is.numeric, round, digits=0)

#결측치 시각화 -> mice 적용
aggr(df4,prop=FALSE,numbers=TRUE)
#규칙이 보임

#mice 적용. 10번 돌림
imp=mice(df4, seed=1234, m=10)

imp_output = complete(imp)
colSums(is.na(imp_output))
#채워지지 않는 거 버려
df_final = imp_output[colSums(is.na(imp_output)) == 0]
colSums(is.na(df_final))

#종속변수: Q16
str(df_final)
df_final = subset(df_final, select = -c(COUNTRY, CITY))

#작업 저장
imp_bak = imp_output
save(imp_output, file="mice_bak.RData")

#랜덤포레스트!
library(caret)
library(randomForest)

idx = createDataPartition(df_final$Q16, p=0.7, list = F)
train = df_final[idx,]
test = df_final[-idx,]

rf = randomForest(Q16~., data = train)
varImpPlot(rf)
importance(rf)

#####정확도
y_pred = predict(rf, test)
confusionMatrix(y_pred, test$Q16)
#약 96%ㅋㅋㅋㅋㅋㅋ

#10FOLD CV
accuracy_full<-c()
idx <- createFolds(df_final$Q16, k=10)
idx[[1]] #validation set으로 (10fold)

for (i in 1:10){
    training_fold <- df_final[-idx[[i]],]
    test_fold = df_final[idx[[i]],]
    
    classifier = randomForest( Q16~.,
                      data = training_fold)
    
    y_pred = predict ( classifier, newdata = test_fold )
    cm = table ( test_fold$Q16, y_pred )
    accuracy = sum(diag(cm))/sum(cm)
    
    accuracy_full<-c(accuracy_full,accuracy)
    print(i)
}


accuracy_full
mean(accuracy_full)



