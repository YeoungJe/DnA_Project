# 170908 D&A Data Mining
## 디렉토리 설정
setwd("C:/future/2학년 2학기 학회/1주차 코드 및 파일")

## 필요한 package 설치
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(car)) install.packages("car"); library(car)
if(!require(C50)) install.packages("C50"); library(C50)
if(!require(ROCR)) install.packages("ROCR"); library(ROCR)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

## 데이터 불러오기
ch<-read.delim("Hshopping.txt", stringsAsFactors=FALSE)
head(ch)
str(ch)

## 우리가 알고 싶은 변수는? = 반품여부
## C50 패키지의 경우 우리 알고싶은 변수를 Factor화 해줘야 함
ch$반품여부<-factor(ch$반품여부)

# factor로 바뀐거 확인
class(ch$반품여부)

# set.seed() 설정을 해주면 결과값이 똑같이 나오게됨
set.seed(1)

# 데이터 나누기(sampling  train=7 : test=3)
inTrain<-createDataPartition(y=ch$반품여부, p=0.7, list=FALSE)

ch.train<-ch[inTrain,]
ch.test<-ch[-inTrain,]

# 데이터 개수 및 변수 개수 확인
dim(ch.train); dim(ch.test)

# 의사결정나무 파라미터 설정
## winnow = 입력필드가 유용한지 측정하여 모델에 넣을지 말지를 결정하는 것
## globalPruning = 가지치기를 얼마나 할 것인지에 대한 설정
c5_options <- C5.0Control(winnow=FALSE, noGlobalPruning = FALSE)

# 모델링
c5_model<-C5.0(반품여부 ~ 성별+나이+구매금액+출연자, data=ch.train, control=c5_options, rules=FALSE)
C5imp(c_model)

# 결과 확인
summary(c_model)
summary(c5_model)
windows()
plot(c5_model)

## 결과를 칼럼으로 추가
ch.test$c5_pred <-predict(c5_model, ch.test, type="class")
ch.test$c5_pred_prob <-predict(c5_model, ch.test, type="prob")
head(ch.test)

## confusion matrix로 확인
confusionMatrix(ch.test$c5_pred, ch.test$반품여부)

# ROC Curve
c5_pred<-prediction(ch.test$c5_pred_prob[,2], ch.test$반품여부)
c5_model.perf1 <- performance(c5_pred, "tpr", "fpr") # ROC curve
plot(c5_model.perf1, colorize=TRUE)
performance(c5_pred, "auc")@y.values[[1]]

### Random Forest 
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)

# 모델 set.seed 지정
set.seed(1)
head(mj.train)
# 모델 구축
rf_model <- randomForest(반품여부 ~ 성별+나이+구매금액+출연자, data=mj.train, ntree=500, mtry=2)
rf_model

# 에러율 확인
plot(rf_model)

# 변수 중요도 확인
importance(rf_model)
varImpPlot(rf_model)

# confusion matrix로 확인
mj.test$rf_pred<-predict(rf_model, mj.test, type="response")
confusionMatrix(mj.test$rf_pred, mj.test$반품여부)

# ROC cureve
mj.test$rf_pred_prob<-predict(rf_model, mj.test, type="prob")
rf_pred<-prediction(mj.test$rf_pred_prob[,2],mj.test$반품여부)
rf_model.perf1 <-performance(rf_pred, "tpr", "fpr") # ROC-chart
plot(rf_model.perf1, colorize=TRUE); abline(a=0, b=1, lty=3)

performance(rf_pred, "auc")@y.values[[1]]
