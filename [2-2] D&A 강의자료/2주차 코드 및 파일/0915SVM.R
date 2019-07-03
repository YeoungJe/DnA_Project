# 170915 D&A data mining
###SVM 모델
setwd("C:/future/2학년 2학기 학회/2주차 코드 및 파일")
#Load Pacakages
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(ROCR)) install.packages("ROCR"); library(ROCR)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

#Load Data
cb <- read.delim("Hshopping.txt", fileEncoding = "EUC-KR", stringsAsFactors=FALSE)
cb$반품여부 <- factor(cb$반품여부)

# data 탐색
cb %>% head

# data 분할
set.seed(3)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

#SVM Modeling
svm_model <- svm(반품여부~성별+나이+구매금액+출연자, data=cb.train, cost=300, gamma=0.9, probability = TRUE)
summary(svm_model)
windows()
plot(svm_model, data=cb.train, 구매금액~나이)

# 결과값 확인
cb.test$svm_pred <- predict(svm_model, cb.test)
confusionMatrix(cb.test$svm_pred, cb.test$반품여부)
postResample(cb.test$svm_pred, cb.test$반품여부)

# 예측 확률값
cb.test$svm_pred_prob <- attr(predict(svm_model, cb.test, probability = TRUE), "probabilities")[,2]
svm_pred <- prediction(cb.test$svm_pred_prob, cb.test$반품여부)
# ROC-chart
svm_model.perf1 <- performance(svm_pred, "tpr", "fpr") # tpr : true positive rate TP/P.
svm_model.perf2 <- performance(svm_pred, "lift", "rpp") # lift : lift value P(Yhat = + | Y = +)/P(Yhat = +).
plot(svm_model.perf1, colorize=TRUE); abline(a=0, b=1, lty=3)
plot(svm_model.perf2, colorize=TRUE); abline(v=0.4, lty=3)
performance(svm_pred, "auc")@y.values[[1]]

#the best values to use for the parameters gamma and cost
#cost는 분리할 때 경계선 근처에 있는 것들을 어느정도 포함 시킬 것인지에 대한 파라매터이다.
#c가 커질 수록 좀더 정확하게 나누는 것이고 c가 작아 질 수록 모호하게 나눈다고 보면 된다.
#https://www.quora.com/What-are-C-and-gamma-with-regards-to-a-support-vector-machine
set.seed(123)
tune.svm(반품여부~성별+나이+구매금액+출연자, data=cb.train, gamma=seq(.5, .9, by=.05), cost=seq(100,1000, by=100))

##### SVM Practices with iris Data
head(iris)
plot(iris)
plot(iris$Petal.Length, iris$Petal.Width, col=iris$Species)

set.seed(123)
inTrain <- createDataPartition(y=iris$Species, p=0.6, list=FALSE)
col <- c("Petal.Length", "Petal.Width", "Species")
iris.train <- iris[inTrain, col]
iris.test <- iris[-inTrain, col]

svmfit <- svm(Species ~ ., data=iris.train, kernel="linear", cost=100, scale=FALSE)
plot(svmfit, iris.train)

tuned <- tune(svm, Species ~ ., data=iris.train, kernel="linear", ranges = list(cost=c(0.001,0.01,0.1,1,10,100)))
summary(tuned)
plot(tuned)

svmfit_tuned <- svm(Species ~ ., data=iris.train, kernel="linear", cost=10, scale=FALSE)
p <- predict(svmfit_tuned, iris.test, type="class")
plot(p)
table(p, iris.test[,"Species"])
mean(p == iris.test[, "Species"])
