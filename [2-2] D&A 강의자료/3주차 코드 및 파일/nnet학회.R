##학회 준비 
setwd("C:/future/2학년 2학기 학회/3주차 코드 및 파일")

## 데이터 -목적 :분류(이분/다분)성별,연령- 변수(eda)- 모델() -데이터 전처리과정
#train 데이터로 분류를 나름 정확하게 했어 /test ##train데이터에 과적합이 된다. >해답 :cv
#caret !! kaggle :선배들의 발자취. 예시 

# Load Pacakages
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(ROCR)) install.packages("ROCR"); library(ROCR)
if(!require(nnet)) install.packages("nnet"); library(nnet)
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(NeuralNetTools)) install.packages("NeuralNetTools"); library(NeuralNetTools)
if(!require(neuralnet)) install.packages("neuralnet"); library(neuralnet)

# Load Data
cb <- read.delim("Hshopping.txt", fileEncoding = "EUC-KR", stringsAsFactors=FALSE)
cb$반품여부 <- factor(cb$반품여부) # 명목형값예측일경우

# set seed & data split
set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

##nnet Modeling
nnet_model <- nnet(반품여부~성별+나이+구매금액+출연자, 
                       data=cb.train, size=3,maxit =200,decay=5e-4) #size : 은닉층값 설정 
                                                                    #maxit :iter 조정
summary(nnet_model)

source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nnet_model)

garson(nnet_model) #변수 중요도 

confusionMatrix(predict(nnet_model, newdata=cb.test, type="class"), cb.test$반품여부)

nnet_pred<-ROCR::prediction(predict(nnet_model, newdata=cb.test, type="raw"), cb.test$반품여부)
nnet_model.perf1 <-performance(nnet_pred, "tpr", "fpr") # ROC-chart
nnet_model.perf2 <-performance(nnet_pred, "lift", "rpp") # Lift chart
plot(nnet_model.perf1, colorize=TRUE); plot(nnet_model.perf2, colorize=TRUE)

#nnet feature
#Weight decay #정규화방법 :가중치가 클수 큰 페널티를 부가함 
             #사용법: nnet()함수에서 decay에작은값(ex: 5e-4)을지정
#Initial random weights  #초기 가중치 설정
                        #사용법 : nnet()함수에서 range에 임의값(ex:0.1) 지정시 -0.1~0.1로 설정 

##neuralnet 시각화에 특화된 모델
cb <- read.delim("Hshopping.txt", fileEncoding = "EUC-KR", stringsAsFactors=FALSE)

# neuralnet 함수의 경우 목표값이 numeric이여야함. 따라서 factor화 안함. 
cb$반품여부 <-as.numeric(cb$반품여부)

set.seed(1)
inTrain <- createDataPartition(y=cb$반품여부, p=0.6, list=FALSE)
cb.train <- cb[inTrain,]
cb.test <- cb[-inTrain,]

nnet2_model <-neuralnet(반품여부 ~ 성별+나이+구매금액+출연자, data=cb.train, hidden=3, threshold=0.01)
# hidden: # of hidden nodes 
# threshold: if change in error at a given step of iteration is
# less than the threshold, the model will stop further optimization.
# linear.output: If act.fct('logistic' or 'tanh') should not be appliedto the output neurons set linear output to TRUE, otherwise to FALSE(default = TRUE)
# stepmax: the maximum steps for the training of the neural network..
  
plot(nnet2_model)

par(mfrow=c(1,1))
gwplot(nnet2_model, selected.covariate= "성별", min=-3, max=6)
gwplot(nnet2_model, selected.covariate= "나이", min=-3, max=6)
gwplot(nnet2_model, selected.covariate= "구매금액", min=-3, max=6) 
#일반화 가중치의 분산(GW)이 0에 가까움  >결과에 영향 없음
gwplot(nnet2_model, selected.covariate= "출연자", min=-3, max=6)
#일반화 가중치의 분산이 1보다 큼 >비선형효과 있음 
par(mfrow=c(1,1))

cb.test$nnet2_pred_prob <-compute(nnet2_model, covariate=cb.test[, c(2:5)])$net.result
cb.test$nnet2_pred <-ifelse(cb.test$nnet2_pred_prob > 0.5, 1, 0)
confusionMatrix(cb.test$nnet2_pred, cb.test$반품여부)

