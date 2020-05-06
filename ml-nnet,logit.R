##########machine learning 


#####rawdata
rawdata <- read.csv("C:/Users/Administrator/Downloads/data/Data mining/uga_raw_trans_dm5.csv",head=T)
rawdata <- na.omit(rawdata)


###일반고
rawdata.nh <- rawdata[rawdata$고교유형=="3",]

samp.nh <- sample(2,nrow(rawdata.nh),replace=T,prob=c(0.7,0.3))
train.data.nh <- rawdata.nh[samp.nh==1,]
test.data.nh <- rawdata.nh[samp.nh==2,]

test.feature.var.nh <- test.data.nh[,-10]
test.class.var.nh <- test.data.nh[,10]


###과학고
rawdata.sh <- rawdata[rawdata$고교유형=="1",]

samp.sh <- sample(2,nrow(rawdata.sh),replace=T,prob=c(0.7,0.3))
train.data.sh <- rawdata.sh[samp.sh==1,]
test.data.sh <- rawdata.sh[samp.sh==2,]

test.feature.var.sh <- test.data.sh[,-11]
test.class.var.sh <- test.data.sh[,11]



#####예측데이터
new <- read.csv("C:/Users/Administrator/Downloads/data/Data mining/uga_raw_trans_dm5_GPAtest1.csv",head=T)
new <- na.omit(new)


###일반고
new.nh <- new[new$고교유형=="3",]

###과학고
new.sh <- new[new$고교유형=="1",]




install.packages("e1071")
library(e1071)
install.packages("kernlab")
library(kernlab)
install.packages("caret")
library(caret)
install.packages("rpart")
library(rpart)
install.packages("mlbench")
library(mlbench)
library(ggplot2)


formula.init <-"GPA등급~전형+수학내신+과학내신+졸업구분+
추천서_추천정도+면접유형"
formula.init <- as.formula(formula.init)


#####neural net

install.packages("nnet")
library(nnet)



###neural net - 일반고
nnet.nh <- train(formula.init, data = train.data.nh, method = "nnet")
nnet.pred.nh <- predict(nnet.nh,test.feature.var.nh,type="raw")
nnet.pred.nh <- round(nnet.pred.nh)
confusionMatrix(data=nnet.pred.nh, reference=test.class.var.nh, positive='1')


control <- trainControl(method="repeatedcv", number=10, repeats=2)
nnet.model.nh <- train(formula.init, data=train.data.nh, method="nnet", trControl=control)
importance.nnet.nh <- varImp(nnet.model.nh, scale=FALSE)

importance.nnet.nh1 <- read.csv("C:/Users/Administrator/Downloads/data/Data mining/importance.nnet.nh1.csv",head=T)

ggplot(data=importance.nnet.nh1, aes(x=reorder(X,overall),y=overall)) +
  geom_bar(stat="identity") + coord_flip() +
  ggtitle("일반고 출신 재학생 GPA등급을 결정하는 변수별 중요도 - 신경망 모형")


###nnet - new data predict
nnet.predict.nh <- predict(nnet.nh,newdata=test.data.nh)
nnet.predict.nh <- round(nnet.predict.nh)
nnet.predict.nh
table(nnet.predict.nh)


###neural net - 과학고
nnet.sh <- train(formula.init, data = train.data.sh, method = "nnet")
nnet.pred.sh <- predict(nnet.sh,test.feature.var.sh,type="raw")
nnet.pred.sh <- round(nnet.pred.sh)
confusionMatrix(data=nnet.pred.sh, reference=test.class.var.sh, positive='1')


control <- trainControl(method="repeatedcv", number=10, repeats=2)
nnet.model.sh <- train(formula.init, data=train.data.sh, method="nnet", trControl=control)
importance.nnet.sh <- varImp(nnet.model.sh, scale=FALSE)

importance.nnet.sh1 <- read.csv("C:/Users/Administrator/Downloads/data/Data mining/importance.nnet.sh1.csv",head=T)

ggplot(data=importance.nnet.sh1, aes(x=reorder(X,overall),y=overall)) +
  geom_bar(stat="identity") + coord_flip() +
  ggtitle("과학고 출신 재학생 GPA등급을 결정하는 변수별 중요도 - 신경망 모형")


###nnet - new data predict
nnet.predict.sh <- predict(nnet.sh,newdata=test.data.sh)
nnet.predict.sh <- round(nnet.predict.sh)
nnet.predict.sh
table(nnet.predict.sh)



####logit
install.packages("class")
library(class)

logit.nh <- glm(formula = formula.init, data=rawdata.nh)
logit.coef.nh <- summary(logit.nh)$coefficients
logit.coef.nh <- logit.coef.nh[,c(1,4)]
logit.coef.nh


logit.sh <- glm(formula = formula.init, data=rawdata.sh)
logit.coef.sh <- summary(logit.sh)$coefficients
logit.coef.sh <- logit.coef.sh[,c(1,4)]
logit.coef.sh
