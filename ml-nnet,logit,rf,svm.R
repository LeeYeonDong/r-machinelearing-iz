##########machine learning 

rawdata <- read.csv("C:/Users/Administrator/Downloads/data/Data mining/uga_raw_trans_dm5.csv",head=T)
rawdata <- na.omit(rawdata)

samp <- sample(2,nrow(rawdata),replace=T,prob=c(0.7,0.3))
train.data <- rawdata[samp==1,]
test.data <- rawdata[samp==2,]

test.feature.var <- test.data[,-13]
test.class.var <- test.data[,13]

new <- read.csv("C:/Users/Administrator/Downloads/data/Data mining/uga_raw_trans_dm5_test.csv",head=T)
new <- na.omit(new)

install.packages("caret")
library(caret)

library(parallel)


####logistic

formula.init <-"최종결과~전형+고교유형+고교유형세부+수학내신+과학내신+전체내신+졸업구분+
                    추천서_진학의지+추천서_추천정도+면접유형"
formula.init <- as.formula(formula.init)


install.packages("class")
library(class)


logit1 <- glm(formula = formula.init, data = train.data)
logit1

logit.pred <- predict(logit1,test.data,type="response")
logit.pred <- round(logit.pred)
confusionMatrix(data=logit.pred, reference = test.class.var, positive="1")


formula <- "최종결과~전형+고교유형+고교유형세부+수학내신+과학내신+전체내신+졸업구분+
                    추천서_진학의지+추천서_추천정도+면접유형"
formula <- as.formula("최종결과~전형+고교유형+고교유형세부+수학내신+과학내신+전체내신+졸업구분+
                    추천서_진학의지+추천서_추천정도+면접유형")
control <- trainControl(method="repeatedcv", number=10, repeats=2)
logit.model <- train(formula, data=train.data,method="glm", trControl=control)
logit.importance <- varImp(logit.model, scale=FALSE)
plot(logit.importance)


###logistic - new data predict
logit.predict <- predict(logit1,newdata=new)
logit.predict <- round(logit.predict)
logit.predict 
table(logit.predict)


####SVM

install.packages("e1071")
library(e1071)
install.packages("kernlab")
library(kernlab)



formula.init <-"최종결과~전형+고교유형+고교유형세부+수학내신+과학내신+전체내신+졸업구분+
                    추천서_진학의지+추천서_추천정도+면접유형"
formula.init <- as.formula(formula.init)

svm1 <- svm(formula=formula.init, data=train.data, type="C-classification")
svm1


svm.pred <- predict(svm1, test.feature.var)
confusionMatrix(data = svm.pred, reference = test.class.var, positive="1")


control <- trainControl(method="repeatedcv", number=10, repeats=2)
svm.model <- train(formula.init, data=train.data, method="svmRadial", trControl=control)
importance.svm <- varImp(svm.model, scale=FALSE)
plot(importance.svm)


####SVM - new data predict
svm.predict <- predict(svm1,newdata=new)
svm.predict
table(svm.predict)



####Random Forest

install.packages("randomForest")
library(randomForest)
library(e1071) ##최적화 모델 찾기


formula.init <-"최종결과~전형+고교유형+고교유형세부+수학내신+과학내신+전체내신+졸업구분+
                    추천서_진학의지+추천서_추천정도+면접유형"
formula.init <- as.formula(formula.init)


nodesize.vals <- c(2,3,4,5)
ntree.vals <- c(200, 500, 1000, 2000)
tuning.results <- tune.randomForest(formula.init, data=train.data, mtry=3, nodesize=nodesize.vals, ntree=ntree.vals)
tuning.results


###result
Parameter tuning of ‘randomForest’:
  
  - sampling method: 10-fold cross validation 

- best parameters:
  nodesize mtry ntree
4    3   500

- best performance: 0.08850856 



rf1 <- randomForest(formula.init,data=train.data, importance=TRUE, ntree=500, nodesize=4, mtry=3)
rf1


rf1.pred <- predict(rf1,test.feature.var,type="class")
rf1.pred <- round(rf1.pred)
confusionMatrix(data=rf1.pred,reference=test.class.var,positive='1')


names(rf1)
importance <- rf1$importance
importance <- importance[,1]


control <- trainControl(method="repeatedcv", number=10, repeats=2)
rf.model <- train(formula.init, data=train.data, method="rf", trControl=control)
importance.rf <- varImp(rf.model, scale=FALSE)
barplot(importance,beside=T,horiz=T,xlab="importance",axisnames = T,las=1,col="blue2")


####rf - new data predict
rf.predict <- predict(rf1,newdata=new)
rf.predict <- round(rf.predict)
rf.predict
table(rf.predict)



####neural net

install.packages("nnet")
library(nnet)


formula.init <-"최종결과~전형+고교유형+고교유형세부+수학내신+과학내신+전체내신+졸업구분+
추천서_진학의지+추천서_추천정도+면접유형"
formula.init <- as.formula(formula.init)


nnet1 <- train(formula.init, data = train.data, method = "nnet")
nnet1.pred <- predict(nnet1,test.feature.var,type="raw")
nnet1.pred <- round(nnet1.pred)
confusionMatrix(data=nnet1.pred, reference=test.class.var, positive='1')


control <- trainControl(method="repeatedcv", number=10, repeats=2)
nnet.model <- train(formula.init, data=train.data, method="nnet", trControl=control)
importance.nnet <- varImp(nnet.model, scale=FALSE)
plot(importance.nnet)


####nnet - new data predict
nnet.predict <- predict(nnet1,newdata=new)
nnet.predict <- round(rf.predict)
nnet.predict
table(nnet.predict)
