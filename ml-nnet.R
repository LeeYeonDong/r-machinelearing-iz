studata <- read.csv("C:/Users/Administrator/Desktop/18년도 종단분석 연구/2018 종단분석 기본정보 + 추가 변수 0129.csv",head=T)
studata <- studata[studata$졸업예정==1,]
str(studata)


formula.init <-"진로현황~인턴쉽_신물질과학+인턴쉽_정보통신융합+인턴쉽_로봇공학+인턴쉽_에너지공학+인턴쉽_뇌인지과학
+인턴쉽_뉴바이올로지+수상경력+봉사시간+희망진로1+희망진로2"
formula.init <- as.formula(formula.init)

install.packages("neuralnet")
library(neuralnet)


nn.stu <- neuralnet(formula=formula.init,data=studata,hidden=3)
plot(nn.stu)
nn.stu


####################

samp.stu <- sample(2,nrow(studata),replace=T,prob=c(0.7,0.3))
train.stu <- studata[samp.stu==1,]
test.stu <- studata[samp.stu==2,]

test.feature.var.stu <- test.stu[,-29]
test.class.var.stu <- test.stu[,29]

feature.var.stu <- studata[,-29]
class.var.stu <- studata[,29]


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
install.packages("ggplot2")
library(ggplot2)



#####neural net

install.packages("nnet")
library(nnet)


nnet.stu <- train(formula.init, data = studata, method = "nnet")
nnet.pred.stu <- predict(nnet.stu,feature.var.stu,type="raw")
nnet.pred.stu <- round(nnet.pred.stu)
confusionMatrix(data=nnet.pred.stu, reference=class.var.stu, positive='0')

plot(nnet.stu)
