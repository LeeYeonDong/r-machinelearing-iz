install.packages("rpart")
install.packages("rpart.plot")

library(tidyverse)
library(rpart)
library(rpart.plot)
library(readxl)

data <- read_csv("D:/대학원/강의/2024-1 금융신용(상담에 자주)/R Exercise_Classification Tree/Personal Loan.csv")

data %>% str()


# 데이터 분리
divide <- sample(c(rep(0, 0.7 * nrow(data)),c(rep(1, 0.3 * nrow(data)))))

train <- data[divide == 0,]
test <- data[divide == 1,]

data %>% dim()
train %>% dim()
test %>% dim()


# CART 모델 학습
model <- rpart(`Personal Loan` ~ ., data=train, method="class")

model %>% rpart.plot()

# Pruning(가지치기)
model %>% printcp() # cp 테이블 출력

# 예를 들어, cp=0.01로 설정하여 가지치기 수행
pruned_model <- model %>% prune(cp = 0.1)

# 가지치기된 모델 시각화
pruned_model %>% rpart.plot()


# 성능평가
expect <- predict(model, test[,-10], type ="class")

# install.packages("caret")
library(caret)
table(test[[10]], expect) %>% confusionMatrix()



# regression tree
data <- read_csv("D:/대학원/강의/2024-1 금융신용(상담에 자주)/R Exercise_Regression Tree/ToyotaCorolla.csv")

model <- rpart(Price ~ ., data = data, method="class")

model %>% rpart.plot()
