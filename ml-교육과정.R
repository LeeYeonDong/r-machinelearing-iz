# 데이터 불러오기
library(tidyverse)
raw <- read_csv(file = "C:/대학원/논문/교육과정/data476.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
raw <- raw %>% data.frame()

names(raw)[13] <- c("x7차일반")

# 교과명-수학 / 과목구분 - 일반선택, 전문교과1 / 인문계열
교과_수학 <- raw %>% 
  filter(교과명 == c("수학")) %>% 
  filter(과목구분명 %in% c("일반선택","전문교과Ⅰ","진로선택"))

write.csv(교과_수학, file = "C:/대학원/교과_수학.csv", row.names=FALSE)

교과_수학 <- read_csv(file = "C:/대학원/교과_수학.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

# factor
교과_수학 <- 교과_수학 %>% data.frame()

for(i in 1:length(교과_수학)){
  교과_수학[,i] <- factor(교과_수학[,i],unique(교과_수학[,i]))
}

교과_수학 <- 교과_수학 %>% 
  select(
    고교명,
    지역, 소재지,
    고교유형1,
    공사립, 사업여부,
    학년, 학기,
    x7차일반,
    교과명, 과목구분명, 과목명,
    시수, 이수단위,
    문과, 이과, 인문계열, 자연계열)

교과_수학$학년 <- 교과_수학$학년 %>% as.numeric()
교과_수학$학기 <- 교과_수학$학기 %>% as.numeric()
교과_수학$시수 <- 교과_수학$시수 %>% as.numeric()
교과_수학$이수단위 <- 교과_수학$이수단위 %>% as.numeric()

# 빈도표
library(gmodels)

library(descr)

freq(교과_수학$과목구분명, plot = TRUE) %>%
  as.data.frame() %>% 
  arrange(desc(Frequency)) %>% 
  filter(Frequency != 0)

freq(교과_수학$과목명, plot = TRUE) %>%
  as.data.frame() %>% 
  arrange(desc(Frequency)) %>% 
  filter(Frequency != 0)



# 변수선택
step(logit_교과_수학,
     scope = list(lower = ~1,
                  upper = ~지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위),
     direction = "both")

# 로지스틱 회귀분석
logit_교과_수학 <- glm(formula = 인문계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학, family = binomial)

logit_교과_수학 %>% summary()

# cross table
table(교과_수학$인문계열, 교과_수학$지역)
table2 <- table(교과_수학$인문계열, 교과_수학$고교유형1)
write.csv(table2, file = "C:/대학원/table2.csv", row.names=FALSE)

table3 <- table(교과_수학$인문계열, 교과_수학$과목명)
write.csv(table3, file = "C:/대학원/table3.csv", row.names=FALSE)

table4 <- table(교과_수학$인문계열, 교과_수학$사업여부)
write.csv(table4, file = "C:/대학원/table4.csv", row.names=FALSE)

table5 <- table(교과_수학$인문계열, 교과_수학$시수)
write.csv(table5, file = "C:/대학원/table5.csv", row.names=FALSE)

# 의사결정나무
library(partykit)
library(party)
library(caret)

set.seed(1029)

교과_수학 %>% names()

party_교과_수학 <- ctree(formula = 인문계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학)
party_교과_수학 %>% plot()
party_교과_수학 %>% summary()

# 터미널 노드 ratio
party_교과_수학_ter <- partykit:::.list.rules.party(party_교과_수학) %>% data.frame() %>% rownames()

party_교과_수학_ratio <- data.frame()

for(i in 1:length(party_교과_수학_ter)){
  party_교과_수학_ratio.tmp <- party_교과_수학[party_교과_수학_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_교과_수학_ratio <- rbind(party_교과_수학_ratio, party_교과_수학_ratio.tmp)
}

names(party_교과_수학_ratio) <- party_교과_수학[party_교과_수학_ter[i]]$fitted[,3] %>% table() %>% names()
party_교과_수학_ratio$최종노드 <- party_교과_수학_ter

# 결과해석
party_교과_수학_ratio
write.csv(party_교과_수학_ratio, file = "C:/대학원/party_교과_수학_ratio.csv", row.names=FALSE)
party_교과_수학


# 랜덤포레스트
library(randomForest)
                   
set.seed(1029)

rf_교과_수학 <- randomForest(인문계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학, mtry = 7, ntree = 500, importance = TRUE)

# ntree에 따른 에러값
plot(rf_교과_수학$err.rate[, 1], col = "blue")

# 변수별 중요도
rf_교과_수학 %>% importance

varImpPlot(rf_교과_수학, type = 2, pch = 19, col = 1, cex = 1, main = "")


# logit TREE RF 성능비교
party_교과_수학 <- ctree(formula = 인문계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학)

logit_pred <- predict(logit_교과_수학, newdata = 교과_수학, type = "response")
tree_pred <- predict(party_교과_수학, newdata = 교과_수학, type = "response")
rf_pred <- predict(rf_교과_수학, newdata = 교과_수학, type = "response")

library(Epi)
ROC(test = logit_pred, stat = 교과_수학$인문계열, plot="ROC", AUC = TRUE, main="logistic regression")
ROC(test = tree_pred, stat = 교과_수학$인문계열, plot="ROC", AUC = TRUE, main="decision tree")
ROC(test = rf_pred, stat = 교과_수학$인문계열, plot="ROC", AUC = TRUE, main="random forest")



## 자연계열
# 로지스틱 회귀분석
logit_교과_수학 <- glm(formula = 자연계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학, family = binomial)

logit_교과_수학 %>% summary()

# cross table
table <- table(교과_수학$자연계열, 교과_수학$지역)
write.csv(table, file = "C:/대학원/table.csv", row.names=FALSE)

table1 <- table(교과_수학$자연계열, 교과_수학$고교유형1)
write.csv(table1, file = "C:/대학원/table1.csv", row.names=FALSE)

table2 <- table(교과_수학$자연계열, 교과_수학$공사립)
write.csv(table2, file = "C:/대학원/table2.csv", row.names=FALSE)

table3 <- table(교과_수학$자연계열, 교과_수학$사업여부)
write.csv(table3, file = "C:/대학원/table3.csv", row.names=FALSE)

# 의사결정나무
library(partykit)
library(party)
library(caret)

set.seed(1029)

교과_수학 %>% names()

party_교과_수학 <- ctree(formula = 자연계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학)
party_교과_수학 %>% plot()


# 터미널 노드 ratio
party_교과_수학_ter <- partykit:::.list.rules.party(party_교과_수학) %>% data.frame() %>% rownames()

party_교과_수학_ratio <- data.frame()

for(i in 1:length(party_교과_수학_ter)){
  party_교과_수학_ratio.tmp <- party_교과_수학[party_교과_수학_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_교과_수학_ratio <- rbind(party_교과_수학_ratio, party_교과_수학_ratio.tmp)
}

names(party_교과_수학_ratio) <- party_교과_수학[party_교과_수학_ter[i]]$fitted[,3] %>% table() %>% names()
party_교과_수학_ratio$최종노드 <- party_교과_수학_ter

# 결과해석
party_교과_수학_ratio
write.csv(party_교과_수학_ratio, file = "C:/대학원/party_교과_수학_ratio.csv", row.names=FALSE)
party_교과_수학


# 랜덤포레스트
library(randomForest)

set.seed(1029)

rf_교과_수학 <- randomForest(자연계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학, mtry = 7, ntree = 500, importance = TRUE)

# ntree에 따른 에러값
plot(rf_교과_수학$err.rate[, 1], col = "blue")

# 변수별 중요도
rf_교과_수학 %>% importance

varImpPlot(rf_교과_수학, type = 2, pch = 19, col = 1, cex = 1, main = "")




# logit TREE RF 성능비교
party_교과_수학 <- ctree(formula = 자연계열 ~ 지역 + 고교유형1 + 공사립 + 사업여부 + 학년 + 학기 + 과목구분명 + 과목명 + 시수 + 이수단위, data = 교과_수학)

logit_pred <- predict(logit_교과_수학, newdata = 교과_수학, type = "response")
tree_pred <- predict(party_교과_수학, newdata = 교과_수학, type = "response")
rf_pred <- predict(rf_교과_수학, newdata = 교과_수학, type = "response")

library(Epi)
ROC(test = logit_pred, stat = 교과_수학$자연계열, plot="ROC", AUC = TRUE, main="logistic regression")
ROC(test = tree_pred, stat = 교과_수학$자연계열, plot="ROC", AUC = TRUE, main="decision tree")
ROC(test = rf_pred, stat = 교과_수학$자연계열, plot="ROC", AUC = TRUE, main="random forest")

