# 데이터 불러오기
library(tidyverse)
raw <- read_csv(file = "D:/대학원/논문/교육과정/data476_사업수정_과목수정_인문계.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
raw <- raw %>% data.frame()

names(raw)[13] <- c("x7차일반")

# factor
for(i in 1:length(raw)){
  raw[,i] <- factor(raw[,i],unique(raw[,i]))
}
raw %>% str()

raw_tree <- raw %>% 
  select(
    사업여부,
    고교유형1,
    공사립,
    교과명,
    과목구분명,
    과목명)

raw_tree <- raw_tree %>% 
  filter(과목명 == "화법과 작문" | 과목명 == "언어와 매체" | 과목명 == "미적분" | 과목명 == "기하" | 과목명 == "물리학Ⅰ" | 과목명 == "화학Ⅰ" | 과목명 == "생명과학Ⅰ" | 과목명 == "지구과학Ⅰ" | 과목명 == "물리학Ⅱ" | 과목명 == "화학Ⅱ" | 과목명 == "생명과학Ⅱ" | 과목명 == "지구과학Ⅱ" | 과목명 == "사회·문화" | 과목명 == "생활과 윤리" | 과목명 == "한국지리" | 과목명 == "세계지리" | 과목명 == "윤리와 사상" | 과목명 == "동아시아사" | 과목명 == "정치와 법" | 과목명 == "세계사" | 과목명 == "경제" | 과목명 == "경제 수학" )

# 교차table
install.packages("gmodels")
library(gmodels)

# 사업여부 
CrossTable(raw_tree$사업여부,raw_tree$과목구분명,format=c("SPSS"))
table(raw_tree$사업여부,raw_tree$과목구분명)

CrossTable(raw_tree$사업여부,raw_tree$과목명,format=c("SPSS"))
table(raw_tree$사업여부,raw_tree$과목명)

CrossTable(raw_tree$사업여부,raw_tree$인문계열,format=c("SPSS"))
table(raw_tree$사업여부,raw_tree$인문계열)

CrossTable(raw_tree$사업여부,raw_tree$자연계열,format=c("SPSS"))
table(raw_tree$사업여부,raw_tree$자연계열)


## 의사결정 나무
# ctree() Unbiased recursive partitioning based on permutation test 방법을 사용 . p-test를 거친  significance를 기준으로 가지치기를 할 변수를 결정하기 때문에 를 기준으로 가지치기를 될 위험이 없어 별도로 가지치기(Pruning) 할 필요가 없다. 31개까지로 제한돼있다
install.packages("party")
install.packages("caret")
install.packages("partykit")
library(partykit)
library(party)
library(caret)

names(raw_tree)
raw_tree %>% str()

# 의사결정나무-인문1
party_인문1 <- ctree(formula = 인문계열 ~ 사업여부 + 고교유형1 + 공사립 + 교과명 + 과목구분명, data = raw_tree)
party_인문1 %>% plot()

party_pred_인문1 <- predict(party_인문1, raw_tree)
result_인문1 <- confusionMatrix(party_pred_인문1, raw_tree$인문계열) 
result_인문1$overall[1]

# 인문1 분류 ratio
party_인문1_ter <- partykit:::.list.rules.party(party_인문1) %>% data.frame() %>% rownames()

party_인문1_ratio <- data.frame()

for(i in 1:length(party_인문1_ter)){
  party_인문1_ratio.tmp <- party_인문1[party_인문1_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_인문1_ratio <- rbind(party_인문1_ratio, party_인문1_ratio.tmp)
}

names(party_인문1_ratio) <- party_인문1[party_인문1_ter[i]]$fitted[,3] %>% table() %>% names()
party_인문1_ratio$노드 <- party_인문1_ter

# 인문1 분류 비율 저장
party_인문1_ratio
write.csv(party_인문1_ratio, file = "C:/대학원/논문/교육과정/party_인문1_ratio.csv", row.names=FALSE)
party_인문1


# 의사결정나무-인문2
party_인문2 <- ctree(formula = 인문계열 ~ 사업여부 + 고교유형1 + 공사립 + 교과명, data = raw_tree)
party_인문2 %>% plot()

party_pred_인문2 <- predict(party_인문2, raw_tree)
result_인문2 <- confusionMatrix(party_pred_인문2, raw_tree$인문계열) 
result_인문2$overall[1]

# 인문2 분류 ratio
party_인문2_ter <- partykit:::.list.rules.party(party_인문2) %>% data.frame() %>% rownames()

party_인문2_ratio <- data.frame()

for(i in 1:length(party_인문2_ter)){
  party_인문2_ratio.tmp <- party_인문2[party_인문2_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_인문2_ratio <- rbind(party_인문2_ratio, party_인문2_ratio.tmp)
}

names(party_인문2_ratio) <- party_인문2[party_인문2_ter[i]]$fitted[,3] %>% table() %>% names()
party_인문2_ratio$노드 <- party_인문2_ter

# 인문2 분류 비율 저장
party_인문2_ratio
write.csv(party_인문2_ratio, file = "C:/대학원/논문/교육과정/party_인문2_ratio.csv", row.names=FALSE)
party_인문2


# 의사결정나무-인문3
party_인문3 <- ctree(formula = 인문계열 ~ 사업여부 + 고교유형1 + 교과명, data = raw_tree)
party_인문3 %>% plot()

party_pred_인문3 <- predict(party_인문3, raw_tree)
result_인문3 <- confusionMatrix(party_pred_인문3, raw_tree$인문계열) 
result_인문3$overall[1]

# 인문3 분류 ratio
party_인문3_ter <- partykit:::.list.rules.party(party_인문3) %>% data.frame() %>% rownames()

party_인문3_ratio <- data.frame()

for(i in 1:length(party_인문3_ter)){
  party_인문3_ratio.tmp <- party_인문3[party_인문3_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_인문3_ratio <- rbind(party_인문3_ratio, party_인문3_ratio.tmp)
}

names(party_인문3_ratio) <- party_인문3[party_인문3_ter[i]]$fitted[,3] %>% table() %>% names()
party_인문3_ratio$노드 <- party_인문3_ter

# 인문3 분류 비율 저장
party_인문3_ratio
write.csv(party_인문3_ratio, file = "C:/대학원/논문/교육과정/party_인문3_ratio.csv", row.names=FALSE)
party_인문3



# 의사결정나무-인문4
party_인문4 <- ctree(formula = 인문계열 ~ 사업여부 + 과목구분명, data = raw_tree)
party_인문4 %>% plot()

party_pred_인문4 <- predict(party_인문4, raw_tree)
result_인문4 <- confusionMatrix(party_pred_인문4, raw_tree$인문계열) 
result_인문4$overall[1]

# 인문4 분류 ratio
party_인문4_ter <- partykit:::.list.rules.party(party_인문4) %>% data.frame() %>% rownames()

party_인문4_ratio <- data.frame()

for(i in 1:length(party_인문4_ter)){
  party_인문4_ratio.tmp <- party_인문4[party_인문4_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_인문4_ratio <- rbind(party_인문4_ratio, party_인문4_ratio.tmp)
}

names(party_인문4_ratio) <- party_인문4[party_인문4_ter[i]]$fitted[,3] %>% table() %>% names()
party_인문4_ratio$노드 <- party_인문4_ter

# 인문4 분류 비율 저장
party_인문4_ratio
write.csv(party_인문4_ratio, file = "C:/대학원/논문/교육과정/party_인문4_ratio.csv", row.names=FALSE)
party_인문4



# 의사결정나무-인문5
party_인문5 <- ctree(formula = 인문계열 ~ 과목구분명, data = raw_tree)
party_인문5 %>% plot()

party_pred_인문5 <- predict(party_인문5, raw_tree)
result_인문5 <- confusionMatrix(party_pred_인문5, raw_tree$인문계열) 
result_인문5$overall[1]

# 인문5 분류 ratio
party_인문5_ter <- partykit:::.list.rules.party(party_인문5) %>% data.frame() %>% rownames()

party_인문5_ratio <- data.frame()

for(i in 1:length(party_인문5_ter)){
  party_인문5_ratio.tmp <- party_인문5[party_인문5_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_인문5_ratio <- rbind(party_인문5_ratio, party_인문5_ratio.tmp)
}

names(party_인문5_ratio) <- party_인문5[party_인문5_ter[i]]$fitted[,3] %>% table() %>% names()
party_인문5_ratio$노드 <- party_인문5_ter

# 인문5 분류 비율 저장
party_인문5_ratio
write.csv(party_인문5_ratio, file = "C:/대학원/논문/교육과정/party_인문5_ratio.csv", row.names=FALSE)
party_인문5


##### 인문계열은 변수를 뭘 집어넣어도 모델 설명력이 비슷비슷하다. 대부분 상경계열 진학이라 이러한 결과가 나왔다고 추정한다.


# 의사결정나무-자연1
party_자연1 <- ctree(formula = 자연계열 ~ 사업여부 + 고교유형1 + 공사립 + 교과명 + 과목구분명, data = raw_tree)
party_자연1 %>% plot()

party_pred_자연1 <- predict(party_자연1, raw_tree)
result_자연1 <- confusionMatrix(party_pred_자연1, raw_tree$자연계열) 
result_자연1$overall[1]

# 자연1 분류 ratio
party_자연1_ter <- partykit:::.list.rules.party(party_자연1) %>% data.frame() %>% rownames()

party_자연1_ratio <- data.frame()

for(i in 1:length(party_자연1_ter)){
  party_자연1_ratio.tmp <- party_자연1[party_자연1_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_자연1_ratio <- rbind(party_자연1_ratio, party_자연1_ratio.tmp)
}

names(party_자연1_ratio) <- party_자연1[party_자연1_ter[i]]$fitted[,3] %>% table() %>% names()
party_자연1_ratio$노드 <- party_자연1_ter

# 자연1 분류 비율 저장
party_자연1_ratio
write.csv(party_자연1_ratio, file = "C:/대학원/논문/교육과정/party_자연1_ratio.csv", row.names=FALSE)
party_자연1


# 의사결정나무-자연2
party_자연2 <- ctree(formula = 자연계열 ~ 사업여부 + 고교유형1 + 공사립 + 교과명, data = raw_tree)
party_자연2 %>% plot()

party_pred_자연2 <- predict(party_자연2, raw_tree)
result_자연2 <- confusionMatrix(party_pred_자연2, raw_tree$자연계열) 
result_자연2$overall[1]

# 자연2 분류 ratio
party_자연2_ter <- partykit:::.list.rules.party(party_자연2) %>% data.frame() %>% rownames()

party_자연2_ratio <- data.frame()

for(i in 1:length(party_자연2_ter)){
  party_자연2_ratio.tmp <- party_자연2[party_자연2_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_자연2_ratio <- rbind(party_자연2_ratio, party_자연2_ratio.tmp)
}

names(party_자연2_ratio) <- party_자연2[party_자연2_ter[i]]$fitted[,3] %>% table() %>% names()
party_자연2_ratio$노드 <- party_자연2_ter

# 자연2 분류 비율 저장
party_자연2_ratio
write.csv(party_자연2_ratio, file = "C:/대학원/논문/교육과정/party_자연2_ratio.csv", row.names=FALSE)
party_자연2


# 의사결정나무-자연3
party_자연3 <- ctree(formula = 자연계열 ~ 사업여부 + 고교유형1 + 교과명 + 과목구분명, data = raw_tree)
party_자연3 %>% plot()

party_pred_자연3 <- predict(party_자연3, raw_tree)
result_자연3 <- confusionMatrix(party_pred_자연3, raw_tree$자연계열) 
result_자연3$overall[1]

# 자연3 분류 ratio
party_자연3_ter <- partykit:::.list.rules.party(party_자연3) %>% data.frame() %>% rownames()

party_자연3_ratio <- data.frame()

for(i in 1:length(party_자연3_ter)){
  party_자연3_ratio.tmp <- party_자연3[party_자연3_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_자연3_ratio <- rbind(party_자연3_ratio, party_자연3_ratio.tmp)
}

names(party_자연3_ratio) <- party_자연3[party_자연3_ter[i]]$fitted[,3] %>% table() %>% names()
party_자연3_ratio$노드 <- party_자연3_ter

# 자연3 분류 비율 저장
party_자연3_ratio
write.csv(party_자연3_ratio, file = "C:/대학원/논문/교육과정/party_자연3_ratio.csv", row.names=FALSE)
party_자연3


# 의사결정나무-자연4
party_자연4 <- ctree(formula = 자연계열 ~ 사업여부 + 고교유형1 + 과목구분명, data = raw_tree)
party_자연4 %>% plot()

party_pred_자연4 <- predict(party_자연4, raw_tree)
result_자연4 <- confusionMatrix(party_pred_자연4, raw_tree$자연계열) 
result_자연4$overall[1]

# 자연4 분류 ratio
party_자연4_ter <- partykit:::.list.rules.party(party_자연4) %>% data.frame() %>% rownames()

party_자연4_ratio <- data.frame()

for(i in 1:length(party_자연4_ter)){
  party_자연4_ratio.tmp <- party_자연4[party_자연4_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_자연4_ratio <- rbind(party_자연4_ratio, party_자연4_ratio.tmp)
}

names(party_자연4_ratio) <- party_자연4[party_자연4_ter[i]]$fitted[,3] %>% table() %>% names()
party_자연4_ratio$노드 <- party_자연4_ter

# 자연4 분류 비율 저장
party_자연4_ratio
write.csv(party_자연4_ratio, file = "C:/대학원/논문/교육과정/party_자연4_ratio.csv", row.names=FALSE)
party_자연4


# 의사결정나무-자연5
party_자연5 <- ctree(formula = 자연계열 ~ 사업여부 + 교과명, data = raw_tree)
party_자연5 %>% plot()

party_pred_자연5 <- predict(party_자연5, raw_tree)
result_자연5 <- confusionMatrix(party_pred_자연5, raw_tree$자연계열) 
result_자연5$overall[1]

# 자연5 분류 ratio
party_자연5_ter <- partykit:::.list.rules.party(party_자연5) %>% data.frame() %>% rownames()

party_자연5_ratio <- data.frame()

for(i in 1:length(party_자연5_ter)){
  party_자연5_ratio.tmp <- party_자연5[party_자연5_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_자연5_ratio <- rbind(party_자연5_ratio, party_자연5_ratio.tmp)
}

names(party_자연5_ratio) <- party_자연5[party_자연5_ter[i]]$fitted[,3] %>% table() %>% names()
party_자연5_ratio$노드 <- party_자연5_ter

# 자연5 분류 비율 저장
party_자연5_ratio
write.csv(party_자연5_ratio, file = "C:/대학원/논문/교육과정/party_자연5_ratio.csv", row.names=FALSE)
party_자연5