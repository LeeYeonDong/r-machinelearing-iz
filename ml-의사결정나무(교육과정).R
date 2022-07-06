# 데이터 불러오기
library(tidyverse)
raw <- read_csv(file = "C:/대학원/논문/교육과정/data476.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
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
    과목구분명,
    과목명,
    인문계열,
    자연계열)

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
# ctree() 
library(party)
library(caret)

names(raw_tree)

# ?씤臾멸퀎
# ?쟾泥?
# data split
set.seed(1029)

intrain <- createDataPartition(y = raw_tree$吏?뿭, p=0.7, list=FALSE) 

train_tree <- raw_tree[intrain,]
test_tree <- raw_tree[-intrain,]

#
party_?씤臾?1 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 吏?뿭 + 怨좉탳?쑀?삎1 + 怨듭궗由? + x7李⑥씪諛? + 援먭낵紐? + 怨쇰ぉ紐? + ?떆?닔 + ?씠?닔?떒?쐞 + 臾멸낵 + ?씠怨?, data = train_tree)
party_?씤臾?1 %>% plot()

party_pred_?씤臾?1 <- predict(party_?씤臾?1, test_tree)
result_?씤臾?1 <- confusionMatrix(party_pred_?씤臾?1, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?1$overall[1]


# 援먯쑁?듅援? : 媛뺣궓3援?, ?닔?꽦援?, ?빐?슫??援? vs 洹몄쇅
# data split
raw_tree_援먯쑁?듅援? <- raw_tree %>% 
  mutate(援먯쑁?듅援? = ifelse(?냼?옱吏 == c("媛뺣궓援?","?꽌珥덇뎄","?넚?뙆援?","?닔?꽦援?","?빐?슫??援?"),"援먯쑁?듅援?","洹몄쇅"))

raw_tree_援먯쑁?듅援?$援먯쑁?듅援? <- raw_tree_援먯쑁?듅援?$援먯쑁?듅援? %>% as.factor()

set.seed(1029)

intrain <- createDataPartition(y = raw_tree_援먯쑁?듅援?$吏?뿭, p=0.7, list=FALSE) 

train_tree <- raw_tree_援먯쑁?듅援?[intrain,]
test_tree <- raw_tree_援먯쑁?듅援?[-intrain,]

# 援먯쑁?듅援?
party_?씤臾?2 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 援먯쑁?듅援?, data = train_tree)
party_?씤臾?2 %>% plot()

party_pred_?씤臾?2 <- predict(party_?씤臾?2, test_tree)
result_?씤臾?2 <- confusionMatrix(party_pred_?씤臾?2, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?2$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?씤臾?2_ter <- partykit:::.list.rules.party(party_?씤臾?2) %>% data.frame() %>% rownames()

party_?씤臾?2_ratio <- data.frame()

for(i in 1:length(party_?씤臾?2_ter)){
  party_?씤臾?2_ratio.tmp <- party_?씤臾?2[party_?씤臾?2_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?씤臾?2_ratio <- rbind(party_?씤臾?2_ratio, party_?씤臾?2_ratio.tmp)
}

names(party_?씤臾?2_ratio) <- party_?씤臾?2[party_?씤臾?2_ter[i]]$fitted[,3] %>% table() %>% names()
party_?씤臾?2_ratio$理쒖쥌?끂?뱶 <- party_?씤臾?2_ter

# 寃곌낵?빐?꽍
party_?씤臾?2_ratio
write.csv(party_?씤臾?2_ratio, file = "C:/???븰?썝/party_?씤臾?2_ratio.csv", row.names=FALSE)
party_?씤臾?2


# 援먯쑁?듅援? + 怨좉탳?쑀?삎1
party_?씤臾?3 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 援먯쑁?듅援? + 怨좉탳?쑀?삎1, data = train_tree)
party_?씤臾?3 %>% plot()

party_pred_?씤臾?3 <- predict(party_?씤臾?3, test_tree)
result_?씤臾?3 <- confusionMatrix(party_pred_?씤臾?3, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?3$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?씤臾?3_ter <- partykit:::.list.rules.party(party_?씤臾?3) %>% data.frame() %>% rownames()

party_?씤臾?3_ratio <- data.frame()

for(i in 1:length(party_?씤臾?3_ter)){
  party_?씤臾?3_ratio.tmp <- party_?씤臾?3[party_?씤臾?3_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?씤臾?3_ratio <- rbind(party_?씤臾?3_ratio, party_?씤臾?3_ratio.tmp)
}

names(party_?씤臾?3_ratio) <- party_?씤臾?3[party_?씤臾?3_ter[i]]$fitted[,3] %>% table() %>% names()
party_?씤臾?3_ratio$理쒖쥌?끂?뱶 <- party_?씤臾?3_ter

# 寃곌낵?빐?꽍
party_?씤臾?3_ratio
write.csv(party_?씤臾?3_ratio, file = "C:/???븰?썝/party_?씤臾?3_ratio.csv", row.names=FALSE)
party_?씤臾?3

train_tree$怨듭궗由? %>% table()


#
party_?씤臾?4 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 援먯쑁?듅援? + 怨좉탳?쑀?삎1 + 怨듭궗由?, data = train_tree)
party_?씤臾?4 %>% plot()

party_pred_?씤臾?4 <- predict(party_?씤臾?4, test_tree)
result_?씤臾?4 <- confusionMatrix(party_pred_?씤臾?4, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?4$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?씤臾?4_ter <- partykit:::.list.rules.party(party_?씤臾?4) %>% data.frame() %>% rownames()

party_?씤臾?4_ratio <- data.frame()

for(i in 1:length(party_?씤臾?4_ter)){
  party_?씤臾?4_ratio.tmp <- party_?씤臾?4[party_?씤臾?4_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?씤臾?4_ratio <- rbind(party_?씤臾?4_ratio, party_?씤臾?4_ratio.tmp)
}

names(party_?씤臾?4_ratio) <- party_?씤臾?4[party_?씤臾?4_ter[i]]$fitted[,3] %>% table() %>% names()
party_?씤臾?4_ratio$理쒖쥌?끂?뱶 <- party_?씤臾?4_ter

# 寃곌낵?빐?꽍
party_?씤臾?4_ratio
write.csv(party_?씤臾?4_ratio, file = "C:/???븰?썝/party_?씤臾?4_ratio.csv", row.names=FALSE)
party_?씤臾?4


#
party_?씤臾?5 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 吏?뿭, data = train_tree)
party_?씤臾?5 %>% plot()

party_pred_?씤臾?5 <- predict(party_?씤臾?5, test_tree)
result_?씤臾?5 <- confusionMatrix(party_pred_?씤臾?5, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?5$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?씤臾?5_ter <- partykit:::.list.rules.party(party_?씤臾?5) %>% data.frame() %>% rownames()

party_?씤臾?5_ratio <- data.frame()

for(i in 1:length(party_?씤臾?5_ter)){
  party_?씤臾?5_ratio.tmp <- party_?씤臾?5[party_?씤臾?5_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?씤臾?5_ratio <- rbind(party_?씤臾?5_ratio, party_?씤臾?5_ratio.tmp)
}

names(party_?씤臾?5_ratio) <- party_?씤臾?5[party_?씤臾?5_ter[i]]$fitted[,3] %>% table() %>% names()
party_?씤臾?5_ratio$理쒖쥌?끂?뱶 <- party_?씤臾?5_ter

# 寃곌낵?빐?꽍
party_?씤臾?5_ratio
write.csv(party_?씤臾?5_ratio, file = "C:/???븰?썝/party_?씤臾?5_ratio.csv", row.names=FALSE)
party_?씤臾?5

train_tree$援먭낵紐? %>% table()
train_tree$怨쇰ぉ援щ텇紐? %>% table()
train_tree$怨쇰ぉ紐? %>% table()


# 援먭낵 : 援먭낵紐? + 怨쇰ぉ援щ텇紐? + 怨쇰ぉ紐? + 臾멸낵 + ?씠怨?
party_?씤臾?6 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 援먭낵紐? + 臾멸낵 + ?씠怨?, data = train_tree)
party_?씤臾?6 %>% plot()

party_pred_?씤臾?6 <- predict(party_?씤臾?6, test_tree)
result_?씤臾?6 <- confusionMatrix(party_pred_?씤臾?6, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?6$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?씤臾?6_ter <- partykit:::.list.rules.party(party_?씤臾?6) %>% data.frame() %>% rownames()

party_?씤臾?6_ratio <- data.frame()

for(i in 1:length(party_?씤臾?6_ter)){
  party_?씤臾?6_ratio.tmp <- party_?씤臾?6[party_?씤臾?6_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?씤臾?6_ratio <- rbind(party_?씤臾?6_ratio, party_?씤臾?6_ratio.tmp)
}

names(party_?씤臾?6_ratio) <- party_?씤臾?6[party_?씤臾?6_ter[i]]$fitted[,3] %>% table() %>% names()
party_?씤臾?6_ratio$理쒖쥌?끂?뱶 <- party_?씤臾?6_ter

# 寃곌낵?빐?꽍
write.csv(party_?씤臾?6_ratio, file = "C:/???븰?썝/party_?씤臾?6_ratio.csv", row.names=FALSE)
party_?씤臾?6



#
party_?씤臾?7 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 援먭낵紐? + 怨쇰ぉ援щ텇紐?, data = train_tree)
party_?씤臾?7 %>% plot()

party_pred_?씤臾?7 <- predict(party_?씤臾?7, test_tree)
result_?씤臾?7 <- confusionMatrix(party_pred_?씤臾?7, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?7$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?씤臾?7_ter <- partykit:::.list.rules.party(party_?씤臾?7) %>% data.frame() %>% rownames()

party_?씤臾?7_ratio <- data.frame()

for(i in 1:length(party_?씤臾?7_ter)){
  party_?씤臾?7_ratio.tmp <- party_?씤臾?7[party_?씤臾?7_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?씤臾?7_ratio <- rbind(party_?씤臾?7_ratio, party_?씤臾?7_ratio.tmp)
}

names(party_?씤臾?7_ratio) <- party_?씤臾?7[party_?씤臾?7_ter[i]]$fitted[,3] %>% table() %>% names()
party_?씤臾?7_ratio$理쒖쥌?끂?뱶 <- party_?씤臾?7_ter

# 寃곌낵?빐?꽍
party_?씤臾?7_ratio
write.csv(party_?씤臾?7_ratio, file = "C:/???븰?썝/party_?씤臾?7_ratio.csv", row.names=FALSE)
party_?씤臾?7


#
party_?씤臾?8 <- ctree(formula = ?씤臾멸퀎?뿴 ~ 怨쇰ぉ援щ텇紐? + 臾멸낵 + ?씠怨?, data = train_tree)
party_?씤臾?8 %>% plot()

party_pred_?씤臾?8 <- predict(party_?씤臾?8, test_tree)
result_?씤臾?8 <- confusionMatrix(party_pred_?씤臾?8, test_tree$?씤臾멸퀎?뿴) 
result_?씤臾?8$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?씤臾?8_ter <- partykit:::.list.rules.party(party_?씤臾?8) %>% data.frame() %>% rownames()

party_?씤臾?8_ratio <- data.frame()

for(i in 1:length(party_?씤臾?8_ter)){
  party_?씤臾?8_ratio.tmp <- party_?씤臾?8[party_?씤臾?8_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?씤臾?8_ratio <- rbind(party_?씤臾?8_ratio, party_?씤臾?8_ratio.tmp)
}

names(party_?씤臾?8_ratio) <- party_?씤臾?8[party_?씤臾?8_ter[i]]$fitted[,3] %>% table() %>% names()
party_?씤臾?8_ratio$理쒖쥌?끂?뱶 <- party_?씤臾?8_ter

# 寃곌낵?빐?꽍
party_?씤臾?8_ratio
write.csv(party_?씤臾?8_ratio, file = "C:/???븰?썝/party_?씤臾?8_ratio.csv", row.names=FALSE)
party_?씤臾?8


train_tree %>% names()
## ?옄?뿰怨?
# 援먯쑁?듅援?
party_?옄?뿰2 <- ctree(formula = ?옄?뿰怨꾩뿴 ~ 吏?뿭, data = train_tree)
party_?옄?뿰2 %>% plot()

party_pred_?옄?뿰2 <- predict(party_?옄?뿰2, test_tree)
result_?옄?뿰2 <- confusionMatrix(party_pred_?옄?뿰2, test_tree$?옄?뿰怨꾩뿴) 
result_?옄?뿰2$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?옄?뿰2_ter <- partykit:::.list.rules.party(party_?옄?뿰2) %>% data.frame() %>% rownames()

party_?옄?뿰2_ratio <- data.frame()

for(i in 1:length(party_?옄?뿰2_ter)){
  party_?옄?뿰2_ratio.tmp <- party_?옄?뿰2[party_?옄?뿰2_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?옄?뿰2_ratio <- rbind(party_?옄?뿰2_ratio, party_?옄?뿰2_ratio.tmp)
}

names(party_?옄?뿰2_ratio) <- party_?옄?뿰2[party_?옄?뿰2_ter[i]]$fitted[,3] %>% table() %>% names()
party_?옄?뿰2_ratio$理쒖쥌?끂?뱶 <- party_?옄?뿰2_ter

# 寃곌낵?빐?꽍
party_?옄?뿰2_ratio
write.csv(party_?옄?뿰2_ratio, file = "C:/???븰?썝/party_?옄?뿰2_ratio.csv", row.names=FALSE)
party_?옄?뿰2


# 援먯쑁?듅援?
party_?옄?뿰3 <- ctree(formula = ?옄?뿰怨꾩뿴 ~ 援먯쑁?듅援? + 怨좉탳?쑀?삎1 + 怨듭궗由? , data = train_tree)
party_?옄?뿰3 %>% plot()

party_pred_?옄?뿰3 <- predict(party_?옄?뿰3, test_tree)
result_?옄?뿰3 <- confusionMatrix(party_pred_?옄?뿰3, test_tree$?옄?뿰怨꾩뿴) 
result_?옄?뿰3$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?옄?뿰3_ter <- partykit:::.list.rules.party(party_?옄?뿰3) %>% data.frame() %>% rownames()

party_?옄?뿰3_ratio <- data.frame()

for(i in 1:length(party_?옄?뿰3_ter)){
  party_?옄?뿰3_ratio.tmp <- party_?옄?뿰3[party_?옄?뿰3_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?옄?뿰3_ratio <- rbind(party_?옄?뿰3_ratio, party_?옄?뿰3_ratio.tmp)
}

names(party_?옄?뿰3_ratio) <- party_?옄?뿰3[party_?옄?뿰3_ter[i]]$fitted[,3] %>% table() %>% names()
party_?옄?뿰3_ratio$理쒖쥌?끂?뱶 <- party_?옄?뿰3_ter

# 寃곌낵?빐?꽍
party_?옄?뿰3_ratio
write.csv(party_?옄?뿰3_ratio, file = "C:/???븰?썝/party_?옄?뿰3_ratio.csv", row.names=FALSE)
party_?옄?뿰3


train_tree %>% names()

# 援먯쑁?듅援?
party_?옄?뿰4 <- ctree(formula = ?옄?뿰怨꾩뿴 ~ 怨좉탳?쑀?삎1 + 怨듭궗由? , data = train_tree)
party_?옄?뿰4 %>% plot()

party_pred_?옄?뿰4 <- predict(party_?옄?뿰4, test_tree)
result_?옄?뿰4 <- confusionMatrix(party_pred_?옄?뿰4, test_tree$?옄?뿰怨꾩뿴) 
result_?옄?뿰4$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?옄?뿰4_ter <- partykit:::.list.rules.party(party_?옄?뿰4) %>% data.frame() %>% rownames()

party_?옄?뿰4_ratio <- data.frame()

for(i in 1:length(party_?옄?뿰4_ter)){
  party_?옄?뿰4_ratio.tmp <- party_?옄?뿰4[party_?옄?뿰4_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?옄?뿰4_ratio <- rbind(party_?옄?뿰4_ratio, party_?옄?뿰4_ratio.tmp)
}

names(party_?옄?뿰4_ratio) <- party_?옄?뿰4[party_?옄?뿰4_ter[i]]$fitted[,3] %>% table() %>% names()
party_?옄?뿰4_ratio$理쒖쥌?끂?뱶 <- party_?옄?뿰4_ter

# 寃곌낵?빐?꽍
party_?옄?뿰4_ratio
write.csv(party_?옄?뿰4_ratio, file = "C:/???븰?썝/party_?옄?뿰4_ratio.csv", row.names=FALSE)
party_?옄?뿰4


# 援먯쑁?듅援?
party_?옄?뿰5 <- ctree(formula = ?옄?뿰怨꾩뿴 ~ 吏?뿭 + 怨듭궗由? , data = train_tree)
party_?옄?뿰5 %>% plot()

party_pred_?옄?뿰5 <- predict(party_?옄?뿰5, test_tree)
result_?옄?뿰5 <- confusionMatrix(party_pred_?옄?뿰5, test_tree$?옄?뿰怨꾩뿴) 
result_?옄?뿰5$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?옄?뿰5_ter <- partykit:::.list.rules.party(party_?옄?뿰5) %>% data.frame() %>% rownames()

party_?옄?뿰5_ratio <- data.frame()

for(i in 1:length(party_?옄?뿰5_ter)){
  party_?옄?뿰5_ratio.tmp <- party_?옄?뿰5[party_?옄?뿰5_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?옄?뿰5_ratio <- rbind(party_?옄?뿰5_ratio, party_?옄?뿰5_ratio.tmp)
}

names(party_?옄?뿰5_ratio) <- party_?옄?뿰5[party_?옄?뿰5_ter[i]]$fitted[,3] %>% table() %>% names()
party_?옄?뿰5_ratio$理쒖쥌?끂?뱶 <- party_?옄?뿰5_ter

# 寃곌낵?빐?꽍
party_?옄?뿰5_ratio
write.csv(party_?옄?뿰5_ratio, file = "C:/???븰?썝/party_?옄?뿰5_ratio.csv", row.names=FALSE)
party_?옄?뿰5

train_tree %>% names()
# 援먭낵紐?
party_?옄?뿰6 <- ctree(formula = ?옄?뿰怨꾩뿴 ~ 援먭낵紐?, data = train_tree)
party_?옄?뿰6 %>% plot()

party_pred_?옄?뿰6 <- predict(party_?옄?뿰6, test_tree)
result_?옄?뿰6 <- confusionMatrix(party_pred_?옄?뿰6, test_tree$?옄?뿰怨꾩뿴) 
result_?옄?뿰6$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?옄?뿰6_ter <- partykit:::.list.rules.party(party_?옄?뿰6) %>% data.frame() %>% rownames()

party_?옄?뿰6_ratio <- data.frame()

for(i in 1:length(party_?옄?뿰6_ter)){
  party_?옄?뿰6_ratio.tmp <- party_?옄?뿰6[party_?옄?뿰6_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?옄?뿰6_ratio <- rbind(party_?옄?뿰6_ratio, party_?옄?뿰6_ratio.tmp)
}

names(party_?옄?뿰6_ratio) <- party_?옄?뿰6[party_?옄?뿰6_ter[i]]$fitted[,3] %>% table() %>% names()
party_?옄?뿰6_ratio$理쒖쥌?끂?뱶 <- party_?옄?뿰6_ter

# 寃곌낵?빐?꽍
party_?옄?뿰6_ratio
write.csv(party_?옄?뿰6_ratio, file = "C:/???븰?썝/party_?옄?뿰6_ratio.csv", row.names=FALSE)
party_?옄?뿰6


# 援먭낵紐?
party_?옄?뿰7 <- ctree(formula = ?옄?뿰怨꾩뿴 ~ 援먭낵紐? + 臾멸낵 + ?씠怨?, data = train_tree)
party_?옄?뿰7 %>% plot()

party_pred_?옄?뿰7 <- predict(party_?옄?뿰7, test_tree)
result_?옄?뿰7 <- confusionMatrix(party_pred_?옄?뿰7, test_tree$?옄?뿰怨꾩뿴) 
result_?옄?뿰7$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?옄?뿰7_ter <- partykit:::.list.rules.party(party_?옄?뿰7) %>% data.frame() %>% rownames()

party_?옄?뿰7_ratio <- data.frame()

for(i in 1:length(party_?옄?뿰7_ter)){
  party_?옄?뿰7_ratio.tmp <- party_?옄?뿰7[party_?옄?뿰7_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?옄?뿰7_ratio <- rbind(party_?옄?뿰7_ratio, party_?옄?뿰7_ratio.tmp)
}

names(party_?옄?뿰7_ratio) <- party_?옄?뿰7[party_?옄?뿰7_ter[i]]$fitted[,3] %>% table() %>% names()
party_?옄?뿰7_ratio$理쒖쥌?끂?뱶 <- party_?옄?뿰7_ter

# 寃곌낵?빐?꽍
party_?옄?뿰7_ratio
write.csv(party_?옄?뿰7_ratio, file = "C:/???븰?썝/party_?옄?뿰7_ratio.csv", row.names=FALSE)
party_?옄?뿰7


# 援먭낵紐?
party_?옄?뿰8 <- ctree(formula = ?옄?뿰怨꾩뿴 ~ 怨쇰ぉ援щ텇紐? + 臾멸낵 + ?씠怨?, data = train_tree)
party_?옄?뿰8 %>% plot()

party_pred_?옄?뿰8 <- predict(party_?옄?뿰8, test_tree)
result_?옄?뿰8 <- confusionMatrix(party_pred_?옄?뿰8, test_tree$?옄?뿰怨꾩뿴) 
result_?옄?뿰8$overall[1]

# ?꽣誘몃꼸 ?끂?뱶 ratio
party_?옄?뿰8_ter <- partykit:::.list.rules.party(party_?옄?뿰8) %>% data.frame() %>% rownames()

party_?옄?뿰8_ratio <- data.frame()

for(i in 1:length(party_?옄?뿰8_ter)){
  party_?옄?뿰8_ratio.tmp <- party_?옄?뿰8[party_?옄?뿰8_ter[i]]$fitted[,3] %>% table() %>% prop.table()
  party_?옄?뿰8_ratio <- rbind(party_?옄?뿰8_ratio, party_?옄?뿰8_ratio.tmp)
}

names(party_?옄?뿰8_ratio) <- party_?옄?뿰8[party_?옄?뿰8_ter[i]]$fitted[,3] %>% table() %>% names()
party_?옄?뿰8_ratio$理쒖쥌?끂?뱶 <- party_?옄?뿰8_ter

# 寃곌낵?빐?꽍
party_?옄?뿰8_ratio
write.csv(party_?옄?뿰8_ratio, file = "C:/???븰?썝/party_?옄?뿰8_ratio.csv", row.names=FALSE)
party_?옄?뿰8