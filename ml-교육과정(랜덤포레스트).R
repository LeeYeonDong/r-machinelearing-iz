# 데이터 불러오기
# 인문
library(tidyverse)
인문 <- read_csv(file = "D:/대학원/논문/교육과정/data476_사업수정_과목수정_인문계.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
인문 <- 인문 %>% data.frame()

names(인문)[13] <- c("x7차일반")

인문 <- 인문 %>% 
  select(
    사업여부,
    고교유형1,
    공사립,
    교과명,
    과목구분명,
    과목명,
    인문계열)

# 인문 <- 인문 %>% 
  # filter(과목명 == "화법과 작문" | 과목명 == "언어와 매체" | 과목명 == "미적분" | 과목명 == "기하" | 과목명 == "물리학Ⅰ" | 과목명 == "화학Ⅰ" | 과목명 == "생명과학Ⅰ" | 과목명 == "지구과학Ⅰ" | 과목명 == "물리학Ⅱ" | 과목명 == "화학Ⅱ" | 과목명 == "생명과학Ⅱ" | 과목명 == "지구과학Ⅱ" | 과목명 == "사회·문화" | 과목명 == "생활과 윤리" | 과목명 == "한국지리" | 과목명 == "세계지리" | 과목명 == "윤리와 사상" | 과목명 == "동아시아사" | 과목명 == "정치와 법" | 과목명 == "세계사" | 과목명 == "경제" | 과목명 == "경제 수학" )

# factor
for(i in 1:length(인문)){
  인문[,i] <- factor(인문[,i],unique(인문[,i]))
}
인문 %>% str()


# 랜덤포레스트
library(randomForest)

인문 %>% names()
unique(인문$과목명) %>% length()

set.seed(1029)
memory.size()

memory.limit()
memory.limit(5000)

인문_rf_result <- randomForest(인문계열 ~ 사업여부 + 고교유형1 + 공사립 + 과목구분명, data = 인문, mtry = 4, ntree = 1000, importance = TRUE)

# ntree에 따른 에러값
plot(인문_rf_result$err.rate[, 1], col = "blue")

# 변수별 중요도
인문_rf_result %>% importance

varImpPlot(인문_rf_result, type = 2, pch = 19, col = 1, cex = 1, main = "")


# 자연
자연 <- read_csv(file = "D:/대학원/논문/교육과정/data476_사업수정_과목수정_자연계.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
자연 <- 자연 %>% data.frame()

names(자연)[13] <- c("x7차일반")

자연 <- 자연 %>% 
  select(
    사업여부,
    고교유형1,
    공사립,
    교과명,
    과목구분명,
    과목명,
    자연계열)

# 인문 <- 인문 %>% 
# filter(과목명 == "화법과 작문" | 과목명 == "언어와 매체" | 과목명 == "미적분" | 과목명 == "기하" | 과목명 == "물리학Ⅰ" | 과목명 == "화학Ⅰ" | 과목명 == "생명과학Ⅰ" | 과목명 == "지구과학Ⅰ" | 과목명 == "물리학Ⅱ" | 과목명 == "화학Ⅱ" | 과목명 == "생명과학Ⅱ" | 과목명 == "지구과학Ⅱ" | 과목명 == "사회·문화" | 과목명 == "생활과 윤리" | 과목명 == "한국지리" | 과목명 == "세계지리" | 과목명 == "윤리와 사상" | 과목명 == "동아시아사" | 과목명 == "정치와 법" | 과목명 == "세계사" | 과목명 == "경제" | 과목명 == "경제 수학" )

# factor
for(i in 1:length(자연)){
  자연[,i] <- factor(자연[,i],unique(자연[,i]))
}
자연 %>% str()


# 랜덤포레스트
library(randomForest)

자연 %>% names()
unique(자연$과목명) %>% length()

set.seed(1029)
memory.size()

memory.limit()
memory.limit(5000)

자연_rf_result <- randomForest(자연계열 ~ 사업여부 + 고교유형1 + 공사립 + 과목구분명, data = 자연, mtry = 4, ntree = 1000, importance = TRUE)

# ntree에 따른 에러값
plot(인문_rf_result$err.rate[, 1], col = "blue")

# 변수별 중요도
인문_rf_result %>% importance

varImpPlot(인문_rf_result, type = 2, pch = 19, col = 1, cex = 1, main = "")
