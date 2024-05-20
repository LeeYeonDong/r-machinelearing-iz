install.packages("qcc")
library(qcc)
library(tidyverse)

# 예제 4-2 p.109
set.seed(1029)

d <- 5 # 5일간 측정
t <- 4 # 4시간마다
s <- 24/t # 표본번호

pst <- replicate(d, rnorm(d*s, mean = 110, sd = 2)) %>% as.data.frame()

names(pst) <- paste0("day",1:ncol(pst)) # 일
pst <- pst %>% 
  gather(key = "일", value = "value")

pst$시각 <- seq(from = 4, to = 24, by = t) # 시각

pst$표본번호 <- c(1:s) # 표본번호

pst$부분군 <- c(1:(nrow(pst)/d)) # 부분군

# mutate 함수를 사용하면 데이터 프레임 크기를 맞춰줘야함
pst$일 <- str_replace_all(pst$일, pattern = "day", replacement = "") %>% 
  as.integer()

pst <- pst %>% 
  as_tibble() %>% 
  select(일, 시각, 표본번호, 부분군, value)

pst %>% view()

# xbar chart
xbar_chart <- pst %>% 
  select(value) %>% 
  as.matrix() %>% 
  matrix(nrow = d*s, byrow = FALSE) %>% 
  qcc(type = "xbar", plot = TRUE)

xbar_chart %>% summary()
xbar_chart$limits

process.capability(xbar_chart, spec.limits = c((xbar_chart$limits)[1,1],(xbar_chart$limits)[1,2]))


# 예제 5-1 p.126
# xbar-r chart
rbar_chart <- pst %>% 
  select(value) %>% 
  as.matrix() %>% 
  matrix(nrow = d*s, byrow = FALSE) %>% 
  qcc(type = "R", plot = TRUE)

rbar_chart %>% summary()
rbar_chart$limits


# 예제 5-2 p.137
set.seed(1029)

d <- 10 # 10일간 측정
t <- 8 # 8시간마다
s <- 24/t # 표본번호

pd <- replicate(d, rbinom(s, size = 100, prob = 0.051)) %>% as.data.frame()

names(pd) <- paste0("day",1:ncol(pd)) # 일
pd <- pd %>% 
  gather(key = "일", value = "value")

pd$시각 <- seq(from = 8, to = 24, by = t) # 시각

pd$표본번호 <- c(1:s) # 표본번호

pd$일 <- str_replace_all(pd$일, pattern = "day", replacement = "") %>% 
  as.integer()

pd <- pd %>% 
  as_tibble() %>% 
  select(일, 시각, 표본번호, value)

# np chart
np_chart <- pd %>% 
  select(value) %>% 
  as.matrix() %>% 
  matrix(nrow = d*s, byrow = FALSE) %>% 
  qcc(type = "np", sizes = 100, plot = TRUE)

np_chart %>% summary()
np_chart$limits


# 연습문제 9 p.143
set.seed(1029)

d <- 5 # 5일간 측정
t <- 2 # 2시간마다
s <- 24/t # 표본번호

che <- replicate(d, rnorm(s, mean = 20, sd = 5)) %>% as.data.frame()

names(che) <- paste0("day",1:ncol(che)) # 일
che <- che %>% 
  gather(key = "일", value = "value")

che$시각 <- seq(from = 2, to = 24, by = t) # 시각

che$표본번호 <- c(1:d*s) # 표본번호

# mutate 함수를 사용하면 데이터 프레임 크기를 맞춰줘야함

che$일 <- str_replace_all(che$일, pattern = "day", replacement = "") %>% 
  as.integer()

che <- che %>% 
  as_tibble() %>% 
  select(일, 시각, 표본번호, value)

view(che)

# X_MR 관리도
# x_chart
x_chart <- che %>% 
  select(value) %>% 
  as.matrix() %>% 
  matrix(nrow = d*s, byrow = FALSE) %>% 
  qcc(type = "xbar.one", plot = TRUE)

x_chart %>% summary()
x_chart$limits

# mr_chart
mr_chart_mat <- che %>% 
  select(value) %>% 
  as.matrix() %>% 
  matrix(nrow = d*s, byrow = FALSE)

ad_mat <- matrix(nrow = d*s-1, ncol = 2, byrow = FALSE)
ad_mat[,1] <- mr_chart_mat[1:(d*s-1),]
ad_mat[,2] <- mr_chart_mat[(1+1):(d*s),]

mr_chart <- ad_mat %>% 
  qcc(type = "R", plot = TRUE)

mr_chart %>% summary()
mr_chart$limits


# 연습문제 10 p.146
set.seed(1029)

d <- 4 # 4일간 측정
t <- 4 # 4시간마다
s <- 24/t # 표본번호

def <- replicate(d, rnorm(s, mean = 200, sd = 5)) %>% as.data.frame()

names(def) <- paste0("day",1:ncol(def)) # 일
def <- def %>% 
  gather(key = "일", value = "value")

def$시각 <- seq(from = 4, to = 24, by = t) # 시각

def$표본번호 <- c(1:d*s) # 표본번호

# mutate 함수를 사용하면 데이터 프레임 크기를 맞춰줘야함

def$일 <- str_replace_all(def$일, pattern = "day", replacement = "") %>% 
  as.integer()

def <- def %>% 
  as_tibble() %>% 
  select(일, 시각, 표본번호, value)

def$불량개수 <- rnorm(d*s, mean = 10, sd = 5) %>% round()

# p_chart
p_chart_mat <- def %>% 
  select(value, 불량개수) %>% 
  as.matrix() %>% 
  matrix(nrow = d*s, byrow = FALSE)

p_chart <- p_chart_mat[,2] %>% 
  qcc(type = "p", sizes = p_chart_mat[,1], plot = TRUE)
p_chart %>% summary()
p_chart$limits