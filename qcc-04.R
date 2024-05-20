## 주성분 분석
library(tidyverse)
library(corrplot)
library(data.table)
library(factoextra)
library(FactoMineR)

# mpg data
data(airquality)
airquality %>% str()
air <- airquality %>% select(-c("Month", "Day"))
air <- air %>% na.omit()


# corr
air %>% cor() %>% corrplot()

# PCA
air %>% PCA(graph = FALSE) %>% summary()
air %>% PCA(graph = FALSE) %>% str()

# eigen, percentage of variance
(air %>% PCA(graph = FALSE))$eig

# screeplot - 축소된 차원의 정보를 얼마나 보존 하는가
air %>% PCA(graph = FALSE) %>% fviz_screeplot()

# biplot
air %>% PCA(graph = FALSE) %>% 
  fviz_pca_var(col.var="contrib",
               gradient.cols = c("#a18b6b", "#002554"), repel = TRUE)

air %>% PCA(graph = FALSE) %>% fviz_pca_biplot(repel = FALSE)


## T^2 관리도
# 예제 6-3 p.165
library(qcc)
x1 <- rnorm(120, mean = 50, sd = 10) %>% 
  matrix(nrow = 30)
x2 <- rnorm(120, mean = 20, sd = 10) %>% 
  matrix(nrow = 30)

x12 <- list(x1 = x1, x2 = x2)

mqcc(x12, type = "T2", limits = FALSE, pred.limits = TRUE, confidence = 0.99) %>% summary()

mqcc(x12, type = "T2", limits = FALSE, pred.limits = TRUE, confidence = 0.99) %>% str()

mqcc(x12, type = "T2") %>% ellipseChart(show.id = TRUE)


# 연습문제 7 p.170
table6_8 <- c(
  9.46, 9.50, 9.99, 10.04,
  11.75, 10.30, 9.69, 9.82,
  10.56, 11.30, 4.63, 12.39,
  9.17, 7.48, 8.87, 11.94,
  13.62, 10.98, 8.57, 10.60,
  6.25, 15.26, 10.51, 14.07,
  10.64, 11.82, 8.13, 12.13,
  8.63, 14.48, 10.98, 10.85,
  9.41, 7.57, 8.67, 9.71,
  8.51, 10.81, 8.47, 13.68,
  10.37, 12.05, 7.90, 13.86,
  9.68, 11.94, 13.43, 10.97,
  6.76, 7.18, 10.26, 12.35,
  8.39, 12.48, 11.64, 12.31,
  11.38, 12.13, 11.19, 13.17,
  10.97, 11.13, 12.18, 11.01,
  10.14, 8.99, 13.49, 12.84,
  8.5, 12.29, 10.39, 9.84,
  12.98, 12.04, 12.59, 8.52, 
  11.87, 11.96, 10.77, 9.27,
  12.06, 9.83, 12.69, 14.09,
  8.23, 10.67, 17.10, 10.65,
  6.28, 9.13, 7.91, 13.01,
  12.00, 10.65, 9.62, 12.06,
  12.76, 11.99, 10.58, 11.84,
  11.69, 10.66, 11.73, 6.02,
  9.17, 10.99, 8.91, 11.85,
  11.69, 7.92, 9.06, 10.56,
  9.27, 8.03, 12.81, 12.46,
  9.06, 13.50, 13.87, 7.7)

mat <- matrix(table6_8, ncol = , byrow = TRUE)

ewma(rowMeans(mat), sizes = , std.dev = , lambda = , nsigmas = , center = )


# 연습문제 8 p.173
# 부그룹(i=1)만 xbar, S^2, T^2 구하기