install.packages("xgboost")
library(xgboost)
library(tidyverse)
raw <- read_csv(file = "D:/대학원/논문/교육과정/더미_정리.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))
raw <- raw %>% tibble()

raw$계열 <- raw$계열 %>% as.factor()

y_num <- as.integer(raw$계열)-1
y_num %>% length()

x_raw <- raw %>% 
  select(-계열, -고교명, -학교코드) %>% 
  data.matrix()
x_raw %>% dim()

bst1 <- xgboost(data = x_raw, label = y_num,
                 max_depth = 6, nthread = 2, 
                 nrounds = 1000)



rt <- xgb.importance(colnames(x_raw), model = bst1)
xgb.plot.importance(rt) %>% str()

print(rt)

rt <- rt %>% as_tibble()
rt <- rt %>% arrange(-Importance)
rt30 <- rt[1:30,]
rt30$Importance <- rt30$Importance %>% round(4)


ggplot(rt30, aes(x=Importance, y=reorder(Feature,Importance), fill = Importance)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = Importance),hjust = -0.1,size=5) +
scale_x_continuous(expand = c(0,0), limit=c(0,0.5)) 


# one-hot encoding
raw$계열 <- raw$계열 %>% as.factor()

library(mltools)
library(data.table)
new_raw <- one_hot(as.data.table(raw))