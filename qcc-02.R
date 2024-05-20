# Generate example dataset
set.seed(123) # for reproducibility
treatment <- rep(c("A", "B"), each = 40)
gender <- rep(c("male", "female"), times = 40)
blood_pressure <- rnorm(80, mean = 120, sd = 10)
df <- data.frame(treatment, gender, blood_pressure)

# Perform two-way ANOVA
model <- aov(blood_pressure ~ treatment + gender, data = df) # 교호작용 "X"
summary(model) # view ANOVA table

model <- aov(blood_pressure ~ treatment + gender + treatment*gender, data = df) # 교호작용 "o"
summary(model) # view ANOVA table

library(tidyverse)
df <- read_csv(file = "D:/대학원/강의/2023-1 품질경영/anova.csv", col_names = TRUE, locale = locale('ko',encoding='utf-8'))

df$시료번호 <- df$시료번호 %>% as.factor()

aov(value ~ 측정자 + 시료번호 + 측정자*시료번호, data = df) %>% summary()# 교호작용 "o"
