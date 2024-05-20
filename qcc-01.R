# R 버전에 맞는 Rtools 설치
# 삭제된 패키지 아카이브 확인
# https://cran.r-project.org/web/packages/qualityTools/index.html
# https://cran.r-project.org/src/contrib/Archive/qualityTools/

# 오프라인 패키지 설치
install.packages("qualityTools_1.55.tar.gz", repos = NULL, type = "source")

# 깃헙 패키지 설치
devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

remotes::install_url("https://github.com/microsoft/LightGBM/releases/download/v3.0.0/lightgbm-3.0.0-r-cran.tar.gz")

install.packages("qcc")
library(qcc)
library(tidyverse)

# Generate random data
set.seed(1029)
data <- data.frame(replicate(3, rnorm(100, mean = 10, sd = 2)))
names(data) <- paste0("op",1:3)
data$part <- paste0("M",1:10)

data %>% head()

data <- data %>% 
  pivot_longer(cols = -part, names_to = "op", values_to = "Measurement")

data$part <- data$part %>% as.factor()
data$op <- data$op %>% as.factor()

# Measurement by part
ggplot(data, aes(x = part, y = Measurement, fill = part)) +
  geom_boxplot() +
  labs(title = "Measurement by part",
       x = "part",
       y = "Measurement") +
  theme_bw()

# Measurement by operator
ggplot(data, aes(x = op, y = Measurement, fill = op)) +
  geom_boxplot() +
  labs(title = "Measurement by operator",
       x = "operator",
       y = "Measurement") +
  theme_bw()

# xbar Chart by operator - ggplot
data$id <- rep(c(1:((data %>% nrow())/(unique(data$op) %>% length()))), times = 3)

data$id <- paste0(data$op,"_",data$id)

rep_col <- c(rep("#E69F00",times = (data %>% filter(op == unique(data$op)[1]) %>% nrow())),
             rep("#56B4E9",times = (data %>% filter(op == unique(data$op)[2]) %>% nrow())),
             rep("#009E73",times = (data %>% filter(op == unique(data$op)[3]) %>% nrow())))

data_m <- data %>% 
  select(-c(id)) %>% 
  arrange(part, op)
data_m$id <- paste0(data_m$op,"_",data_m$part)

data_m <- data_m %>% 
  group_by(op,part,id) %>% 
  summarise(xbar = mean(Measurement))

data_m %>% 
  ggplot(aes(x = id, y = xbar, fill = op)) +
  geom_bar(stat = "identity")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, size = 5)) +
  xlab("Description") +
  scale_y_continuous(limit = c(0,max(data_m$xbar))) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(colour="black", size = 10, face = "bold")) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = bar_col) +
  geom_hline(yintercept = 10.0152, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 10.56388, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 9.46652, color = "black", linetype = "dashed") +
  annotate("text", x = 0, y = 10, label = "Mean", color = "black", size = 4, hjust = 0) +
  annotate("text", x = 0, y = 11, label = "UCL", color = "black", size = 4, hjust = 0) +
  annotate("text", x = 0, y = 9, label = "LCL", color = "black", size = 4, hjust = 0)


# R Chart by operator - ggplot
data_r <- data %>% 
  select(-c(id)) %>% 
  arrange(part, op)
data_r$id <- paste0(data_r$op,"_",data_r$part)

data_r <- data_r %>% 
  group_by(op,part,id) %>% 
  summarise(range = max(Measurement) - min(Measurement))

data_r %>% 
  ggplot(aes(x = id, y = range, fill = op)) +
  geom_bar(stat = "identity")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, size = 5)) +
  xlab("Description") +
  scale_y_continuous(limit = c(0,max(data_r$range))) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(colour="black", size = 10, face = "bold")) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = bar_col)

# xbar Chart by operator and part - qcc
qcc.groups(data$Measurement, data$op) %>% 
  qcc(type = "xbar",plot = TRUE) %>% 
  summary()

qcc.groups(data$Measurement, data$part) %>% 
  qcc(type = "xbar", plot = TRUE)

# R Chart by operator and part - qcc
data_mat <- data %>% select(-c(id))
data_mat$id <- rep(1:(nrow(data_mat)/length(unique(data_mat$op))), each = length(unique(data_mat$op)))

data_mat <- data_mat %>% 
  pivot_wider(names_from = op, values_from = Measurement) %>%   select(-c(part, id)) %>% as.matrix()

data_mat %>% 
  qcc(type = "R", plot = FALSE) %>% 
  summary()
