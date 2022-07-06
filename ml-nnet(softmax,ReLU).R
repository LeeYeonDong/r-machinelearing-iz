# ?????????
word_vectors_NC_df <- word_vectors_NC %>% as.data.frame()
word_vectors_NC_df$label <- rbinom(n = length(word_vectors_NC_df$V1), size = 5, prob = 0.5)
word_vectors_NC_df$label <- word_vectors_NC_df$label %>% as.factor()
word_vectors_NC_df$label2 <- word_vectors_NC_df$label %>% class.ind()

rownames(word_vectors_NC_df) <- NULL

# ????????? ??????
set.seed(1029)
inTrain <- createDataPartition(y = word_vectors_NC_df$label, p=0.6, list=FALSE)
df_train <- word_vectors_NC_df[inTrain,]
df_test <- word_vectors_NC_df[-inTrain,]

# neuralnet - backpropagation
devtools::install_github("bips-hb/neuralnet") # relu?????? ??????
library(neuralnet)
## ????????? ??????
df_train %>% head()

neural_model <- neuralnet(label ~ V1 + V2 + V3, 
                        data = df_train, 
                        hidden = 5, act.fct = "relu",
                        err.fct="sse", linear.output = FALSE, likelihood = TRUE)
neural_model %>% plot()
neural_model %>% summary()

neural_model$result.matrix

temp_test <- df_test[,c(1:3,33)]
neural_pred <- predict(neural_model, df_test[,c(1:3,33)])
table(df_test$label, apply(neural_pred, 1, which.max)) 


# compute ??????
neural_pred <- neuralnet::compute(neural_model, df_test[,c(1:3,33)])
neural_pred %>% summary()

roundedresults <- sapply(results,round,digits=0)
roundedresultsdf <- data.frame(roundedresults)

table(roundedresultsdf$actual, roundedresultsdf$prediction)


# nnet
install.packages("nnet")
install.packages("caret")
install.packages("ROCR")
library(nnet)
library(caret)
library(ROCR)
library(reshape2)

df_train$label2
nn_model <- nnet(label2 ~ V1 + V2 + V3, data = df_train, 
                 size = 3, maxit = 1000, entropy=TRUE, 
                 softmax = TRUE, decay = 0.0005)

nn_model %>% summary()

nn_pred <- predict(nn_model, df_test[,c(1:3)], type = "class")
table(nn_pred, df_test$label)

# ????????? R ?????? ?????? ????????????
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

nn_model %>% plot.nnet()

install.packages("NeuralNetTools")
library(NeuralNetTools)
nn_model %>% garson() # one output model(Y:??????????????? 1?????????) ???, ????????? ???????????? ?????? ??????
nn_model %>% lekprofile() # one output model(Y:??????????????? 1?????????) ???, ????????? ???????????? ?????? ??????
