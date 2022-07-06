install.packages("devtools")
library(devtools)

devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow()

library(tensorflow)
tf$constant("Hellow Tensorflow")
install_tensorflow()

install_tensorflow(version = "2.0.0")
install_tensorflow(version = "nightly-gpu")  # gpu version

#test
install.packages("keras")
library(keras)
mnist <- dataset_mnist()
mnist$train$x <- mnist$train$x/255
mnist$test$x <- mnist$test$x/255

model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(28, 28)) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(10, activation = "softmax")

summary(model)

model %>% 
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

model %>% 
  fit(
    x = mnist$train$x, y = mnist$train$y,
    epochs = 5,
    validation_split = 0.3,
    verbose = 2
  )

predictions <- predict(model, mnist$test$x)
head(predictions, 2)


