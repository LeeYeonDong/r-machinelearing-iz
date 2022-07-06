library(tidyverse)

# torch
# install.packages("torch")
# install.packages("luz")
library(torch)
library(luz)

torch_tensor(1)

# install.packages("torchvision")
library(torchvision)

# remotes::install_github("mlverse/torchdatasets")
library(torchdatasets)

## Chapter 1 딥러닝 첫걸음, 텐서 (tensor) 만들기
# 빈텐서
x <- torch_empty(5, 3)
x %>% dim()

# 랜덤 텐서
rand_tensor <- torch_rand(5, 3)
rand_tensor

# 단위 텐서
x <- torch_eye(4)

# 영 텐서
x <- torch_zeros(3, 5)
x

# 텐서 직접선언
y <- torch_tensor(matrix(c(1, 2, 3, 4, 5, 6), ncol = 2))
y

# 연산자 사용
y <- torch_tensor(matrix(1:6, ncol = 2))
y

# seq() 함수 사용
y <- torch_tensor(matrix(seq(0.1, 1, by = 0.1), ncol = 2))
y

y <- torch_tensor(matrix(seq(0, 1, length.out = 10), ncol = 2))
y

# %>% 연산자 사용
library(magrittr)
y2 <- torch_tensor(1:5 %>% diag())
y2

## Chapter 2 텐서 (tensor) 연산
torch_manual_seed(1029)

A <- torch_tensor(1:6)
B <- torch_rand(2, 3)
C <- torch_rand(2, 3, 2)

A
B
C

A$dtype
B$dtype
