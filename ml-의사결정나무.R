rawdata <- read.csv("C:/Users/Administrator/Downloads/data/Data mining/uga_raw_trans_dm5.csv",head=T)
rawdata <- na.omit(rawdata)

nh <- rawdata[rawdata$고교유형 %in% c(2,3,4,5,6,7,8,9),]
sh <- rawdata[rawdata$고교유형 == "1",]




install.packages("rpart")
library(rpart)



nh.tree1 <- rpart(최종결과~수학내신+과학내신, data=nh)
plot(nh.tree1, compress=T, uniform=T, margin=0.1)
text(nh.tree1, use.n=T, col="black")


sh.tree1 <- rpart(최종결과~수학내신+과학내신,data=sh)
plot(sh.tree1, compress=T, uniform=T, margin=0.1)
text(sh.tree1, use.n=T, col="black")


install.packages("rattle")
install.packages("RGtk2")
install.packages("rpart.plot")
install.packages("RColorBrewer")

library(rattle)				# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)  # Color selection for fancy tree 



fancyRpartPlot(nh.tree1,main="비과학고 최종합격자 내신등급 결정 트리",palettes=c("Blues","Greys"))
fancyRpartPlot(sh.tree1,main="과학고 최종합격자 내신등급 결정 트리",palettes=c("Blues","Greys"))
