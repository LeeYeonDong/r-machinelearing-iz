#########clustering

studata <- read.csv("C:/Users/Administrator/Desktop/18년도 종단분석 연구/raw data/2018 종단분석 기본정보 + 추가 변수 0129(재학생).csv",head=T)
studata <- na.omit(studata)

studata.nh <- studata[studata$고교유형==c("3. 일반고","4. 자공고","5. 자사고"),]
studata.sh <- studata[studata$고교유형=="1. 과학고",]


library(ggplot2)



######일반고

studata.nh.kmeans <- kmeans(studata.nh[,c("전체내신","GPA_평균")], 3)
studata.nh.kmeans

names(studata.nh.kmeans)

table(studata.nh.kmeans$cluster)
prop.table(studata.nh.kmeans$size)


studata.nh.kmeans.cluster <- studata.nh.kmeans$cluster
studata.nh.kmeans.cluster.x.y <- cbind(studata.nh.kmeans.cluster, studata.nh[,c("전체내신","GPA_평균")])


str(studata.nh.kmeans.cluster.x.y)


studata.nh.kmeans.x.y <- transform(studata.nh.kmeans.cluster.x.y, studata.nh.kmeans.cluster=as.factor(studata.nh.kmeans.cluster))
str(studata.nh.kmeans.x.y)


ggplot(data=studata.nh.kmeans.x.y, aes(x=전체내신, y=GPA_평균, colour=studata.nh.kmeans.cluster)) + 
  geom_point(shape=19, size=4) + 
  labs(colour="재학생 그룹") +
  scale_x_continuous(trans="reverse") +
  ggtitle("일반고 출신 재학생 그룹(전체내신,GPA평균)")

studata.nh.kmeans$centers



######과학고

studata.sh.kmeans <- kmeans(studata.sh[,c("전체내신","GPA_평균")], 4)
studata.sh.kmeans

names(studata.sh.kmeans)

table(studata.sh.kmeans$cluster)
prop.table(studata.sh.kmeans$size)


studata.sh.kmeans.cluster <- studata.sh.kmeans$cluster
studata.sh.kmeans.cluster.x.y <- cbind(studata.sh.kmeans.cluster, studata.sh[,c("전체내신","GPA_평균")])


str(studata.sh.kmeans.cluster.x.y)


studata.sh.kmeans.x.y <- transform(studata.sh.kmeans.cluster.x.y, studata.sh.kmeans.cluster=as.factor(studata.sh.kmeans.cluster))
str(studata.sh.kmeans.x.y)


ggplot(data=studata.sh.kmeans.x.y, aes(x=전체내신, y=GPA_평균, colour=studata.sh.kmeans.cluster)) + 
  geom_point(shape=19, size=4) + 
  scale_x_continuous(trans="reverse") +
  labs(colour="재학생 그룹") +
  ggtitle("과학고 출신 재학생 그룹(전체내신,GPA평균)")

studata.sh.kmeans$centers
