ten<-read.csv("C:/Users/Administrator/Downloads/data/내신하위-GPA추이/14-17 고교내신 GPA 군집 time.csv",head=T)
install.packages("ggplot2")
library(ggplot2)

ten$내신그룹[ten$내신그룹==""] <- NA
ten$GPA그룹[ten$GPA그룹==""] <- NA

ten  <- ten[complete.cases(ten[,c("내신그룹")]),]
ten  <- ten[complete.cases(ten[,c("GPA그룹")]),]

levels(ten$차수)
ten$차수 <- factor(ten$차수)
str(ten)


#####고교유형에 따른 수과내신, GPA평균 분포
ten_plot <- ggplot(data=ten, aes(x=수과내신, y=GPA_평균, colour=고교유형)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +
  stat_smooth(method=lm, level=0.95) +
  ggtitle("고교유형에 따른 수과내신, GPA평균 분포")
ten_plot


####일반고 과학고 분류 - 내신그룹 학기에 따른 수과내신, GPA평균 분포
ten_nh <- ten[ten$고교유형=="일반고",]
ten_sh <- ten[ten$고교유형=="과학고",]

ten_nh_plot <- ggplot(data=ten_nh, aes(x=수과내신, y=GPA_평균, colour=내신그룹)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +
  stat_smooth(method=lm, level=0.95) +
  ggtitle("내신그룹에 따른 수과내신, GPA평균 분포-일반고")
ten_nh_plot

ten_sh_plot <- ggplot(data=ten_sh, aes(x=수과내신, y=GPA_평균, colour=내신그룹)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +
  stat_smooth(method=lm, level=0.95) +
  ggtitle("내신그룹에 따른 수과내신, GPA평균 분포-과학고")
ten_sh_plot



ten_nh_plot2 <- ggplot(data=ten_nh, aes(x=수과내신, y=GPA_평균, colour=GPA그룹)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +
  stat_smooth(method=lm, level=0.95) +
  ggtitle("GPA그룹에 따른 수과내신, GPA평균 분포-일반고")
ten_nh_plot2

ten_sh_plot2 <- ggplot(data=ten_sh, aes(x=수과내신, y=GPA_평균, colour=GPA그룹)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +
  stat_smooth(method=lm, level=0.95) +
  ggtitle("GPA그룹에 따른 수과내신, GPA평균 분포-과학고")
ten_sh_plot2



ten_nh_n_plot <- ggplot(data=ten_nh, aes(x=수과내신, y=GPA_평균, colour=차수)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +
  ggtitle("학기에 따른 수과내신, GPA평균 분포-일반고")
ten_nh_n_plot

ten_sh_n_plot <- ggplot(data=ten_sh, aes(x=수과내신, y=GPA_평균, colour=차수)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +  
  ggtitle("학기에 따른 수과내신, GPA평균 분포-과학고")
ten_sh_n_plot


###상관계수
cor.test(ten_nh$수과내신,ten_nh$GPA_평균) ##일반고
cor.test(ten_sh$수과내신,ten_sh$GPA_평균) ##과학고  


#####학기별 중심값
ten_1 <- ten[ten$차수=="1차",]
ten_1_kmeans <- kmeans(ten_1[,c("수과내신","GPA_평균")],1)
ten_2 <- ten[ten$차수=="2차",]
ten_2_kmeans <- kmeans(ten_2[,c("수과내신","GPA_평균")],1)
ten_3 <- ten[ten$차수=="3차",]
ten_3_kmeans <- kmeans(ten_3[,c("수과내신","GPA_평균")],1)
ten_4 <- ten[ten$차수=="4차",]
ten_4_kmeans <- kmeans(ten_4[,c("수과내신","GPA_평균")],1)
ten_5 <- ten[ten$차수=="5차",]
ten_5_kmeans <- kmeans(ten_5[,c("수과내신","GPA_평균")],1)
ten_6 <- ten[ten$차수=="6차",]
ten_6_kmeans <- kmeans(ten_6[,c("수과내신","GPA_평균")],1)
ten_7 <- ten[ten$차수=="7차",]
ten_7_kmeans <- kmeans(ten_7[,c("수과내신","GPA_평균")],1)

ten_1_kmeans$centers
ten_2_kmeans$centers
ten_3_kmeans$centers
ten_4_kmeans$centers
ten_5_kmeans$centers
ten_6_kmeans$centers
ten_7_kmeans$centers

spec_data0<-read.csv("C:/Users/Administrator/Downloads/data/내신하위-GPA추이/14-17 고교내신 GPA 군집 time 전체.csv",head=T)


ten_n_plot <- ggplot(data=ten, aes(x=수과내신, y=GPA_평균, colour=차수)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +  
  ggtitle("학기에 따른 수과내신, GPA평균 분포-전체") +
  geom_point(data=spec_data0, aes(x=수과내신, y=GPA_평균),colour="black", shape=19, size=5) +
  geom_text(data=spec_data0, aes(label=차수), colour="black", position=position_dodge(width=0.9), vjust=-1)
ten_n_plot


###학기별 중심값 - 일반고
ten_nh_1 <- ten_nh[ten_nh$차수=="1차",]
ten_nh_1_kmeans <- kmeans(ten_nh_1[,c("수과내신","GPA_평균")],1)
ten_nh_2 <- ten_nh[ten_nh$차수=="2차",]
ten_nh_2_kmeans <- kmeans(ten_nh_2[,c("수과내신","GPA_평균")],1)
ten_nh_3 <- ten_nh[ten_nh$차수=="3차",]
ten_nh_3_kmeans <- kmeans(ten_nh_3[,c("수과내신","GPA_평균")],1)
ten_nh_4 <- ten_nh[ten_nh$차수=="4차",]
ten_nh_4_kmeans <- kmeans(ten_nh_4[,c("수과내신","GPA_평균")],1)
ten_nh_5 <- ten_nh[ten_nh$차수=="5차",]
ten_nh_5_kmeans <- kmeans(ten_nh_5[,c("수과내신","GPA_평균")],1)
ten_nh_6 <- ten_nh[ten_nh$차수=="6차",]
ten_nh_6_kmeans <- kmeans(ten_nh_6[,c("수과내신","GPA_평균")],1)
ten_nh_7 <- ten_nh[ten_nh$차수=="7차",]
ten_nh_7_kmeans <- kmeans(ten_nh_7[,c("수과내신","GPA_평균")],1)

ten_nh_1_kmeans$centers
ten_nh_2_kmeans$centers
ten_nh_3_kmeans$centers
ten_nh_4_kmeans$centers
ten_nh_5_kmeans$centers
ten_nh_6_kmeans$centers
ten_nh_7_kmeans$centers


spec_data1<-read.csv("C:/Users/Administrator/Downloads/data/내신하위-GPA추이/14-17 고교내신 GPA 군집 time 일반고.csv",head=T)


ten_nh_n_plot <- ggplot(data=ten_nh, aes(x=수과내신, y=GPA_평균, colour=차수)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +  
  ggtitle("학기에 따른 수과내신, GPA평균 분포-일반고") +
  geom_point(data=spec_data1, aes(x=수과내신, y=GPA_평균),colour="black", shape=19, size=5) +
  geom_text(data=spec_data1, aes(label=차수), colour="black", position=position_dodge(width=0.9), vjust=-1)
ten_nh_n_plot


###학기별 중심값 - 과학고
ten_sh_1 <- ten_sh[ten_sh$차수=="1차",]
ten_sh_1_kmeans <- kmeans(ten_sh_1[,c("수과내신","GPA_평균")],1)
ten_sh_2 <- ten_sh[ten_sh$차수=="2차",]
ten_sh_2_kmeans <- kmeans(ten_sh_2[,c("수과내신","GPA_평균")],1)
ten_sh_3 <- ten_sh[ten_sh$차수=="3차",]
ten_sh_3_kmeans <- kmeans(ten_sh_3[,c("수과내신","GPA_평균")],1)
ten_sh_4 <- ten_sh[ten_sh$차수=="4차",]
ten_sh_4_kmeans <- kmeans(ten_sh_4[,c("수과내신","GPA_평균")],1)
ten_sh_5 <- ten_sh[ten_sh$차수=="5차",]
ten_sh_5_kmeans <- kmeans(ten_sh_5[,c("수과내신","GPA_평균")],1)
ten_sh_6 <- ten_sh[ten_sh$차수=="6차",]
ten_sh_6_kmeans <- kmeans(ten_sh_6[,c("수과내신","GPA_평균")],1)
ten_sh_7 <- ten_sh[ten_sh$차수=="7차",]
ten_sh_7_kmeans <- kmeans(ten_sh_7[,c("수과내신","GPA_평균")],1)

ten_sh_1_kmeans$centers
ten_sh_2_kmeans$centers
ten_sh_3_kmeans$centers
ten_sh_4_kmeans$centers
ten_sh_5_kmeans$centers
ten_sh_6_kmeans$centers
ten_sh_7_kmeans$centers


spec_data2<-read.csv("C:/Users/Administrator/Downloads/data/내신하위-GPA추이/14-17 고교내신 GPA 군집 time 과학고.csv",head=T)

ten_sh_n_plot <- ggplot(data=ten_sh, aes(x=수과내신, y=GPA_평균, colour=차수)) +
  geom_point(shape=19, size=4) +
  scale_x_continuous(trans="reverse") +  
  ggtitle("학기에 따른 수과내신, GPA평균 분포-과학고") +
  geom_point(data=spec_data2, aes(x=수과내신, y=GPA_평균),colour="black", shape=19, size=5) +
  geom_text(data=spec_data2, aes(label=차수), colour="black", position=position_dodge(width=0.9), vjust=-1)
ten_sh_n_plot



####차수, 고교유형별 GPA
ten_time<-aggregate(ten$GPA_평균,by=list(ten$고교유형,ten$차수),mean)
names(ten_time)<-c("고교유형","차수","GPA_평균")
ten_time <- ten_time[ten_time$고교유형==c("영재학교","과학고","일반고"),]
ten_time

ten_time<-read.csv("C:/Users/Administrator/Downloads/data/내신하위-GPA추이/14-17 고교내신 GPA 군집.csv",head=T)

ten_time_plot <- ggplot(data=ten_time, aes(x=차수, y=GPA_평균, colour=고교유형, group=고교유형)) +
  geom_line() +
  geom_point(size=3) +
  geom_text(aes(label=GPA_평균, size=2,vjust=-1,hjust=0)) +
  ggtitle("14-17년도 고교 유형별 수과내신,GPA 현황")
ten_time_plot

