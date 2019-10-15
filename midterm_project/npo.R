library(rpart)
library(pls)

set.seed(123);
setwd("C:\\Users\\hyunho\\Shiny\\study\\3-1\\univ_class\\MachineLearning\\midterm_project")
npo.data=read.csv("NPO2019.csv")
#install.packages("dplyr") # dplyr 설치
library(dplyr) # dplyr 로드

# 모든항목 10점 준 대상 제거
npo.data[npo.data$ID==6,]
npo.data[npo.data$ID==29,]
npo.data[npo.data$ID==74,]
npo.data[npo.data$ID==80,]
npo.data[npo.data$ID==113,]
npo.data[npo.data$ID==126,]
npo.data[npo.data$ID==146,]
npo.data = npo.data %>% filter(!npo.data$ID==6)
npo.data = npo.data %>% filter(!npo.data$ID==29)
npo.data = npo.data %>% filter(!npo.data$ID==74)
npo.data = npo.data %>% filter(!npo.data$ID==80)
npo.data = npo.data %>% filter(!npo.data$ID==113)
npo.data = npo.data %>% filter(!npo.data$ID==126)
npo.data = npo.data %>% filter(!npo.data$ID==146)
# 모든항목에 0을 많이 준 대상 제거
npo.data[npo.data$ID==102,]
npo.data = npo.data %>% filter(!npo.data$ID==102)
# 장난처럼 평가한 대상 제거
npo.data[npo.data$ID==16,]
npo.data = npo.data %>% filter(!npo.data$ID==16)
# 결측치확인 
# 결측치 처리하기 -> 결측 데이터 제거, 평균값 사용
for(i in 1:length(colnames(npo.data))){
  print(colnames(npo.data)[i])
  print(sum(is.na(npo.data[,i])))
  npo.data = npo.data %>% filter(!is.na(i))
}

npo.data <- npo.data %>% filter(!is.na(firstknow) & !is.na(knowyear))
npo.data <- npo.data %>% filter(!is.na(experience) & !is.na(times))

for(i in 1:length(colnames(npo.data))){
  print(colnames(npo.data)[i])
  print(sum(is.na(npo.data[,i])))
  npo.data = npo.data %>% filter(!is.na(i))
}

# 데이터 분포 확인 
library(ggplot2) 
# 1. k means 클러스터링 
# 중요도 변수만 클러스터링
col_list = colnames(npo.data)
col_list = col_list[3:20]
npo.clustering = npo.data[col_list]
npo.clustering

#npo.clustering<-scale(npo.clustering)
npo.clustering
## k정하기
library(NbClust)

#par("mar")
#par(mar=c(3,3,3,3))
#par(mar=c(0.0001,0.0001,0.0001,0.0001))

nc <- NbClust(npo.clustering, method="kmeans")
table(nc$Best.n[1,])
par(mfrow=c(1,1)) # 하나로 합침
barplot(table(nc$Best.n[1,]), xlab="# of Clusters", ylab="# of Criteria")

fit.npo <- kmeans(npo.clustering, 2, nstart = 25)
fit.npo$size
#boxplot(fit.npo$centers)
library("factoextra")
fviz_cluster(fit.npo,npo.clustering)
fviz_cluster(fit.npo,npo.clustering, geom = "point", pointsize = 1)


# 너무 커서 안그려져요
#plot(npo.clustering, col=fit.npo)

library(fpc)
library(cluster)

pamk.test <- pamk(npo.clustering)
pamk.test$nc
#table(pamk.test$pamobject$clustering, npo.clustering$outsider)

#par(mar=c(5,5,5,5))
par(mfrow=c(1,2))
plot(pamk.test$pamobject)

pam.fit <- pam(npo.clustering, 2)

par(mfrow=c(1,2))
plot(pamk.test$pamobject)
par(mfrow=c(1,1))

fviz_cluster(pam.fit)
fviz_cluster(pamk.test$pamobject)
fviz_cluster(pam.fit,npo.clustering, geom = "point", pointsize = 1)

clara.cluster <- clara(npo.clustering, 2, samples = 50, pamLike = TRUE)

print(clara.cluster)
clara.cluster$medoids
fviz_cluster(clara.cluster, stand = TRUE, geom = "point", pointsize = 1)

# withniss값을 그래프로 그리기
#wss <- 0 
#for(i in 1:15)  wss[i] <- sum(kmeans(npo.clustering,centers=i)$withinss)
#plot(1:15, wss, type="b",xlab = "Number of Clusters", ylab ="Within group sum of squares")

npo.clustering$a1 <- as.numeric(npo.clustering$a1)
npo.clustering$b1 <- as.numeric(npo.clustering$b1)
plot(jitter(npo.clustering$a1),jitter(npo.clustering$b1),pch=fit.npo$cluster,col=fit.npo$cluster)

npo.clustering$a1 <- as.numeric(npo.clustering$a2)
npo.clustering$b1 <- as.numeric(npo.clustering$b2)
plot(jitter(npo.clustering$a2),jitter(npo.clustering$b2),pch=fit.npo$cluster,col=fit.npo$cluster)

## 
## kmeans 가우시안(mclust) 계층적구조
## 해야할 것 다른 알고리즘을 사용
## 클러스터링 결과를 시각화 -> 눈에 띄게 군집되어 있는 그래프 찾기
## 군집 네이밍 

### 2번 문제

npo.data<-cbind(npo.data,fit.npo$cluster)
names(npo.data)[30] <- c("cluster")

npo_1 = subset(npo.data, cluster==1 )
npo_2 = subset(npo.data, cluster==2 )

#npo_3 = subset(npo.data, kmc$cluster==3 )


mean(npo_1$recommend)
mean(npo_2$recommend)
#mean(npo_3$recommend)

mean_outsider<-c(mean(npo_1$outsider),mean(npo_2$outsider))
mean_recommend<-c(mean(npo_1$recommend),mean(npo_2$recommend))
mean_future_recommend<-c(mean(npo_1$future_recommend),mean(npo_2$future_recommend))
mean_lookfor<-c(mean(npo_1$lookfor),mean(npo_2$lookfor))
mean_future_lookfor<-c(mean(npo_1$future_lookfor),mean(npo_2$future_lookfor))
mean_trust<-c(mean(npo_1$trust),mean(npo_2$trust))
mean_firstknow<-c(mean(npo_1$firstknow),mean(npo_2$firstknow))
mean_knowyear<-c(mean(npo_1$knowyear),mean(npo_2$knowyear))
mean_experience<-c(mean(npo_1$experience),mean(npo_2$experience))
mean_times<-c(mean(npo_1$times),mean(npo_2$times))

df<-data.frame(mean_outsider)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,0.8))
df<-data.frame(mean_knowyear)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,3.5))
df<-data.frame(mean_times)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,1.8))
df<-data.frame(mean_recommend)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,1))
df<-data.frame(mean_future_recommend,mean_lookfor,mean_trust,mean_future_lookfor,mean_experience)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,10))
legend(13,10,c("npo_1","npo_2"),cex=0.7,fill=rainbow(nrow(df)))

#boxplot(npo.clustering)

# 1번 1,2,3 그룹의 단기적 장기적 차이점 찾아보기


## 2. 외부관계자 내부관계자 비교
npo_out<-subset(npo.data,outsider==1)
npo_out
npo_in<-subset(npo.data,outsider==0)
npo_in
npo_in[,26]

mean_outsider<-c(mean(npo_out$outsider),mean(npo_in$outsider))
mean_recommend<-c(mean(npo_out$recommend),mean(npo_in$recommend))
mean_future_recommend<-c(mean(npo_out$future_recommend),mean(npo_in$future_recommend))
mean_lookfor<-c(mean(npo_out$lookfor),mean(npo_in$lookfor))
mean_future_lookfor<-c(mean(npo_out$future_lookfor),mean(npo_in$future_lookfor))
mean_trust<-c(mean(npo_out$trust),mean(npo_in$trust))
#mean_firstknow<-c(mean(npo_out$firstknow),mean(npo_in$firstknow))
mean_knowyear<-c(mean(npo_out$knowyear),mean(npo_in$knowyear))
mean_experience<-c(mean(npo_out$experience),mean(npo_in$experience))
mean_times<-c(mean(npo_out$times),mean(npo_in$times))

df<-data.frame(mean_outsider)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,0.8))
df<-data.frame(mean_knowyear)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,4))
df<-data.frame(mean_times)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,2))
df<-data.frame(mean_recommend)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,1))
df<-data.frame(mean_future_recommend,mean_lookfor,mean_trust,mean_future_lookfor,mean_experience)
df
barplot(as.matrix(df),beside=T,col=rainbow(nrow(df)),ylim=c(0,10))

npo_in = npo_in[,-c(26)]
npo_in = npo_in[,-c(1)]
npo_out = npo_out[,-c(26)]
npo_out = npo_out[,-c(1)]

boxplot(npo_in)
boxplot(npo_out)

legend(12,9,c("out","in"),cex=0.7,fill=rainbow(nrow(df)))
var.test(npo_in$a5,npo_out$a5)
var.test(npo_in$a2,npo_out$a2)

## 

set.seed(123)
## random forest
library(randomForest)
npo.data
npo.rf <- npo.data[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
#npo.rf=npo.data
i = sample(1:nrow(npo.rf), round(nrow(npo.rf)*0.7))
npo.train = npo.rf[i,]
npo.test = npo.rf[-i,]
npo.train$cluster <- as.factor(npo.train$cluster)
data(npo.train)
MB <- randomForest(npo.train$cluster ~.,data=npo.train, importance = T, proximity = T)
print(MB)
names(MB)
importance(MB, type=2)
varImpPlot(MB)
round(importance(MB), 2)

pred.MB <- predict(MB, newdata=npo.test)
tab=table(npo.test$cluster, pred.MB, dnn=c("Actual", "Predicted"))
print(tab)

error_rate = 1-sum(diag(tab)/sum(tab))
error_rate

plot(MB)


## 결측치 포함한 클러스트링
setwd("C:\\Users\\hyunho\\Shiny\\study\\3-1\\univ_class\\MachineLearning\\midterm_project")
npo.data=read.csv("NPO2019.csv")


# 데이터 분포 확인 
library(ggplot2) 
# 1. k means 클러스터링 
# 중요도 변수만 클러스터링
col_list = colnames(npo.data)
col_list = col_list[3:20]
npo.clustering = npo.data[col_list]
npo.clustering

#npo.clustering<-scale(npo.clustering)
npo.clustering
## k정하기
library(NbClust)

#par("mar")
#par(mar=c(3,3,3,3))
#par(mar=c(0.0001,0.0001,0.0001,0.0001))

nc <- NbClust(npo.clustering, method="kmeans")
table(nc$Best.n[1,])
par(mfrow=c(1,1)) # 하나로 합침
barplot(table(nc$Best.n[1,]), xlab="# of Clusters", ylab="# of Criteria")

fit.npo <- kmeans(npo.clustering, 2, nstart = 25)
fit.npo$size
#boxplot(fit.npo$centers)
library("factoextra")
fviz_cluster(fit.npo,npo.clustering)
fviz_cluster(fit.npo,npo.clustering, geom = "point", pointsize = 1)

npo.clustering
mean(npo.clustering$a1)
mean(npo.clustering$b1)

df1<-data.frame(a=c(mean(npo.clustering$a1),mean(npo.clustering$a2),mean(npo.clustering$a3),mean(npo.clustering$a4),mean(npo.clustering$a5),mean(npo.clustering$a6),mean(npo.clustering$a7),mean(npo.clustering$a8),mean(npo.clustering$a9)),b=c(mean(npo.clustering$b1),mean(npo.clustering$b2),mean(npo.clustering$b3),mean(npo.clustering$b4),mean(npo.clustering$b5),mean(npo.clustering$b6),mean(npo.clustering$b7),mean(npo.clustering$b8),mean(npo.clustering$b9)))
plot(df1$a, df1$b,xlim=c(6.5,9),ylim=c(6.5,9))
abline(a=0,b=1,col="black",lty=6)

npo_1 = subset(npo.data, cluster==1 )
npo_2 = subset(npo.data, cluster==2 )

#npo_3 = subset(npo.data, kmc$cluster==3 )


mean(npo_1$recommend)
mean(npo_2$recommend)
#mean(npo_3$recommend)


#############
mean_data <- matrix(c(
  mean(npo.clustering$a1), mean(npo.clustering$b1),
  mean(npo.clustering$a2), mean(npo.clustering$b2),
  mean(npo.clustering$a3), mean(npo.clustering$b3),
  mean(npo.clustering$a4), mean(npo.clustering$b4),
  mean(npo.clustering$a5), mean(npo.clustering$b5),
  mean(npo.clustering$a6), mean(npo.clustering$b6),
  mean(npo.clustering$a7), mean(npo.clustering$b7),
  mean(npo.clustering$a8), mean(npo.clustering$b8),
  mean(npo.clustering$a9), mean(npo.clustering$b9)),
  byrow=T, nrow=9, ncol=2
)
plot(mean_data,xlab="단기적 중요도",ylab="장기적 중요도", xlim=c(4,10), ylim=c(4,10), col=2, pch=1:9)

centers_1 <- matrix(c(
  mean(npo_1$a1), mean(npo_1$b1),
  mean(npo_1$a2), mean(npo_1$b2),
  mean(npo_1$a3), mean(npo_1$b3),
  mean(npo_1$a4), mean(npo_1$b4),
  mean(npo_1$a5), mean(npo_1$b5),
  mean(npo_1$a6), mean(npo_1$b6),
  mean(npo_1$a7), mean(npo_1$b7),
  mean(npo_1$a8), mean(npo_1$b8),
  mean(npo_1$a9), mean(npo_1$b9)),
  byrow=T, nrow=9, ncol=2)
centers_2 <- matrix(c(
    mean(npo_2$a1), mean(npo_2$b1),
    mean(npo_2$a2), mean(npo_2$b2),
    mean(npo_2$a3), mean(npo_2$b3),
    mean(npo_2$a4), mean(npo_2$b4),
    mean(npo_2$a5), mean(npo_2$b5),
    mean(npo_2$a6), mean(npo_2$b6),
    mean(npo_2$a7), mean(npo_2$b7),
    mean(npo_2$a8), mean(npo_2$b8),
    mean(npo_2$a9), mean(npo_2$b9)),
  byrow=T, nrow=9, ncol=2)
points(centers_1, col=3, pch=1:9, cex=1)
points(centers_2, col=4, pch=1:9, cex=1)

legend("bottomright", 
       c("a1,b1", "a2,b2", "a3,b3", "a4,b4", "a5,b5", "a6,b6","a7,b7", "a8,b8", "a9,b9"), 
       pch = 1:9
)
legend("topleft",c("Every","npo_1", "npo_2"), col = c(2:4),pch = 1)
abline(a=0,b=1,col="black",lty=6)

