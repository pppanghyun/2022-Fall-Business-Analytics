install.packages("MASS")
install.packages("e1071")
library(MASS)
library(e1071)
n1 <- n2 <- 50 # 데이터 50개
m1 <- c(1.5,1) # 다변수정규분포1 평균 
m2 <- c(-2,-2) # 다변수정규분포2 평균

v1 <- v2 <- diag(2) # 0,1 설정
x1 <- mvrnorm(n1, m1, v1) # 다변수정규분포1
x2 <- mvrnorm(n2, m2, v2) # 다변수정규분포2

# linearly separable한 데이터 생성
plot(x1[,1],x1[,2],col=2,xlim=c(-4,4),ylim=c(-4,4)) # x1 data plot
points(x2[,1],x2[,2],col=3) # x2 data plot

# dataframe check
dat <- data.frame(cbind(rep(c(0,1),each=n1),rbind(x1,x2))) # x1=0, x2=1
colnames(dat) <- c("y","z1","z2")

# svm with large C
model_largec <- svm(as.factor(y)~.,kernel="linear",cost=10e15, data=dat) # c가 무한(cost=10e15)으로 큰 경우 = hard margin
summary(model_largec)
predict(model_largec,dat[,-1])
plot(model_largec,dat)
model_largec$coefs # 3개의 support vector 존재 (이때의 w값)
# soft margin(에러 허용하는 svm)에서는 c값이 매우 커지면 hard margin(일반 SVM)과 같아진다. (soft svm with large C = svm), 에러를 허용 안함

# svm with Small C (추가실험)
model_smallc <- svm(as.factor(y)~.,kernel="linear",cost=10e-15, data=dat) # c가 없는경우(cost=10e-15)으로 큰 경우
summary(model_smallc)
predict(model_smallc,dat[,-1])
plot(model_smallc,dat)
model_smallc$coefs # 모든 점이 support vector가 됨 = w가 전부 0
# soft margin(에러 허용하는 svm) c값이 매우 작으면, 즉 error를 고려하지 않는 경우엔 모든 점이 support vector가 된다. (의미가 없다)