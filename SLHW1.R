#importing libraries
library(MASS)
library(class)

#importing and formatting data
spam <- read.table("C:\\Users\\wrigh\\OneDrive\\Desktop\\spambase.data",skip=0,sep=',')
spam <- as.data.frame(spam)

#building test and training sets
n<-dim(spam)[1];
test <- spam[seq(10, n, 10), ]
train <- spam[-c(seq(10, n, 10)), ]

#train, predict, performance assessment of linear model
fit.l <- lda(V58 ~. , data = train, prior = c(0.5, 0.5))
predict.l <- predict(fit.l, test)
accuracy.l <- sum(predict.l$class == test[, 58])/length(predict.l$class)
accuracy.l

#train, predict, performance assessment of quadratic model
fit.q <- qda(V58 ~. , data = train, prior = c(0.5, 0.5))
predict.q <- predict(fit.q, test)
accuracy.q <- sum(predict.q$class == test[, 58])/length(predict.q$class)
accuracy.q

#train, predict, performance assessment of k nearest neighbours model (unnormalised)
predict.k <- knn(train = train[, 1:57], test = test[, 1:57], cl = train[, 58], k=3)
accuracy.k <- sum(predict.k == test[, 58])/length(predict.k)
accuracy.k

#train, predict, performance assessment of k nearest neighbours model(normalised)
mus <- apply(train[, 1:57], 2 , mean)
sigmas <- apply(train[, 1:57], 2, sd)

for (i in 1:57){
  train[, i] <- (train[, i] - mus[i])/sigmas[i]
}

for (i in 1:57){
  test[,i] <- (test[, i] - mus[i])/sigmas[i]
}

predict.kn <- knn(train = train[, 1:57], test = test[, 1:57], cl = train[, 58], k=3)
accuracy.kn <- sum(predict.kn == test[, 58])/length(predict.kn)
accuracy.kn