trainset <- as.matrix(read.csv('~/authorshiptrainingset.csv',header=FALSE))
testset <- as.numeric(read.csv('~/cuckooscalling.csv',header=FALSE))

C <- dim(trainset)[1]
pis <- rep(1/C,C)

thetas <- matrix(numeric(C*dim(trainset)[2]),nrow=C)

for (i in 1:C){
  thetas <- rbind(trainset, trainset[i,]/sum(trainset[i,]))
}

posteriors <- numeric(C)

for (i in 1:C){
  posteriors[i] <- log(pis[i]) + dmultinom(testset,prob=thetas[i,],log=TRUE)
}

posteriors
