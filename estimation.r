findBoundary <-function(x,y,lower=4,upper=6) {
  x0 <- x[y==0]
  x1 <- x[y==1]
  mu0 <- mean(x0); sigma0 <- sd(x0)
  mu1 <- mean(x1); sigma1 <- sd(x1)
  f <-function(x) {
    0.5*log(sigma1) - 0.5*log(sigma0) + (x-mu1)^2 / (2*sigma1^2) - (x-mu0)^2 / (2*sigma0^2)
  }
  gamma <- uniroot(f,lower=lower,upper=upper)$root
  return(gamma)
}

x0 <- iris[iris$Species == 'setosa',]$Sepal.Length
x1 <- iris[iris$Species == 'versicolor',]$Sepal.Length
x <- c(x0, x1)
#y is known for the historical data
truey <- c(rep(0,length(x0)), rep(1,length(x1)))
findBoundary(x,y)


#set up axes
xaxis <- seq(4,7,length=1000)
yaxis <- seq(0,1.2,length=1000)
plot(xaxis,yaxis,type=’n’)
#plot kernel density estimates
lines(density(x0))
lines(density(x1),col=’red’)
#add decision boundary
abline(v=5.42,lty=3,lwd=2)

#performance assessment
y <- c(rep(0,length(x0)), rep(1,length(x1)))
n <- length(x)
#randomly choose the training set. We do it by selecting indexes so that we assign paired vales of x and y to both sets. The set.seed
#function initialises the ranodm number generator so that we get
#the same random numbers every time we run the code (so the
# results can be replicated)
set.seed(1)
trainindices <- sample(1:n, round(0.66*n), replace=FALSE)
trainx <- x[trainindices]
trainy <- y[trainindices]
testx <- x[-trainindices]
testy <- y[-trainindices]

gamma <- findBoundary(trainx,trainy)
ypreds <- numeric(length(testx))
ypreds[testx<=gamma] <- 0
ypreds[testx>gamma] <- 1

sum(ypreds!=testy)/length(testy)
