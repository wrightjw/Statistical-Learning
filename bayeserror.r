f <- function(x){
  0.5*log(sigma1)-0.5*log(sigma0)+(x-mu1)^2/(2*sigma1^2) - (x-mu0)^2/(2*sigma0^2)
}

p <- 0.5 #probability of each classification with equal species
mu0 <- 4.8; sigma0 <- sqrt(0.1) #setosa distribution
mu1 <- 6; sigma1 <- sqrt(0.25) #versicolour distribution

x <- 5.5

p0 <- p*dnorm(x,mu0,sigma0) #probability of setosa classification
p1 <- (1-p)*dnorm(x,mu1,sigma1) #probability of versicolour classification

uniroot(f,lower=3,upper=6) #decision boundary

bayes_error <- p*(1-pnorm(gamma,mu0,sigma0)) + (1-p)*(pnorm(gamma, mu1,sigma1))
bayes_error
