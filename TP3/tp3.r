runifa <- function(n) {
  if(!exists("param")) param <<- sample(10:20, 1)
  runif(n, min = 0, max = param)
}

runifa(100)
estim <- function(n) {
  return(2*mean(n))
}

a <- replicate(1000, estim(runifa(n)))
a
boxplot(a)

mk <- function(n,k) {
  ((k + 1) * mean(n^k))^(1/k)
}
k <- 100
ak <- mk(runifa(100), k)
ak


runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}

mu_ <- mean(runknown(100))
mu_
var_ <- var(runknown(100))
var_

hist(runknown(1000))
plot(ecdf(runknown(1000)))

mu <- 7.2
sigma <- sqrt(32.36)
n <- 100
x <- runknown(n)
T <- (mean(runknown(n)) - mu)/(sigma/sqrt(n))

m <- 100
hist(replicate(m, (mean(runknown(n)) - mu)/(sigma/sqrt(n))))


random.T <- function(n) {
  x <- runknown(n)
  (mean(x) - mu)/(sigma/sqrt(n))
}
t.1000 <- replicate(1000, random.T(n))
mean(t.1000)
var(t.1000)

plot(ecdf(t.1000))
curve(pnorm, add=TRUE)
