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

f <- function(lambda, x) {
  dexp(x, rate = lambda)
}

L <- function(lambda, x) {
  prod(f(lambda, x))
}


logL <- function(lambda, x) {
  sum(log(f(lambda, x)))
}

n <- 100
x <- rexp(n, rate = 3)
logL(3.1, x)

lambdas <- seq(0, 6, 0.01)
logL.lambdas <- sapply(lambdas, function(lambda) logL(lambda, x))
plot(lambdas, logL.lambdas, type = "l")

x <- rexp(n, rate = 3)
opt <- optimize(logL, lower = 0, upper = 6, maximum = TRUE, x = x)
opt$maximum

sim.EMV <- function() {
  x <- rexp(n, rate = 3)
  opt <- optimize(logL, lower = 0, upper = 6, maximum = TRUE, x = x)
  lambda_real <- opt$maximum
  return(lambda_real)
}


simu <- replicate(10000, sim.EMV())
hist(simu)

mean(simu)
var(simu)

mean(simu) - 3

10000/(9999) *3 - 3

sim.Fisher <- function() {
  x <- rexp(n, rate = 3)
  
  logLx <- function(lambda) logL(lambda, x)
  # Information de Fisher
  (grad(logLx, 3))^2
  
}

sim.Fisher()

(inf.Fisher <- mean(replicate(10000, sim.Fisher())))
n/(3^2)
1/inf.Fisher
var(simu)


n <- 100
grad2 <- function(f, x) {
  df <- function(x) {
    grad(f, x)
  }
  grad(df, x)
}
sim.Fisher <- function() {
  x <- rexp(n, 3)
  # Log-vraisemblance par rapport à x
  logLx <- function(lambda) logL(lambda, x)
  # Information de Fisher
  grad2(logLx, 3)
}
(-mean(replicate(1000, sim.Fisher())))
