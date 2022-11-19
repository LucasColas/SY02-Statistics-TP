source("src/utils.R")

chisq1 <- function(mu, sigma, n) {
  x <- rnorm(n, mean=mu, sd=sigma)
  return (((n-1)*var(x))/(sigma^2))
}

mu <- 3
n <- 100 
sigma <- 2

chisq1000 <- replicate(1000, chisq1(mu, sigma, n))
hist(chisq1000, freq = FALSE)
curve(dchisq(x, df = n-1), add = TRUE)

stu1 <- function() {
  x <- rnorm(n, mean = mu, sd = sigma)
  (mean(x) - mu)/(sd(x)/sqrt(n))
}

stu1000 <- replicate(1000, stu1())
hist(stu1000, freq = FALSE)
curve(dt(x, df = n - 1), add = TRUE)

n <- 100
x <- rnorm(n, mean = 42, sd = pi)
alpha <- 0.05
#IC bilatéral pour échantillon de var IID de loi normale avec var connue
mean(x) + c(-1, 1) * qnorm(1 - alpha/2) * pi/sqrt(n)
IC <- c(mean(x) + c(-1) * qnorm(1 - alpha/2) * pi/sqrt(n), mean(x) + c(1) * qnorm(1 - alpha/2) * pi/sqrt(n))
IC

#IC bilatéral pour échantillon de var IID de loi normale avec var inconnu
mean(x) + c(-1, 1) * qt(1 - alpha/2, df = n - 1) * sd(x)/sqrt(n)
t.test(x, conf.level = 1 - alpha)$conf.int

gen_IC <- function(x, alpha) {
  n <- length(x)
  mean(x) + c(-1, 1) * qt(1 - alpha/2, df = n - 1) * sd(x)/sqrt(n)
}

param <- 42
alpha <- 0.05
ICs <- replicate(1000, gen_IC(rnorm(100, mean = param), alpha))
ICs10 <- replicate(100, gen_IC(rnorm(100, mean = param, sd = 1), alpha))
ICs100 <- replicate(100, gen_IC(rnorm(100, mean = param, sd = 2), alpha))
ICs1000 <- replicate(100, gen_IC(rnorm(100, mean = param, sd = 4), alpha))
plot_ICs(ICs10, param, xlim = c(39, 44), main = "sigma = 1")
plot_ICs(ICs100, param, xlim = c(39, 44), main = "sigma = 2")
plot_ICs(ICs1000, param, xlim = c(39, 44), main = "sigma = 4")

Bon_ICs <- function(n, param, alpha) {
  x <- rnorm(n, mean = param)
  IC <- gen_IC(x, alpha)
  param >= IC[1] & param <= IC[2]
}

n <- 100
alpha <- 0.05
hm <- replicate(100,replicate(10000, Bon_ICs(n, 3, alpha)))
