bottles <- read.csv("data/bottles.data")
bottles$Volume

t.test(bottles, mu = 500, alternative = "less", conf.level = 0.90)


mm <- read.csv("data/MM.data")
mm
sum(mm)
prop.test(mm$Red, sum(mm), 1/6)

prop.test(mm$Green, sum(mm), 1/6)
prop.test(mm$Blue, sum(mm), 1/6)
prop.test(mm$Orange, sum(mm), 1/6)
prop.test(mm$Yellow, sum(mm), 1/6)
prop.test(mm$Brown, sum(mm), 1/6)

install.packages("isdals")
library(isdals)
data("bodyfat")
par(mfrow=c(1,3))
plot(Fat~Thigh,data = bodyfat)
plot(Fat~Triceps,data = bodyfat)
plot(Fat~Midarm,data = bodyfat)

regFTh<-lm(Fat~Thigh,data=bodyfat)
summary(regFTh)

delai <- read.csv("data/delai-data.data")
delai$delai
theta0 <- 1 / 151
n <- length(delai$delai)
(mean(delai$delai) < ((qnorm(0.05) / sqrt(n)) + 1) * (1 / theta0))

pnorm((mean(delai$delai) - 1 / theta0) / (1 / (theta0 * sqrt(n))))


puiss_emp <- function(theta, theta0, n) {
  x <- rexp(n, theta)
  return(mean(x) < ((qnorm(0.05) / sqrt(n)) + 1) * (1 / theta0))
}

theta0 <- 1 / 151
mean(replicate(100, puiss_emp(theta0, theta0, 100)))


data("sleep")
sleep


x1 <- sleep$extra[sleep$group == 1]
x2 <- sleep$extra[sleep$group == 2]
shapiro.test(x1)

shapiro.test(x2)
t.test(x1, mu = 0, alternative = "greater")
t.test(x2, mu = 0, alternative = "greater")
