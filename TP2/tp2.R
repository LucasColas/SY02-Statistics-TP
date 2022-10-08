library(MASS)

head(painters)

par(mfrow = c(2,2))
hist(painters$Composition, main = "Composition", xlab = "Note")
hist(painters$Drawing, main = "Dessin", xlab="Notes")
hist(painters$Colour, main= "Couleur", xlab = "Note")
hist(painters$Expression, main = "Expression", xlab="Note")

moyenne <- (painters[,1]+painters[,2]+painters[,3]+painters[,4])/4

moyenne2 <- rowMeans(painters[,1:4])

n <- length(moyenne)
(x_ <- sum(moyenne)/n) # moyenne empirique

(s2 <- sum((moyenne - x_)^2)/n) # variance empirique

(s <- sqrt(s2)) # écart-type empirique

(s2_ <- sum((moyenne - x_)^2)/(n - 1)) # variance empirique corrigée

(s_ <- sqrt(s2_)) # écart-type empirique corrigé

par(mfrow= c(1,1))
hist(moyenne)

dunif(4, min = 2, max=5)

punif(4, min = 2, max=5)
1-pnorm(3)
pnorm(42,35, 6)
pnorm(50, 35,6) - pnorm(40, 35, 6)
dbinom(4,5,0.5)
dbinom(29,30,0.5)
1-pbinom(14, 20, 0.5)
sum(dbinom(10:15, 20, 0.5))
pbinom(15, 20, 0.5) - pbinom(9,20,0.5)



alpha <- c(0.05, 0.1, 0.9)
qnorm(alpha)
qchisq(alpha, 10)

qt(alpha, 5)
qf(alpha, 2,5)

dloi <- function(x,b) {
  
  a <- 2/(b**2)
  f <- a*x
  f[f > b] <- 0
  f[f < 0] <- 0
  return(f)
}

dloi(-1:5, 3)

curve(dloi(x,3), from = -5, to = 5)

ploi <- function(x,b) {
  
  
  F <- x^2/b^2
  F[x < 0] <- 0
  F[x>=b] <- 1
  return(F)
}

curve(ploi(x,3), from = -5, to = 5)

qloi <- function(alpha, b) {
  fa <- b*sqrt(alpha)
  fa[alpha == 0] <- 0
  fa[alpha == 1] <- b
  return(fa)
}

curve(qloi(x, 3), from = 0, to = 1)
rloi <- function(n, b) {
  u <- runif(n)
  x <- qloi(u, b)
  return(x)
}

par(mfrow = c(2,2))
for (n in c(10, 50, 10000, 50000)) {
  hist(rloi(n, 3), breaks = round(1 + 10/3 * log10(n)), freq = FALSE, main = n, xlim = c(-1, 4))
  curve(dloi(x, 3), add = TRUE, col = "red")
}
