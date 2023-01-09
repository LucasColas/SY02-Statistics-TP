library(MASS)
immer
t.test(immer$Y1, immer$Y2, paired = TRUE)


difference <- immer$Y1 < immer$Y2
sign <- immer$Y1 < immer$Y2
sign[sign]
nsuccess <- length(sign[sign])
n <- length(sign)
prop.test(nsuccess, n, p = 0.5)

var.test(shoes$A, shoes$B)
t.test(shoes$A, shoes$B, var.equal = TRUE)

shapiro.test(galaxies)

delai <- read.table("data/delai-data.data", header = TRUE)$delai
min(delai)
(lambda <- 1 / mean(delai))
ks.test(delai, "pexp", lambda)
(breaks = quantile(delai, seq(0, 1, .1)))


(x = table(cut(delai, breaks=breaks, include.lowest = TRUE)))
(p = diff(c(0, pexp(breaks, 1 / mean(delai))[2:(length(breaks) - 1)], 1)))
pexp(breaks[2], 1 / mean(delai))
pexp(breaks[3], 1 / mean(delai))
chisq.test(x, p = p)

glace <- data.frame(chocolat = c(100, 350), vanille = c(120, 200), fraise = c(60, 90), row.names = c("homme", "femme"))
chisq.test(glace)

ct <- chisq.test(glace)
ct$observed
ct$expected
sum((ct$expected - glace)^2/ct$expected)
cold <- read.csv("data/cold.data", row.names = 1)
cold
chisq.test(cold)
