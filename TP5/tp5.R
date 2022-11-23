require(stats)
attach(anscombe)
a <- read.csv('data/hooker-data.data')
x <- c(0, 0.2, 0.3, 0.6)
y <-  c(1.01, 1.44, 1.55, 2.1)
donnees <- data.frame(varx = x,
                      vary = y)
reg <- lm(vary~varx, data = donnees)
summary(reg)
coeff = coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))

plot(x, y, main = eq)
abline(reg, col="blue")

Res = residuals(reg)
sum(Res)

#prediction y barre
coeff[2]*mean(x) + coeff[1]
mean(y)

#Variance totale 
Sy2 <- mean(y^2) - mean(y)^2
Sy2

#Variance expliquée par la régression
Sreg <- mean(reg$fitted.values^2)-mean(y)^2
Sreg

#Variance résiduelle 
Sres <- mean((reg$residuals)^2)
Sres

#Variance totale 
Stot <- Sreg+Sres
Stot


R2 <- Sreg/Sy2
R2


cor(reg$fitted.values,y , method='pearson')
m1 <- lm(y1 ~ x1)
plot(x1, y1)
abline(m1$coefficients[1], m1$coefficients[2])

m2 <- lm(y2 ~ x2)
plot(x2, y2)
abline(m2$coefficients[1], m2$coefficients[2])

m3 <- lm(y3 ~ x3)
plot(x3, y3)
abline(m3$coefficients[1], m3$coefficients[2])

m4 <- lm(y4 ~ x4)
plot(x4, y4)
abline(m4$coefficients[1], m4$coefficients[2])


#6
summary(m4)
qqnorm(m1$residuals)
rstandard(m4)

#normalité
qqnorm(rstandard(m1))
qqline(rstandard(m1))
hist(rstandard(m4), freq = FALSE)

plot(x4,rstandard(m4))
plot(m4$fitted.values, rstandard(m4))

#Prediction
summary(a)
a

#Modèle
model_hooker <- lm(Pression ~ Temp, data = a)

#Vérifications des hypothèses
plot(a$Temp, a$Pression)
abline(model_hooker$coefficients[1], model_hooker$coefficients[2])


summary(model_hooker)
qqnorm(model_hooker$residuals)
qqline(model_hooker$residuals)
plot(model_hooker$fitted.values, rstandard(model_hooker))
?predict.lm
?confint
confint(model_hooker, level = 0.99)
data_test <- data.frame(Temp = c(97))
predict(model_hooker, data_test, level=0.99, interval = 'confidence')
