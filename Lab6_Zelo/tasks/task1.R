# 1
# 1.1
library(ISLR)
library(boot)
set.seed(1)

deltas = rep(NA, 10)
for (i in 1:10) {
    fit = glm(wage ~ poly(age, i), data = Wage)
    deltas[i] = cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(1:10, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
d.min = which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)

fit1 = lm(wage ~ age, data = Wage)
fit2 = lm(wage ~ poly(age, 2), data = Wage)
fit3 = lm(wage ~ poly(age, 3), data = Wage)
fit4 = lm(wage ~ poly(age, 4), data = Wage)
fit5 = lm(wage ~ poly(age, 5), data = Wage)
fit6 = lm(wage ~ poly(age, 6), data = Wage)
fit7 = lm(wage ~ poly(age, 7), data = Wage)
fit8 = lm(wage ~ poly(age, 8), data = Wage)
fit9 = lm(wage ~ poly(age, 9), data = Wage)
fit10 = lm(wage ~ poly(age, 10), data = Wage)
anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10)

plot(wage ~ age, data = Wage, col = "darkgrey")
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
fit = lm(wage ~ poly(age, 2), data = Wage)
preds = predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

plot(wage ~ age, data = Wage, col = "darkgrey")
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
fit = lm(wage ~ poly(age, 3), data = Wage)
preds = predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

# 1.2
cvs = rep(NA, 10)
for (i in 2:10) {
    Wage$age.cut = cut(Wage$age, i)
    fit = glm(wage ~ age.cut, data = Wage)
    cvs[i] = cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
d.min = which.min(cvs)
points(which.min(cvs), cvs[which.min(cvs)], col = "red", cex = 2, pch = 20)

plot(wage ~ age, data = Wage, col = "darkgrey")
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
fit = glm(wage ~ cut(age, 8), data = Wage)
preds = predict(fit, data.frame(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)