# 3
library(boot)
set.seed(1)

setwd('E:\\Study\\5_course\\MSNLabs\\Lab6\\tasks')
autos = read.csv('Auto.csv', header = T, na.string = '?')
autos = na.omit(autos)
attach(autos)

cat("\n")
print(cor(autos[, -9]))
pairs(autos[, -9])
cat("\n")

deltas = rep(0, 15)
for (i in 1:15) {
    fit = glm(mpg ~ poly(displacement, i), data = autos)
    deltas[i] = cv.glm(autos, fit, K = 10)$delta[1]
}
plot(1:15, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
points(which.min(deltas), deltas[which.min(deltas)],
 col = "blue", cex = 2, pch = 20)
print(paste('Polynom power for min MSE: ', which.min(deltas)))
cat("\n")

cvs = rep(Inf, 10)
for (i in 2:10) {
    autos$dis.cut = cut(displacement, i)
    fit = glm(mpg ~ dis.cut, data = autos)
    cvs[i] = cv.glm(autos, fit, K = 10)$delta[1]
}
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
points(which.min(cvs), cvs[which.min(cvs)],
 col = "blue", cex = 2, pch = 20)
print(paste('Number of cuts for min MSE: ', which.min(cvs)))
cat("\n")

library(splines)
cvs = rep(Inf, 10)
for (i in 3:10) {
    fit = glm(mpg ~ ns(displacement, df = i), data = autos)
    cvs[i] = cv.glm(autos, fit, K = 10)$delta[1]
}
plot(3:10, cvs[-c(1, 2)], xlab = "Degree", ylab = "Test MSE", type = "l")
points(which.min(cvs), cvs[which.min(cvs)], 
col = "blue", cex = 2, pch = 20)
print(paste('Degrees of freedom for min MSE [splines]: ', which.min(cvs)))
cat("\n")

library(gam)
fit = gam(mpg ~ s(displacement, df = 2) +
 s(horsepower, df = 2), data = autos)
print(summary(fit))
