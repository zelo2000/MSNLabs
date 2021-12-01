# 1
library(ISLR)
library(boot)
set.seed(1)

cat("\n")
print(summary(Wage))

# 1.1
deltas = rep(0, 10)

for (i in 1:10) {
    fit = glm(wage ~ poly(age, i), data = Wage)
    deltas[i] = cv.glm(Wage, fit, K = 10)$delta[1]
}

plot(1:10, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
points(which.min(deltas), deltas[which.min(deltas)],
    col = "blue", cex = 2, pch = 20)
print(paste('Polynom power for min MSE: ', which.min(deltas)))
cat("\n")

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
print(anova(fit1, fit2, fit3, fit4, fit5,
 fit6, fit7, fit8, fit9, fit10))

PlotFunc = function(poly_pow = 0, is_poly = FALSE, is_cut = FALSE) {
    plot(wage ~ age, data = Wage, col = "darkgray")

    age.grid = seq(from = range(Wage$age)[1],
     to = range(Wage$age)[2])
    
    if (is_poly) { 
        fit = lm(wage ~ poly(age, poly_pow), data = Wage)
        pred = predict(fit, newdata = list(age = age.grid))
        lines(age.grid, pred, col = "blue", lwd = 2)
    }
    if (is_cut) {
        fit = lm(wage ~ cut(age, 8), data = Wage)
        pred = predict(fit, newdata = list(age = age.grid))
        lines(age.grid, pred, col = "blue", lwd = 2)
    }
}

par(mfrow = c(1, 2))
PlotFunc(2, TRUE)
PlotFunc(3, TRUE)

# 1.2
par(mfrow = c(1, 1))
cvs = rep(Inf, 10)

for (i in 2:10) {
    Wage$age.cut = cut(Wage$age, i)
    fit = glm(wage ~ age.cut, data = Wage)
    cvs[i] = cv.glm(Wage, fit, K = 10)$delta[1]
}

cat("\n")
plot(2:10, cvs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
points(which.min(cvs), cvs[which.min(cvs)], 
    col = "blue", cex = 2, pch = 20)
print(paste('Number of cuts for min MSE: ', which.min(cvs)))

PlotFunc(, , TRUE)
