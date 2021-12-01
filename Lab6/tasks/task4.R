# 4
library(MASS)
set.seed(1)
cat("\n")
print(summary(Boston))

# 4.1
fit = lm(nox ~ poly(dis, 3), data = Boston)
print(summary(fit))

dis.grid = seq(from = range(Boston$dis)[1],
 to = range(Boston$dis)[2], by = 0.1)
preds = predict(fit, list(dis = dis.grid))

plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds, col = "blue", lwd = 2)

# 4.2
rss = rep(0, 10)
for (i in 1:10) {
    fit = lm(nox ~ poly(dis, i), data = Boston)
    rss[i] = sum(fit$residuals^2)
}
plot(1:10, rss, xlab = "Degree", ylab = "RSS", type = "l")
points(which.min(rss), rss[which.min(rss)],
 col = "blue", cex = 2, pch = 20)
print(paste('Polynom power for min RSS: ', which.min(rss)))
cat("\n")

# 4.3
library(boot)

deltas = rep(0, 10)
for (i in 1:10) {
    fit = glm(nox ~ poly(dis, i), data = Boston)
    deltas[i] = cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
points(which.min(deltas), deltas[which.min(deltas)],
 col = "blue", cex = 2, pch = 20)
print(paste('Polynom power for min MSE: ', which.min(deltas)))

# 4.4
library(splines)
fit = lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 10)), data = Boston)
print(summary(fit))

pred = predict(fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds, col = "blue", lwd = 2)

# 4.5
rss = rep(Inf, 15)
for (i in 3:15) {
    fit = lm(nox ~ bs(dis, df = i), data = Boston)
    rss[i] = sum(fit$residuals^2)
}
plot(3:15, rss[-c(1, 2)], xlab = "Degrees of freedom", ylab = "RSS", type = "l")
points(which.min(rss), rss[which.min(rss)],
 col = "blue", cex = 2, pch = 20)
print(paste('Degrees of freedom for min RSS: ', which.min(rss)))
cat("\n")

# 4.6
cv = rep(Inf, 15)
for (i in 3:15) {
    fit = glm(nox ~ bs(dis, df = i), data = Boston)
    cv[i] = cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(3:15, cv[-c(1, 2)], xlab = "Degrees of freedom", ylab = "Test MSE", type = "l")
points(which.min(cv), cv[which.min(cv)],
 col = "blue", cex = 2, pch = 20)
print(paste('Degrees of freedom for min MSE: ', which.min(cv)))
cat("\n")
