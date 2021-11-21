# 4

# 4.1
library(MASS)
set.seed(1)
fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)

dislims = range(Boston$dis)
dis.grid = seq(from = dislims[1], to = dislims[2], by = 0.1)
preds = predict(fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds, col = "red", lwd = 2)

# 4.2
rss = rep(NA, 10)
for (i in 1:10) {
    fit = lm(nox ~ poly(dis, i), data = Boston)
    rss[i] = sum(fit$residuals^2)
}
plot(1:10, rss, xlab = "Degree", ylab = "RSS", type = "l")

# 4.3
library(boot)
deltas = rep(NA, 10)
for (i in 1:10) {
    fit = glm(nox ~ poly(dis, i), data = Boston)
    deltas[i] = cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")

# 4.4
library(splines)
fit = lm(nox ~ bs(dis, knots = c(4, 7, 11)), data = Boston)
summary(fit)

pred = predict(fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, preds, col = "red", lwd = 2)

# 4.5
rss = rep(NA, 16)
for (i in 3:16) {
    fit = lm(nox ~ bs(dis, df = i), data = Boston)
    rss[i] = sum(fit$residuals^2)
}
plot(3:16, rss[-c(1, 2)], xlab = "Degrees of freedom", ylab = "RSS", type = "l")

# 4.6
cv = rep(NA, 16)
for (i in 3:16) {
    fit = glm(nox ~ bs(dis, df = i), data = Boston)
    cv[i] = cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(3:16, cv[-c(1, 2)], xlab = "Degrees of freedom", ylab = "Test MSE", type = "l")