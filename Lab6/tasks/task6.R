# 6
# 6.1
set.seed(1)

X1 = rnorm(100)
X2 = rnorm(100)
eps = rnorm(100, sd = 0.1)

betas = runif(3, min=-5, max=5)
cat("\n")
print("Beta-values list:")
print(betas)

Y = betas[1] + betas[2] * X1 +
 betas[3] * X2 + eps

# 6.2
beta0 = rep(0, 1000)
beta1 = rep(0, 1000)
beta2 = rep(0, 1000)
beta1[1] = runif(1, min=-5, max=5)

cat("\n")
print(paste('Beta1[1] : ', beta1[1]))

# 6.3-6.5
for (i in 1:1000) {
    a = Y - beta1[i] * X1
    beta2[i] = lm(a ~ X2)$coef[2]

    a = Y - beta2[i] * X2
    lm.fit = lm(a ~ X1)

    if (i < 1000) {
        beta1[i + 1] = lm.fit$coef[2]
    }

    beta0[i] = lm.fit$coef[1]
}

plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas",
    ylim = c(-5, 5), col = "darkgreen")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")
legend("center", c("beta0", "beta1", "beta2"), 
    col = c("darkgreen", "red", "blue"))

# 6.6
lm.fit = lm(Y ~ X1 + X2)

plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas",
 ylim = c(-5, 5), col = "darkgreen")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")

abline(h = lm.fit$coef[1], lty = "dashed", lwd = 2, col = "black")
abline(h = lm.fit$coef[2], lty = "dashed", lwd = 2, col = "black")
abline(h = lm.fit$coef[3], lty = "dashed", lwd = 2, col = "black")

legend("center", c("beta0", "beta1", "beta2", "multiple regression"), lty = c(1, 
    1, 1, 2), col = c("darkgreen", "red", "blue", "black"))
