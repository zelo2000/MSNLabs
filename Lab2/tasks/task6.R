# 6
set.seed(1)

# 6.1
x = rnorm(100)

print(x)
cat("\n")

# 6.2
eps = rnorm(100, 0, 0.25)
print(eps)

# 6.3
cat("\n")
y = -1 + 0.5*x + eps
print(paste('Vector Y length: ', length(y)))
print('Beta_0 = -1, Beta_1 = 0.5')

# 6.4
plot(x, y)
print(paste('Correlation between x and y: ', cor(x, y)))

# 6.5
fit1 = lm(y~x)
print(summary(fit1))

# 6.6
plot(x, y)
legend("topleft", legend="Approximation line", fill="red", cex=0.9)
abline(fit1, lwd=3, col="red")

# 6.7
fit2 = lm(y ~ x + I(x^2))
print(summary(fit2))
print(anova(fit1, fit2))

# 6.8
eps2 = rnorm(100, 0, 0.05)
y2 = -1 + 0.5*x + eps2

fit3 = lm(y2~x)
print(summary(fit3))

# 6.9
eps3 = rnorm(100, 0, 0.5)
y3 = -1 + 0.5*x + eps3

fit4 = lm(y3~x)
print(summary(fit4))

# 6.10
print(confint(fit1))
print(confint(fit3))
print(confint(fit4))