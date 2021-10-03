# 4
set.seed(1)
x = rnorm(100)
y = 2*x+rnorm(100)

print(x)
cat("\n")
print(y)

# 4.1
fit1 = lm(y~x+0)
print(summary(fit1))

# 4.2
fit2 = lm(x~y+0)
print(summary(fit2))

# 4.3
print(paste('Correlation x, y: ', cor(x, y)))

# 4.4

# 4.5

# 4.6