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
print(paste('Correlation between x and y: ', cor(x, y)))

# 4.4 (manually)
print(paste('T-statistic: ', 
(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))))

# 4.5 (manually)

# 4.6
lm.fit = lm(y~x)
lm.fit2 = lm(x~y)

print(summary(lm.fit))
print(summary(lm.fit2))