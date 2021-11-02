# 5
library(MASS)
attach(Boston)
# fix(Boston)

# 5.1
mean(medv)

# 5.2
medv_se = sd(medv) / sqrt(length(medv))
paste("Noaiaa?oia iioeaea: ", round(medv_se, 2))

# 5.3
library(boot)
set.seed(1)

boot.fn = function(data, index) {
    return (mean(data[index]))
}

boot(medv, boot.fn, 1000)

# 5.4
cat("\n")
t.test(medv)

ci = c(22.533 - 2 * 0.411, 22.533 + 2 * 0.411)
ci

# 5.5
cat("\n")
median(medv)

# 5.6
boot.fn2 = function(data, index) {
    return (median(data[index]))
}

boot(medv, boot.fn2, 1000)

# 5.7
cat("\n")
quantile(medv, c(0.1))

# 5.8
boot.fn3 = function(data, index) {
    return (quantile(data[index], c(0.1)))
}

boot(medv, boot.fn3, 1000)

