# 5
library(MASS)
attach(Boston)
# fix(Boston)

# 5.1
cat("\n")
medv_mean = mean(medv)
print(paste("Mean of medv: ", round(medv_mean, 2)))

# 5.2
medv_se = sd(medv) / sqrt(length(medv))
print(paste("Standard error: ", round(medv_se * 100, 2), "%"))

# 5.3
library(boot)
set.seed(1)

boot.fn = function(data, index) {
    return (mean(data[index]))
}

print(boot(medv, boot.fn, 100))

# 5.4
cat("\n")
print(t.test(medv))

ci = c(22.53 - 1.96 * 0.35, 22.53 + 1.96 * 0.35)
print(ci)

# 5.5
cat("\n")
medv_median = median(medv)
print(paste("Median of medv: ", round(medv_median, 2)))

# 5.6
boot.fn2 = function(data, index) {
    return (median(data[index]))
}

print(boot(medv, boot.fn2, 100))

# 5.7
cat("\n")
percentile = quantile(medv, c(0.1))
print(percentile)

# 5.8
boot.fn3 = function(data, index) {
    return (quantile(data[index], c(0.1)))
}

print(boot(medv, boot.fn3, 100))
