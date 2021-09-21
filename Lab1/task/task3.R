# 3.1
library (MASS)
Boston
?Boston

# 3.2
pairs(~age+tax+crim+rad+ptratio, Boston)

# 3.3
pairs(Boston, verInd=1)
cat("\n")

# 3.4
Boston$chas = as.factor(Boston$chas)
print(summary(Boston))
attach(Boston)
hist(crim)
hist(tax)
hist(ptratio)
cat("\n")

# 3.5
print(paste("Bounds river:", sum(Boston$chas == 1)))
cat("\n")

# 3.6
print(paste("Median P/t ratio:", median(Boston$ptratio)))
cat("\n")

# 3.7
min_medv = Boston[Boston$medv == min(Boston$medv), ]
print(min_medv)
cat("\n")
print(min_medv[,-4] - apply(Boston[,-4], 2, mean))
