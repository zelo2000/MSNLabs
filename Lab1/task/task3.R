# 3.1
library (MASS)
Boston
?Boston

# 3.2
pairs(Boston)

# 3.3
pairs(Boston, verInd=1)

# 3.4
Boston$chas = as.factor(Boston$chas)
summary(Boston)