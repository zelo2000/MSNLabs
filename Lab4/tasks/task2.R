# 2
# 2.1
library(ISLR)
attach(Default)
cat("\n")
set.seed(1)

fit.glm = glm(default ~ income + balance, data = Default, family = "binomial")
print(summary(fit.glm)$coef)

# 2.2
boot.fn = function(data, index) {
    fit = glm(default ~ income + balance, data = data, family = "binomial", subset = index)
    return (coef(fit))
}

# 2.3
library(boot)
print(boot(Default, boot.fn, 100))
