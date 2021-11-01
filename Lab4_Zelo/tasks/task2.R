# 2
# 2.1
library(ISLR)
attach(Default)

set.seed(1)
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)$coef

# 2.2
boot.fn = function(data, index) {
    fit = glm(default ~ income + balance, data = data, family = "binomial", subset = index)
    return (coef(fit))
}

# 2.3
library(boot)
boot(Default, boot.fn, 100)
