# 1
library(ISLR)
attach(Default)
cat("\n")
print(summary(Default))
# fix(Default)

# 1.1
set.seed(1)
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial")
print(summary(fit.glm))

# 1.2
TestError = function(is_detailed) {
    #   1.2.1
    train = sample(length(balance), length(balance) / 2)

    #   1.2.2
    fit.glm = glm(default ~ income + balance, data = Default,
     family = "binomial", subset = train)

    if (is_detailed) {
      print(summary(fit.glm)$coef)
      cat("\n")
    }

    #   1.2.3
    Default.test = Default[-train, ]

    probs = predict(fit.glm, newdata = Default.test, type = "response")
    pred.glm = rep("No", length(probs))
    pred.glm[probs > 0.5] = "Yes"

    #  1.2.4
    print(paste("Тестова помилка: ", mean(pred.glm != Default.test$default) * 100, "%"))
}

TestError(TRUE)

cat("\n")

# 1.3
for (i in (0:2)) {
  TestError(FALSE)
}

cat("\n")

# 1.4
train = sample(length(balance), length(balance) / 2)

fit.glm = glm(default ~ income + balance + student,
 data = Default, family = "binomial", subset = train)

Default.test = Default[-train, ]

probs = predict(fit.glm, newdata = Default.test, type = "response")
pred.glm = rep("No", length(probs))
pred.glm[probs > 0.5] = "Yes"

print(paste("Тестова помилка: ", mean(pred.glm != Default.test$default) * 100, "%"))
