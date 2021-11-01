# 1
library(ISLR)
attach(Default)
summary(Default)
# fix(Default)

# 1.1
set.seed(1)
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial")
summary(fit.glm)

# 1.2

# 1.2.1
train = sample(dim(Default)[1], dim(Default)[1] / 2)

# 1.2.2
fit.glm = glm(default ~ income + balance, data = Default, family = "binomial", subset = train)
summary(fit.glm)

# 1.2.3
Default.test = Default[-train,]
probs = predict(fit.glm, newdata = Default.test, type = "response")
pred.glm = rep("No", length(probs))
pred.glm[probs > 0.5] = "Yes"

# 1.2.4
paste("Eiao?o??io iiieeie: ", mean(pred.glm != Default.test$default))

cat("\n")

# 1.3
for (i in (0:2)) {
  train = sample(dim(Default)[1], dim(Default)[1] / 2)

  fit.glm = glm(default ~ income + balance, data = Default, family = "binomial", subset = train)

  Default.test = Default[-train,]
  probs = predict(fit.glm, newdata = Default.test, type = "response")
  pred.glm = rep("No", length(probs))
  pred.glm[probs > 0.5] = "Yes"

  print(paste("Eiao?o??io iiieeie: ", mean(pred.glm != Default.test$default)))
}

cat("\n")

# 1.4
train = sample(dim(Default)[1], dim(Default)[1] / 2)

fit.glm = glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train)

Default.test = Default[-train, ]

probs = predict(fit.glm, newdata = Default.test, type = "response")
pred.glm = rep("No", length(probs))
pred.glm[probs > 0.5] = "Yes"

paste("Коефіцієнт помилок:", mean(pred.glm != Default.test$default))
