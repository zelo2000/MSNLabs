# 3
# 3.1
library(ISLR)
attach(Weekly)
set.seed(1)

fit.glm = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
summary(fit.glm)$coef
cat("\n")

# 3.2
fit.glm2 = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = "binomial")
summary(fit.glm2)$coef

# 3.3
cat("\n")
predict.glm(fit.glm2, Weekly[1, ], type = "response") > 0.5

cat("\n")

# 3.4
err = rep(0, dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
    fit.glm = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ],  family = "binomial")
    if (predict.glm(fit.glm, Weekly[i, ], type = "response") > 0.5) {
      if (Direction[i] == "Down") {
        err[i] = 1
      }
    }
}

# 3.5
paste("ЮіГэър LOOCV :", mean(err))
