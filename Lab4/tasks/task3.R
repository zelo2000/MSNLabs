# 3
# 3.1
library(ISLR)
attach(Weekly)
set.seed(1)

fit.glm = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
print(summary(fit.glm))

# 3.2
fit.glm2 = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = "binomial")
print(summary(fit.glm))

# 3.3
print(contrasts(Direction))
cat("\n")

if (predict.glm(fit.glm2, Weekly[1, ], type = "response") > 0.5) {
  if (Direction[1] == "Up") {
    print("Правильне прогнозування Direction для першого спостереження")
  }
  else { print("Неправильне прогнозування Direction для першого спостереження") }
}

# 3.4
error_list = rep(0, length(Direction))
cat("\n")

for (i in 1:length(Direction)) {
    fit.glm = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ],  family = "binomial")
    
    if (predict.glm(fit.glm2, Weekly[i, ], type = "response") > 0.5) {
      if (Direction[i] == "Down") {
        error_list[i] = 1
      }
    }
}

# 3.5
print(paste("LOOCV для тестової помилки: ", mean(error_list), "%"))
