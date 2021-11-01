# 4
# 4.1
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

# 4.2
plot(x, y)

# 4.3
library(boot)

Coords = data.frame(x, y)

LOOCV = function(seed) {
    set.seed(seed)
    cat("\n")
    print(paste("seed --- ", seed))
      # 4.3.1
    fit.glm = glm(y ~ x)
    print(paste("LOOCV оцiнка для тестової помилки [beta0 - beta1]: ",
    round(cv.glm(Coords, fit.glm)$delta[1], 2), "%"))

      # 4.3.2
    fit.glm2 = glm(y ~ x + I(x^2))
    print(paste("LOOCV оцiнка для тестової помилки [beta0 - beta2]: ",
    round(cv.glm(Coords, fit.glm2)$delta[1], 2), "%"))

      # 4.3.3
    fit.glm3 = glm(y ~ poly(x, 3))
    print(paste("LOOCV оцiнка для тестової помилки [beta0 - beta3]: ",
    round(cv.glm(Coords, fit.glm3)$delta[1], 2), "%"))

      # 4.3.4
    fit.glm4 = glm(y ~ poly(x, 4))
    print(paste("LOOCV оцiнка для тестової помилки [beta0 - beta4]: ",
    round(cv.glm(Coords, fit.glm4)$delta[1], 2), "%"))
}

LOOCV(1)

# 4.4
LOOCV(10)
LOOCV(100)

# 4.5
fit.glm = glm(y ~ poly(x, 4))
print(summary(fit.glm))
