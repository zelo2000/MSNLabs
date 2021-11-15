# 1.1
set.seed(1)
x = rnorm(100)
eps = rnorm(100)

# 1.2
cat("\n")
betas = runif(5, min=-5, max=5)
print("Beta-values list:")
print(betas)

y = betas[1] + betas[2] * x +
 betas[3] * x^2 + betas[4] * x^3 + eps

# 1.3
library(leaps)
data.frame = data.frame(y = y, x = x)

BestModelSelection = function(method_, mtext_) {
    cat("\n")
    reg.fit = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) 
    + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.frame, nvmax = 10, method = method_)
    reg.summary = summary(reg.fit)
    print(reg.summary)

    par(mfrow = c(2, 2))

    plot(reg.summary$cp, xlab = "Кiлькість змiнних", ylab = "Cp", type = "l")
    points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)],
    col = "blue", cex = 2, pch = 20)

    plot(reg.summary$bic, xlab = "Кiлькість змiнних", ylab = "BIC", type = "l")
    points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)],
    col = "blue", cex = 2, pch = 20)

    plot(reg.summary$adjr2, xlab = "Кiлькість змiнних", ylab = "Adjusted R^2", type = "l")
    points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)],
    col = "blue", cex = 2, pch = 20)

    mtext(mtext_, side = 3, line = -2, outer = TRUE)

    cat("\n")

    point.a = which.min(reg.summary$cp)
    point.b = which.min(reg.summary$bic)
    point.c = which.max(reg.summary$adjr2)

    if (point.a == point.b && point.a == point.c) {
      print(coef(reg.fit, point.a))
    } else if (point.a == point.b && point.a != point.c) {
      print(coef(reg.fit, point.b))
      print(coef(reg.fit, point.c))
    } else if (point.a != point.b && point.a == point.c || 
              point.a != point.b && point.b == point.c) {
      print(coef(reg.fit, point.a))
      print(coef(reg.fit, point.b))
    } else if (point.a != point.b && point.b == point.c) {
    } else {
      print(coef(reg.fit, point.a))
      print(coef(reg.fit, point.b))
      print(coef(reg.fit, point.c))
    }
}

BestModelSelection("exhaustive",
 "Графiки C.p, BIC та скорегованого R^2")

# 1.4
BestModelSelection("forward",
 "Графiки C.p, BIC та скорегованого R^2 для покрокового вибору вперед")
BestModelSelection("backward",
 "Графiки C.p, BIC та скорегованого R^2 для покрокового вибору назад")

# 1.5
cat("\n")
library(glmnet)

Lasso = function() {
    cat("\n")
    xmat = model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) 
        + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.frame)[, -1]

    cv.out = cv.glmnet(xmat, y, alpha = 1)
    par(mfrow = c(1, 1))

    bestlam = cv.out$lambda.min
    print(paste('Min lambda: ', bestlam))
    plot(cv.out)

    out = glmnet(xmat, y, alpha = 1)
    lasso.coefs = predict(out, s = bestlam, type = "coefficients")[1:11, ]
    print(lasso.coefs[lasso.coefs != 0])
}

Lasso()

# 1.6
y = betas[1] + betas[5] * x^7 + eps
data.frame = data.frame(y = y, x = x)

BestModelSelection("exhaustive",
 "Графiки C.p, BIC та скорегованого R^2")
Lasso()
