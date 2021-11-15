# 4
library(MASS)
attach(Boston)
print(summary(Boston))

# 4.1-4.3
# best subset selection
library(leaps)

predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coef_i = coef(object, id = id)
    xvars = names(coef_i)
    mat[, xvars] %*% coef_i
}

k = 10
folds = sample(1:k, nrow(Boston), replace = TRUE)
cv.errors = matrix(0, k, 13)

for (j in 1:k) {
    best.fit = regsubsets(crim ~ ., data = Boston[folds != j, ], nvmax = 13)
    for (i in 1:13) {
        pred = predict.regsubsets(best.fit, Boston[folds == j, ], id = i)
        cv.errors[j, i] = mean((Boston$crim[folds == j] - pred)^2)
    }
}

mean.cv.errors = rep(0, 13)
for (i in 1:13) {
  mean.cv.errors[i] = mean(cv.errors[, i])
}

cat("\n")
print(paste('Model size for min CV: ', which.min(mean.cv.errors),
 ', min CV error: ', min(mean.cv.errors)))
plot(mean.cv.errors, xlab = "Кількість змінних", ylab = "Помилка кросвалідації",
     col = "red", type = "b")

# lasso
cat("\n")
library(glmnet)
x = model.matrix(crim ~ ., Boston)[, -1]
y = Boston$crim

cv.lasso = cv.glmnet(x, y, alpha = 1, type.measure = "mse")
cat("\n")
print(paste('Lasso: Min lambda: ', cv.lasso$lambda.min,
 ', min CV error: ', min(cv.lasso$cvm)))
plot(cv.lasso)

# ridge
cv.ridge = cv.glmnet(x, y, alpha = 0, type.measure = "mse")
cat("\n")
print(paste('Ridge: Min lambda: ', cv.ridge$lambda.min,
 ', min CV error: ', min(cv.ridge$cvm)))
plot(cv.ridge)

# PCR
cat("\n")
library(pls)

fit.pcr = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
print(summary(fit.pcr))

cat("\n")
print(paste('Min M: ', which.min(fit.pcr$validation$adj),
 ', min CV error: ', min(fit.pcr$validation$adj)))

validationplot(fit.pcr, val.type = "MSEP")
