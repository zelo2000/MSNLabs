# 2
library(ISLR)
attach(College)
print(summary(College))

# 2.1
set.seed(1)
train = sample(1:length(Apps), 0.5 * length(Apps))
College.train = College[train, ]
College.test = College[-train, ]

# 2.2
# print(cor(College[, -1]))
fit.lm = lm(Apps ~ ., data = College.train)
pred.lm = predict(fit.lm, College.test)
cat("\n")
print(paste("Test error: ", round(mean((pred.lm - College.test$Apps)^2), 2)))

# 2.3
cat("\n")
library(glmnet)
train.mat = model.matrix(Apps ~ ., data = College.train)
test.mat = model.matrix(Apps ~ ., data = College.test)

grid = 10 ^ seq(10, -2, length = 100)
fit.ridge = glmnet(train.mat, College.train$Apps, alpha = 0,
 lambda = grid)
cv.ridge = cv.glmnet(train.mat, College.train$Apps, alpha = 0,
 lambda = grid)
cat("\n")
print(dim(coef(fit.ridge)))

bestlam = cv.ridge$lambda.min
print(paste('Min lambda: ', bestlam))
pred.ridge = predict(fit.ridge, s = bestlam, newx = test.mat)
print(paste("Test error: ", round(mean((pred.ridge - College.test$Apps)^2), 2)))

# 2.4
fit.lasso = glmnet(train.mat, College.train$Apps, alpha = 1,
 lambda = grid)
cv.lasso  = cv.glmnet(train.mat, College.train$Apps, alpha = 1,
 lambda = grid)
cat("\n")

bestlam = cv.lasso$lambda.min
print(paste('Min lambda: ', bestlam))
pred.lasso = predict(fit.lasso, s = bestlam, newx = test.mat)
print(paste("Test error: ", round(mean((pred.lasso - College.test$Apps)^2), 2)))

cat("\n")
lasso.coefs = predict(fit.lasso, s = bestlam, type = "coefficients")
print(lasso.coefs)

# 2.5
library(pls)
fit.pcr = pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP", xlab = "Кількість змінних")

cat("\n")
print(paste('Min M: ', which.min(fit.pcr$validation$adj)))
pred.pcr = predict(fit.pcr, College.test, ncomp = which.min(fit.pcr$validation$adj))
print(paste("Test error: ", round(mean((pred.pcr - College.test$Apps)^2), 2)))

# 2.6
fit.pls = plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP", xlab = "Кількість змінних")

cat("\n")
print(paste('Min M: ', which.min(fit.pls$validation$adj)))
pred.pls = predict(fit.pls, College.test, ncomp = which.min(fit.pls$validation$adj))
print(paste("Test error: ", round(mean((pred.pls - College.test$Apps)^2), 2)))

# 2.7
test.mean = mean(College.test$Apps)
methods.list = c(pred.lm, pred.ridge, pred.lasso, pred.pcr, pred.pls)

GetRSqr = function(m) {
  r_result = 1 - mean((m - College.test$Apps)^2) / mean((test.mean - College.test$Apps)^2)
  return(round(r_result, 7)*100)
}

cat("\n")
print(paste("lm prediction R^2: ", GetRSqr(pred.lm), " %"))
print(paste("ridge prediction R^2: ", GetRSqr(pred.ridge), " %"))
print(paste("lasso prediction R^2: ", GetRSqr(pred.lasso), " %"))
print(paste("pcr prediction R^2: ", GetRSqr(pred.pcr), " %"))
print(paste("pls prediction R^2: ", GetRSqr(pred.pls), " %"))
