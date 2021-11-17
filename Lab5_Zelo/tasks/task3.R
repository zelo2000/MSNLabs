# 3.1
set.seed(1)
x = matrix(rnorm(1000 * 20), 1000, 20)
eps = rnorm(1000)

betas = runif(20, min=-2.72, max=3.14)

zero_vals_num = sample(3:10, 1)
for (i in sample(1:length(betas), zero_vals_num)) {
  betas[i] = 0
}
betas

y = x %*% betas + eps

# 3.2
train = sample(1:length(eps), 100)

x.train = x[train, ]
y.train = y[train]

x.test = x[-train, ]
y.test = y[-train]

# 3.3
cat("\n")
library(leaps)

reg.fit = regsubsets(y ~ ., 
                     data = data.frame(y = y.train, x = x.train),
                     nvmax = 20)

BestSubsetSelection = function(x, y, ylab) {
    localData = data.frame(y = y, x = x)
    mat = model.matrix(y ~ ., data = localData, nvmax = 20)

    val.errors = rep(0, 20)
    for (i in 1:20) {
        coef_i = coef(reg.fit, id = i)
        pred = mat[, names(coef_i)] %*% coef_i
        val.errors[i] = mean((pred - y)^2)
    }

    plot(val.errors, xlab = "Predictors amount", ylab = ylab, pch = 19, type = "b")

    return(val.errors)
}

BestSubsetSelection(x.train, y.train, "Training MSE")

# 3.4
test_mse = BestSubsetSelection(x.test, y.test, "Test MSE")

# 3.5
paste('Model size for min MSE: ', which.min(test_mse), ', min MSE: ', min(test_mse))

# 3.6
cat("\n")
coef(reg.fit, which.min(test_mse))

# 3.7
val.errors = rep(0, 20)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")

for (i in 1:20) {
    coef_i = coef(reg.fit, id = i)
    val.errors[i] = 
    sqrt(sum((betas[x_cols %in% names(coef_i)] -
         coef_i[names(coef_i) %in% x_cols])^2))
}

par(mfrow = c(1, 2))

BestSubsetSelection(x.test, y.test, "Test MSE")

plot(val.errors, xlab = "Predictors amount",
 ylab = "Error between estimated and real coefficients", pch = 19, type = "b")
