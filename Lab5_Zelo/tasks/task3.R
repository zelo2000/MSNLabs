# 3.1
set.seed(1)
x = matrix(rnorm(1000 * 20), 1000, 20)
eps = rnorm(1000)

cat("\n")
betas = runif(20, min=-5, max=5)
print("Beta-values list:")

zero_vals_num = sample(3:10, 1)
for (i in sample(1:length(betas), zero_vals_num)) {
  betas[i] = 0
}
print(betas)

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

BestSubsetSelection = function(x_, y_, ylab_) {
    data_ = data.frame(y = y_, x = x_)
    mat = model.matrix(y ~ ., data = data_, nvmax = 20)

    val.errors = rep(0, 20)
    for (i in 1:20) {
        coef_i = coef(reg.fit, id = i)
        pred = mat[, names(coef_i)] %*% coef_i
        val.errors[i] = mean((pred - y_)^2)
    }

    plot(val.errors, xlab = "Кiлькiсть предикторiв", ylab = ylab_,
    col = "red", pch = 19, type = "b")

    return(val.errors)
}

BestSubsetSelection(x.train, y.train, "Тренувальний MSE")

# 3.4
test_mse = BestSubsetSelection(x.test, y.test, "Тестовий MSE")

# 3.5
print(paste('Model size for min MSE: ', which.min(test_mse),
 ', min MSE: ', min(test_mse)))

# 3.6
cat("\n")
print(coef(reg.fit, which.min(test_mse)))

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

BestSubsetSelection(x.test, y.test, "Тестовий MSE")

plot(val.errors, xlab = "Кiлькiсть предикторiв",
 ylab = "Помилка між оціночними та реальними значеннями коефіцієнтів",
 col = "red", pch = 19, type = "b")
