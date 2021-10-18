# 2
setwd('E:\\Study\\5_course\\MSNLabs\\Lab3\\tasks')
autos = read.csv('Auto.csv', header = T, na.string = '?')
autos = na.omit(autos)
# fix(autos)

# 2.1
attach(autos)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
autos = data.frame(autos, mpg01)

# 2.2
print(round(cor(autos[, -9]), 2))
pairs(autos[, -9])

boxplot(cylinders ~ mpg01, data = autos, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = autos, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = autos, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = autos, main = "Weight vs mpg01")

# 2.3
train = (year >= min(year) &
 year <= min(year) + (max(year) - min(year)) %/% 2)

autos.train = autos[train, ]
autos.test = autos[!train, ]
mpg01.test = mpg01[!train]

# print(train)

# 2.4
cat("\n")
library(MASS)
fit.lda = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = autos, subset = train)
print(fit.lda)

cat("\n")
pred.lda = predict(fit.lda, autos.test)
print(table(pred.lda$class, mpg01.test))
print(paste("Test error rate: ",
 mean(pred.lda$class != mpg01.test)))

# 2.5
cat("\n")
fit.qda = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = autos, subset = train)
print(fit.qda)

cat("\n")
pred.qda = predict(fit.qda, autos.test)
print(table(pred.qda$class, mpg01.test))
print(paste("Test error rate: ",
 mean(pred.qda$class != mpg01.test)))

# 2.6
fit.glm = glm(mpg01 ~ cylinders + weight + displacement + horsepower,
 data = autos, family = binomial, subset = train)
print(summary(fit.glm))

probs = predict(fit.glm, autos.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] = 1
print(table(pred.glm, mpg01.test))
print(paste("Test error rate: ",
 mean(pred.glm != mpg01.test)))

# 2.7
cat("\n")
library(class)

train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[!train, ]
mpg01.train = mpg01[train]
set.seed(1)

pred.knn = knn(train.X, test.X, mpg01.train, k = 1)
print(table(pred.knn, mpg01.test))
print(paste("Test error rate: ",
 mean(pred.knn != mpg01.test)))

pred.knn2 = knn(train.X, test.X, mpg01.train, k = 5)
print(table(pred.knn2, mpg01.test))
print(paste("Test error rate: ",
 mean(pred.knn2 != mpg01.test)))

pred.knn3 = knn(train.X, test.X, mpg01.train, k = 15)
print(table(pred.knn3, mpg01.test))
print(paste("Test error rate: ",
 mean(pred.knn3 != mpg01.test)))
