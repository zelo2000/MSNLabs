# 4
library(MASS)
print(summary(Boston))
cat("\n")

attach(Boston)
crim01 = rep(0, length(crim))
crim01[crim > median(crim)] = 1
Boston = data.frame(Boston, crim01)

train = (crim %in% crim[1:(length(crim) %/% 2)])

Boston.train = Boston[train, ]
Boston.test = Boston[!train, ]
crim01.test = crim01[!train]

fit.glm = glm(crim01 ~. - crim01 - crim - zn - rad,
 data = Boston, family = binomial, subset = train)
probs = predict(fit.glm, Boston.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] = 1
print(table(pred.glm, crim01.test))
print(paste("Test error rate: ",
 mean(pred.glm != crim01.test)))

cat("\n")

fit.lda = lda(crim01 ~. - crim01 - crim - zn - rad, data = Boston, subset = train)
pred.lda = predict(fit.lda, Boston.test)
print(table(pred.lda$class, crim01.test))
print(paste("Test error rate: ",
 mean(pred.lda$class != crim01.test)))

cat("\n")

fit.qda = qda(crim01 ~. - crim01 - crim - zn - rad, data = Boston, subset = train)
pred.qda = predict(fit.qda, Boston.test)
print(table(pred.qda$class, crim01.test))
print(paste("Test error rate: ",
 mean(pred.qda$class != crim01.test)))

cat("\n")

library(class)
train.X = cbind(indus, chas, nox, rm, age, dis,
 tax, ptratio, black, lstat, medv)[train, ]
test.X = cbind(indus, chas, nox, rm, age, dis,
 tax, ptratio, black, lstat, medv)[!train, ]
crim01.train = crim01[train]
set.seed(1)

pred.knn = knn(train.X, test.X, crim01.train, k = 1)
print(table(pred.knn, crim01.test))
print(paste("Test error rate: ",
 mean(pred.knn != crim01.test)))

pred.knn2 = knn(train.X, test.X, crim01.train, k = 5)
print(table(pred.knn2, crim01.test))
print(paste("Test error rate: ",
 mean(pred.knn2 != crim01.test)))

pred.knn3 = knn(train.X, test.X, crim01.train, k = 15)
print(table(pred.knn3, crim01.test))
print(paste("Test error rate: ",
 mean(pred.knn3 != crim01.test)))

pred.knn4 = knn(train.X, test.X, crim01.train, k = 100)
print(table(pred.knn4, crim01.test))
print(paste("Test error rate: ",
 mean(pred.knn4 != crim01.test)))
