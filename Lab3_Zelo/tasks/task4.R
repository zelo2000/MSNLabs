# 4
library(MASS)
print(summary(Boston))
cat("\n")

attach(Boston)
crim01 = rep(0, length(crim))
crim01[crim > median(crim)] = 1
Boston = data.frame(Boston, crim01)

train = 1:(length(crim) / 2)
test = (length(crim) / 2 + 1):length(crim)

Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crim01.test = crim01[test]

fit.glm = glm(crim01 ~. - crim01 - crim, data = Boston, family = binomial, subset = train)
probs = predict(fit.glm, Boston.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] = 1
table(pred.glm, crim01.test)
paste("Коефіцієнт помилок: ", mean(pred.glm != crim01.test))

cat("\n")

fit.lda = lda(crim01 ~. - crim01 - crim, data = Boston, subset = train)
pred.lda = predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)
paste("Коефіцієнт помилок: ", mean(pred.lda$class != crim01.test))

cat("\n")

fit.qda = qda(crim01 ~. - crim01 - crim, data = Boston, subset = train)
pred.qda = predict(fit.qda, Boston.test)
table(pred.qda$class, crim01.test)
paste("Коефіцієнт помилок: ", mean(pred.qda$class != crim01.test))

cat("\n")

library(class)
train.X = cbind(indus, chas, nox, rm, age, dis,
 tax, ptratio, black, lstat, medv)[train, ]
test.X = cbind(indus, chas, nox, rm, age, dis,
 tax, ptratio, black, lstat, medv)[test, ]
crim01.train = crim01[train]
set.seed(1)

pred.knn = knn(train.X, test.X, crim01.train, k = 1)
table(pred.knn, crim01.test)
paste("Коефіцієнт помилок: ", mean(pred.knn != crim01.test))

pred.knn2 = knn(train.X, test.X, crim01.train, k = 2)
table(pred.knn2, crim01.test)
paste("Коефіцієнт помилок: ", mean(pred.knn2 != crim01.test))

pred.knn3 = knn(train.X, test.X, crim01.train, k = 4)
table(pred.knn3, crim01.test)
paste("Коефіцієнт помилок: ", mean(pred.knn3 != crim01.test))

pred.knn4 = knn(train.X, test.X, crim01.train, k = 8)
table(pred.knn4, crim01.test)
paste("Коефіцієнт помилок: ", mean(pred.knn4 != crim01.test))
