# 1
# install.packages('ISLR')
library(ISLR)
print(summary(Weekly))

# 1.1
print(cor(Weekly[, -9]))

attach(Weekly)
plot(Volume)

# 1.2
fit.glm = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
 data = Weekly, family = binomial)
print(summary(fit.glm))

# 1.3
probs = predict(fit.glm, type = "response")
print(contrasts(Direction))

pred.glm = rep("Down", length(probs))
pred.glm[probs > 0.5] = "Up"
print(table(pred.glm, Direction))
print(paste("Correct predictions rate: ",
 mean(pred.glm == Direction)))
print(paste("Correct predictions rate when the market goes up: ",
 sum(pred.glm == Direction & Direction == "Up") / sum(Direction == "Up")))
print(paste("Correct predictions rate when the market goes down: ",
 sum(pred.glm == Direction & Direction == "Down") / sum(Direction == "Down")))

# 1.4
train = (Year >= 1990 & Year <= 2008)
Weekly.test = Weekly[!train, ]
Direction.test = Direction[!train]

cat("\n")
print(paste("Rows of train data: ", dim(Weekly.test)[1]))

fit.glm2 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
print(summary(fit.glm2))

probs2 = predict(fit.glm2, Weekly.test, type = "response")
pred.glm2 =  rep("Down", length(probs2))
pred.glm2[probs2 > 0.5] = "Up"
print(table(pred.glm2, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.glm2 == Direction.test)))
print(paste("Correct predictions rate when the market goes up: ",
 sum(pred.glm2 == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up")))
print(paste("Correct predictions rate when the market goes down: ",
 sum(pred.glm2 == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down")))

# 1.5
cat("\n")
library(MASS)
fit.lda = lda(Direction ~ Lag2, data = Weekly, subset = train)
print(fit.lda)

cat("\n")
pred.lda = predict(fit.lda, Weekly.test)
print(table(pred.lda$class, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.lda$class == Direction.test)))
print(paste("Correct predictions rate when the market goes up: ",
 sum(pred.lda$class == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up")))
print(paste("Correct predictions rate when the market goes down: ",
 sum(pred.lda$class == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down")))

# 1.6
cat("\n")
fit.qda = qda(Direction ~ Lag2, data = Weekly, subset = train)
print(fit.qda)

cat("\n")
pred.qda = predict(fit.qda, Weekly.test)
print(table(pred.qda$class, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.qda$class == Direction.test)))
print(paste("Correct predictions rate when the market goes up: ",
 sum(pred.qda$class == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up")))
print(paste("Correct predictions rate when the market goes down: ",
 sum(pred.qda$class == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down")))

# 1.7
cat("\n")
library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])

Direction.train = Direction[train]
set.seed(1)
pred.knn = knn(train.X, test.X, Direction.train, k = 1)
print(table(pred.knn, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.knn == Direction.test)))
print(paste("Correct predictions rate when the market goes up: ",
 sum(pred.knn == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up")))
print(paste("Correct predictions rate when the market goes down: ",
 sum(pred.knn == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down")))

# 1.8
cat("\n")
error_rate.knn = 1 - mean(pred.knn == Direction.test)
error_rate.qda = 1 - mean(pred.qda$class == Direction.test)
error_rate.lda = 1 - mean(pred.lda$class == Direction.test)
error_rate.glm2 = 1 - mean(pred.glm2 == Direction.test)

error_rate_list = c(error_rate.knn, error_rate.qda,
 error_rate.lda, error_rate.glm2)
error_name_list = c("Knn test error rate", "Qda test error rate",
 "Lda test error rate", "Glm test error rate")

print("Test error rate table")
for (idx in 1:length(error_rate_list)) {
  print(paste(error_name_list[idx], ": ", error_rate_list[idx]))
}

# 1.9
cat("\n")
fit.glm3 =  glm(Direction ~ Lag2*Lag1, data = Weekly, family = binomial, subset = train)
probs3 = predict(fit.glm3, Weekly.test, type = "response")
pred.glm3 =  rep("Down", length(probs2))
pred.glm3[probs3 > 0.5] = "Up"
print(table(pred.glm3, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.glm3 == Direction.test)))

cat("\n")
fit.lda2 = lda(Direction ~ Lag2:Lag1 + Lag1, data = Weekly, subset = train)
pred.lda2 = predict(fit.lda2, Weekly.test)
print(table(pred.lda2$class, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.lda2$class == Direction.test)))

cat("\n")

fit.qda2 = qda(Direction ~ Lag2 + abs(Lag2), data = Weekly, subset = train)
pred.qda2 = predict(fit.qda2, Weekly.test)
print(table(pred.qda2$class, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.qda2$class == Direction.test)))

cat("\n")
pred.knn2 = knn(train.X, test.X, Direction.train, k = 5)
print(table(pred.knn2, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.knn2 == Direction.test)))

pred.knn3 = knn(train.X, test.X, Direction.train, k = 15)
print(table(pred.knn3, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.knn3 == Direction.test)))

pred.knn4 = knn(train.X, test.X, Direction.train, k = 30)
print(table(pred.knn4, Direction.test))
print(paste("Correct predictions rate: ",
 mean(pred.knn4 == Direction.test)))
