# 1
# install.packages('ISLR')
library(ISLR)
fix(Weekly)
?Weekly
summary(Weekly)

# 1.1
print(cor(Weekly[, -9]))

attach(Weekly)
plot(Volume)

# 1.2
fit.glm = glm(
	Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
	data = Weekly,
	family = binomial
)
summary(fit.glm)

# 1.3
probs = predict(fit.glm, type = "response")
contrasts(Direction)

pred.glm = rep("Down", length(probs))
pred.glm[probs > 0.5] = "Up"
table(pred.glm, Direction)

paste("?anoea i?aaeeuieo i?iaiic?a: ", mean(pred.glm == Direction))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie ?inoa: ",
 sum(pred.glm == Direction & Direction == "Up") / sum(Direction == "Up"))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie iaaa?: ",
 sum(pred.glm == Direction & Direction == "Down") / sum(Direction == "Down"))

# 1.4
train = (Year < 2009)
Weekly.test = Weekly[!train, ]
Direction.test = Direction[!train]

cat("\n")
paste("E?eue?nou ?yae?a a oanoia?e aea??o?: ", dim(Weekly.test)[1])
paste("E?eue?nou ?yae?a a o?aioaaeui?e aea??o?: ", dim(Weekly[train, ])[1])

fit.glm2 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit.glm2)

probs2 = predict(fit.glm2, Weekly.test, type = "response")
pred.glm2 =  rep("Down", length(probs2))
pred.glm2[probs2 > 0.5] = "Up"
table(pred.glm2, Direction.test)

paste("?anoea i?aaeeuieo i?iaiic?a: ", mean(pred.glm2 == Direction.test))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie ?inoa: ",
 sum(pred.glm2 == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up"))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie iaaa?: ",
 sum(pred.glm2 == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down"))

# 1.5
cat("\n")
library(MASS)
fit.lda = lda(Direction ~ Lag2, data = Weekly, subset = train)
print(fit.lda)

cat("\n")
pred.lda = predict(fit.lda, Weekly.test)
table(pred.lda$class, Direction.test)

paste("?anoea i?aaeeuieo i?iaiic?a: ", mean(pred.lda$class == Direction.test))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie ?inoa: ",
 sum(pred.lda$class == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up"))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie iaaa?: ",
 sum(pred.lda$class == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down"))

# 1.6
cat("\n")
fit.qda = qda(Direction ~ Lag2, data = Weekly, subset = train)
print(fit.qda)

cat("\n")
pred.qda = predict(fit.qda, Weekly.test)
table(pred.qda$class, Direction.test)

paste("?anoea i?aaeeuieo i?iaiic?a: ", mean(pred.qda$class == Direction.test))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie ?inoa: ",
 sum(pred.qda$class == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up"))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie iaaa?: ",
 sum(pred.qda$class == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down"))

# 1.7
cat("\n")
library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])

Direction.train = Direction[train]
set.seed(1)
pred.knn = knn(train.X, test.X, Direction.train, k = 1)
table(pred.knn, Direction.test)

paste("?anoea i?aaeeuieo i?iaiic?a: ", mean(pred.knn == Direction.test))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie ?inoa: ",
 sum(pred.knn == Direction.test & Direction.test == "Up") / sum(Direction.test == "Up"))

paste("?anoea i?aaeeuieo i?iaiic?a eiee ?eiie iaaa?: ",
 sum(pred.knn == Direction.test & Direction.test == "Down") / sum(Direction.test == "Down"))

# 1.8
cat("\n")
error_rate.knn = 1 - mean(pred.knn == Direction.test)
error_rate.qda = 1 - mean(pred.qda$class == Direction.test)
error_rate.lda = 1 - mean(pred.lda$class == Direction.test)
error_rate.glm2 = 1 - mean(pred.glm2 == Direction.test)

paste("Eiao?o??io iiieeie aey eeaneo?eaoi?a K-iaeaee??eo non?a?a c K=1", ": ", error_rate.knn)
paste("Eiao?o??io iiieeie aey eaaa?aoe?iiai aene?ei?iaioiiai aiae?co", ": ", error_rate.qda)
paste("Eiao?o??io iiieeie aey e?i?eiiai aene?ei?iaioiiai aiae?co", ": ", error_rate.lda)
paste( "Eiao?o??io iiieeie aey eia?noe?ii? ?aa?an??", ": ", error_rate.glm2)

# 1.9
cat("\n")
fit.glm3 =  glm(Direction ~ Lag1:Lag2, data = Weekly, family = binomial, subset = train)
probs3 = predict(fit.glm3, Weekly.test, type = "response")
pred.glm3 =  rep("Down", length(probs2))
pred.glm3[probs3 > 0.5] = "Up"
table(pred.glm3, Direction.test)
paste("Частка правильних прогнозів: ", mean(pred.glm3 == Direction.test))

cat("\n")
fit.lda2 = lda(Direction ~ Lag1*Lag2, data = Weekly, subset = train)
pred.lda2 = predict(fit.lda2, Weekly.test)
table(pred.lda2$class, Direction.test)
paste("Частка правильних прогнозів: ", mean(pred.lda2$class == Direction.test))

cat("\n")

fit.qda2 = qda(Direction ~ Lag1 + Lag2 + sqrt(abs(Lag1)), data = Weekly, subset = train)
pred.qda2 = predict(fit.qda2, Weekly.test)
table(pred.qda2$class, Direction.test)
paste("Частка правильних прогнозів: ", mean(pred.qda2$class == Direction.test))

cat("\n")
pred.knn2 = knn(train.X, test.X, Direction.train, k = 1)
table(pred.knn2, Direction.test)
paste("Частка правильних прогнозів: ", mean(pred.knn2 == Direction.test))

pred.knn3 = knn(train.X, test.X, Direction.train, k = 2)
table(pred.knn3, Direction.test)
paste("Частка правильних прогнозів: ", mean(pred.knn3 == Direction.test))

pred.knn4 = knn(train.X, test.X, Direction.train, k = 4)
table(pred.knn4, Direction.test)
paste("Частка правильних прогнозів: ", mean(pred.knn4 == Direction.test))

pred.knn5 = knn(train.X, test.X, Direction.train, k = 8)
table(pred.knn5, Direction.test)
paste("Частка правильних прогнозів: ", mean(pred.knn5 == Direction.test))
