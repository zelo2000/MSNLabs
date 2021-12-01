# 5
library(ISLR)
set.seed(1)
attach(College)
cat("\n")
print(summary(College))

# 5.1
library(leaps)

train = sample(length(Outstate), length(Outstate) / 2)

College.train = College[train, ]
College.test = College[-train, ]

fit = regsubsets(Outstate ~ ., data = College.train, 
	nvmax = 14, method = "forward")
fit.summary = summary(fit)

par(mfrow = c(1, 3))
plot(fit.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
points(which.min(fit.summary$cp), fit.summary$cp[which.min(fit.summary$cp)],
col = "blue", cex = 2, pch = 20)

plot(fit.summary$bic, xlab = "Number of variables", ylab = "BIC", type='l')
points(which.min(fit.summary$bic), fit.summary$bic[which.min(fit.summary$bic)],
col = "blue", cex = 2, pch = 20)

plot(fit.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R2",
 type = "l", ylim = c(0.4, 0.84))
points(which.max(fit.summary$adjr2), fit.summary$adjr2[which.max(fit.summary$adjr2)],
col = "blue", cex = 2, pch = 20)

fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coeffs = coef(fit, id = 6)
cat("\n")
print(names(coeffs))

# 5.2
library(gam)
fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + 
s(PhD, df = 2) + s(perc.alumni, df = 2) +
 s(Expend, df = 2) +
  s(Grad.Rate, df = 2),
	data=College.train)
par(mfrow = c(2, 3))
plot(fit, se = T, col = "blue")

# 5.3
preds = predict(fit, College.test)

err = mean((College.test$Outstate - preds)^2)
print(paste("Test error :", round(err, 2)))

tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
rss = 1 - err / tss
print(paste("RSS :", round(rss, 2)))

# 5.4
print(summary(fit))
