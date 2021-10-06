# 2
setwd('E:\\Study\\5_course\\MSNLabs\\Lab2\\tasks')
autos = read.csv('Auto.csv', header = T, na.string = '?')
autos = na.omit(autos) 
fix(autos)

# 2.1
# qualitative = c(2, 7, 8, 9)
# for (val in qualitative) {
#   autos[, val] = as.factor(autos[, val])
# }

# pairs(autos)

cat("\n")

# 2.2
quantitative = c(1, 3, 4, 5, 6)
print(round(cor(autos[, quantitative]), 2))

# 2.3
lm.fit = lm(mpg~.-name, data=autos)
print(summary(lm.fit))

# 2.4
par(mfrow=c(2,2))
plot(lm.fit)

# 2.5
lm.fit2 = lm(mpg~displacement*origin+acceleration*horsepower, data=autos)
print(summary(lm.fit2))

cat("\n")

# 2.6
lm.fit3 = lm(mpg~horsepower, data=autos)
lm.fit4 = lm(mpg~horsepower + I(log(horsepower)), data=autos)
print(anova(lm.fit3, lm.fit4))

cat("\n")

lm.fit5 = lm(mpg~horsepower + I(horsepower^2), data=autos)
print(anova(lm.fit3, lm.fit5))

cat("\n")

lm.fit6 = lm(mpg~horsepower + I(sqrt(horsepower)), data=autos)
print(anova(lm.fit3, lm.fit6))