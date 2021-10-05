#3
library(ISLR)
fix(Carseats)

#3.1
lm.fit = lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)

# 3.5
lm.fit2 = lm(Sales ~ Price + US, data=Carseats)
summary(lm.fit2)

# 3.7
confint(lm.fit2)

# 3.8
plot(predict(lm.fit2), rstudent(lm.fit2))

par(mfrow=c(2,2))
plot(lm.fit2)