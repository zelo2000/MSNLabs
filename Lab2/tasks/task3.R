#3
library(ISLR)
fix(Carseats)

#3.1
lm.fit = lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)