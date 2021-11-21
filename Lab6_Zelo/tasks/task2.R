# 2
library(ISLR)
set.seed(1)

summary(Wage$maritl)
summary(Wage$jobclass)

par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage,  xlab='maritl', ylab='wage')
plot(Wage$jobclass, Wage$wage,  xlab='jobclass', ylab='wage')

library(gam)
fit0 = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
fit1 = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
fit2 = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
fit3 = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass + maritl, data = Wage)
anova(fit0, fit1, fit2, fit3)

par(mfrow = c(3, 2))
plot(fit3, se = T, col = "blue")