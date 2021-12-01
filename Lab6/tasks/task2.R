# 2
library(ISLR)
set.seed(1)

cat("\n")
print(summary(Wage$maritl))
print(summary(Wage$jobclass))

par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage,  xlab='maritl', ylab='wage')
plot(Wage$jobclass, Wage$wage,  xlab='jobclass', ylab='wage')

library(gam)
fit0 = gam(wage ~ lo(year, span = 0.6) + 
s(age, df = 2) + education, data = Wage)
fit1 = gam(wage ~ lo(year, span = 0.6) + 
s(age, df = 2) + education + jobclass, data = Wage)
fit2 = gam(wage ~ lo(year, span = 0.6) + 
s(age, df = 2) + education + maritl, data = Wage)
fit3 = gam(wage ~ lo(year, span = 0.6) + 
s(age, df = 2) + education + jobclass + maritl, data = Wage)
print(anova(fit0, fit1, fit2, fit3))

par(mfrow = c(2, 2))
plot(fit2, se = T, col = "blue")
