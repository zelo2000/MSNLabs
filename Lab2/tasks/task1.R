# 1
setwd('D:\\LNU\\MSN\\MSNLabs\\Lab2\\tasks')
autos = read.csv('Auto.csv', header = T, na.string = '?')
autos = na.omit(autos) 
fix(autos)

# 1.1
lm.fit = lm(mpg~horsepower,data=autos)
lm.fit
summary(lm.fit)

# 1.2
plot(autos$mpg)
abline(lm.fit)

# 1.3
par(mfrow=c(2,2))
plot(lm.fit)