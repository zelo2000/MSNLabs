#7.1
set.seed(1)
x1=runif(100)
x2 =0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

#7.2
cor(x1, x2)
plot(x1, x2)

#7.3
lm.fit = lm(y~x1+x2)
summary(lm.fit)

#7.4
lm.fit = lm(y~x1)
summary(lm.fit)

#7.5
lm.fit = lm(y~x2)
summary(lm.fit)

#7.7
x1 = c(x1,0.1) 
x2 = c(x2,0.8) 
y = c(y,6)
lm.fit1 = lm(y~x1+x2)
summary(lm.fit1)

lm.fit2 = lm(y~x1)
summary(lm.fit2)

lm.fit3 = lm(y~x2)
summary(lm.fit3)

par(mfrow=c(2,2))
plot(lm.fit1)

par(mfrow=c(2,2))
plot(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit3)

plot(predict(lm.fit1), rstudent(lm.fit1))

plot(predict(lm.fit2), rstudent(lm.fit2))

plot(predict(lm.fit3), rstudent(lm.fit3))

