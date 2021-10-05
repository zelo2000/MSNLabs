#7.1
set.seed(1)
x1=runif(100)
x2 =0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

lm.fit = lm(y~x1+x2)
summary(lm.fit)

#7.2
cor(x1, x2)
plot(x1, x2)

