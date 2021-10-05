# 5.2
set.seed(1)
x=1:100
y=2*x+rnorm(100)
sum(x^2)
sum(y^2)

val = lm(y~x+0)
val$coefficients

val = lm(x~y+0)
val$coefficients

#5.3
x = 1:100 + rnorm(100)
y = x
sum(x^2)
sum(y^2)

val = lm(y~x+0)
val$coefficients

val = lm(x~y+0)
val$coefficients