# 8
library(MASS)
print(summary(Boston))

# 8.1
attach(Boston)

lm.zn = lm(crim~zn)
print(summary(lm.zn))

lm.indus = lm(crim~indus)
print(summary(lm.indus))

lm.chas = lm(crim~chas) 
print(summary(lm.chas))

lm.nox = lm(crim~nox)
print(summary(lm.nox))

lm.rm = lm(crim~rm)
print(summary(lm.rm))

lm.age = lm(crim~age)
print(summary(lm.age))

lm.dis = lm(crim~dis)
print(summary(lm.dis))

lm.rad = lm(crim~rad)
print(summary(lm.rad))

lm.tax = lm(crim~tax)
print(summary(lm.tax))

lm.ptratio = lm(crim~ptratio)
print(summary(lm.ptratio))

lm.black = lm(crim~black)
print(summary(lm.black))

lm.lstat = lm(crim~lstat)
print(summary(lm.lstat))

lm.medv = lm(crim~medv)
print(summary(lm.medv))

par(mfrow=c(2,2))
plot(lm.chas)
plot(lm.age)

# 8.2
lm.all = lm(crim~., data=Boston)
print(summary(lm.all))

# 8.3
all_coeffs = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])

print(all_coeffs)
cat("\n")
print(coefficients(lm.all)[2:14])

# 8.4
lm.zn = lm(crim~poly(zn, 3))
print(summary(lm.zn))

lm.indus = lm(crim~poly(indus, 3))
print(summary(lm.indus))

# lm.chas = lm(crim~poly(chas, 3)) 
# print(summary(lm.chas))

lm.nox = lm(crim~poly(nox, 3))
print(summary(lm.nox))

lm.rm = lm(crim~poly(rm, 3))
print(summary(lm.rm))

lm.age = lm(crim~poly(age, 3))
print(summary(lm.age))

lm.dis = lm(crim~poly(dis, 3))
print(summary(lm.dis))

lm.rad = lm(crim~poly(rad, 3))
print(summary(lm.rad))

lm.tax = lm(crim~poly(tax, 3))
print(summary(lm.tax))

lm.ptratio = lm(crim~poly(ptratio, 3))
print(summary(lm.ptratio))

lm.black = lm(crim~poly(black, 3))
print(summary(lm.black))

lm.lstat = lm(crim~poly(lstat, 3))
print(summary(lm.lstat))

lm.medv = lm(crim~poly(medv, 3))
print(summary(lm.medv))