# 3
# 3.1
cat("\n")
Power = function() {2^3}
paste("2 ^ 3 =", Power())

# 3.2
cat("\n")
Power2 = function(x, a) {x^a}

# 3.3
cat("\n")
paste("2 ^ 2 =", Power2(2, 2))
paste("7 ^ 3 =", Power2(7, 3))
paste("10 ^ 5 =", Power2(10, 5))

# 3.4
Power3 = function(x, a) {
    result = x^a
    return(result)
}

power_2_3 = Power3(2, 3)
paste("2 ^ 3 =", power_2_3)

# 3.5
cat("\n")
x = c(1:10)
par(mfrow=c(2,2))
plot(x, Power3(x, 2), xlab = "x", ylab = "x^2", main = "x^2 / x")

plot(x, Power3(x, 2), log = "x",
 xlab = "Log of x", ylab = "x^2", main = "x^2 / Log of x")

plot(x, Power3(x, 2), log = "y",
 xlab = "x", ylab = "Log of x^2", main = "Log of x^2 / x")

plot(x, Power3(x, 2), log = "xy",
 xlab = "Log of x", ylab = "Log of x^2", main = "Log of x^2 / Log of x")

# 3.6
par(mfrow=c(1,1))
PlotPower = function(x, a) {
  plot(x, Power3(x, a), xlab = "x", ylab = "x^a", main = "x^a / x")
}

PlotPower(-10:10, 5)
