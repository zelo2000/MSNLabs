# 3
# 3.1
cat("\n")
Power = function() {2^3}
print(paste("raising 2 to the 3rd power: ", Power()))

# 3.2
cat("\n")
Power2 = function(x, a) {x^a}
print(paste("x: 5, a: 3 -> power: ", Power2(5, 3)))

# 3.3
cat("\n")
print(paste("x: 4, a: 4 -> power: ", Power2(4, 4)))

for (x in sample(1:25, 3)) {
  a = sample(1:10, 1)
  print(paste("x: ", x, " a:", a, " -> power: ", Power2(x, a)))
}

# 3.4
cat("\n")
Power3 = function(x, a) {
    result = x^a
    return(result)
}

power_res_3_3 = Power3(3, 3)
print(paste("x: 3, a: 3 -> power: ", power_res_3_3))

# 3.5
cat("\n")
x = c(1:10)
plot(x, Power3(x, 2), xlab = "x", ylab = "x^2", main = "x^2 vs x")
plot(x, Power3(x, 2), log = "xy",
 xlab = "Log of x", ylab = "Log of x^2", main = "Log of x^2 vs Log of x")

# 3.6
PlotPower = function(x, a) {
  plot(x, Power3(x, a), xlab = "x", ylab = "x^a", main = "x^a vs x")
}

PlotPower(1:10, 3)
