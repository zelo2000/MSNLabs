# 2.
setwd('E:\\Study\\5_course\\MSNLabs\\Lab1\\task')
autos = read.csv('Auto.csv', header = T, na.string = '?')
autos = na.omit(autos) 
fix(autos)

cat("\n")

# 2.1
qualitative = c(2, 7, 8, 9)
for (val in qualitative) {
  autos[, val] = as.factor(autos[, val])
}
print(summary(autos[]))

cat("\n")

# 2.2
quantitative = c(1, 3, 4, 5, 6)
for (val in quantitative) {
  print(range(autos[, val]))
}

cat("\n")

#2.3
for (val in quantitative) {
  print(paste("Mean", mean(autos[, val]),
   "- Sd", sd(autos[, val])))
}

cat("\n")

#2.4
autos_clipped = autos[-c(10:84),]
fix(autos_clipped)
for (val in quantitative) {
  print(paste("Mean", mean(autos_clipped[, val]),
   "- Sd", sd(autos_clipped[, val])))
}

cat("\n")

for (val in quantitative) {
  mean_diff = mean(autos_clipped[, val])-mean(autos[, val])
  sd_diff = sd(autos_clipped[, val])-sd(autos[, val]) 
  print(paste("Mean diff", mean_diff,
   "- Sd diff", sd_diff))
}

#2.5
pairs(autos)

#2.6
pairs(autos, verInd=1)
