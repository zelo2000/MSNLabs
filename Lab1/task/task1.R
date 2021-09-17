# 1.1
setwd('D:\\LNU\\MSN\\MSNLabs\\Lab1\\task')
college = read.csv('College.csv', header = T, na.string = '?')
print(college)

# 1.2
fix(college)

rownames(college) = college[, 1]
fix(college)

college = college[, -1]
fix(college)

# 1.3.1
summary(college)

#1.3.2
college$Private = as.factor(college$Private)
pairs(college[1:10])

#1.3.3
plot(college$Private, college$Outstate)

#1.3.4
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = " Yes"
Elite = as.factor(Elite)
college = data.frame(college ,Elite)
summary(college)
plot(college$Elit, college$Outstate)

#1.3.5
par(mfrow=c(2,2))
hist(college$Expend)
hist(college$Outstate)
hist(college$PhD)
hist(college$Terminal)
