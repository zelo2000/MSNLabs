# 1.1
setwd('D:\\LNU\\MSN\\Lab1\\task')
college = read.csv('College.csv', header = T, na.string = '?')
# print(college)

# 1.2
fix(college)

rownames(college) = college[, 1]
fix(college)

college = college[, -1]
fix(college)

# 1.3.1
Summary = summary(college)
print(Summary)