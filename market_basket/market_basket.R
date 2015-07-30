# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU"
course = "MSIA_421_Data_Mining"
datafolder = "Association Rules - McKinsey Exercise"
setwd(file.path(main,course, datafolder))

#opts_knit$set(root.dir = getwd())

# Import data
filename = "data.csv"
mydata = read.csv(filename,header = T)

# Look at data
names(mydata)
head(mydata)
tail(mydata)
nrow(mydata)
ncol(mydata)
summary(mydata)

#Use R package arules
install.packages("arules")
library("arules")

# Convert binary matrix to transactions matrix
# where every row contains set of items of each transaction
mydata_matrix = as.matrix(mydata[-1])
row.names(mydata_matrix) = 0:(nrow(mydata)-1)
trans =  as(mydata_matrix, "transactions")
trans

# check transaction
inspect(trans[1:10])
as(trans[1:5], "list")

summary(trans)
itemFrequency(trans)

# association rules
rules = apriori(trans, parameter = list(support = 0.01, 
                                        confidence = 0.6))
inspect(rules)
summary(rules)

# Frequent item sets
sets = apriori(trans, parameter = list(target="freq"))
inspect(sets)
summary(sets)

str(summary(sets))
summary(sets)

write(rules, file = "rules.csv", sep = ",", col.names = NA)
write(sets, file = "sets.csv", sep = "|", col.names = NA)
