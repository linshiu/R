##### STAT 428 - LAB 1
##### Luis Steven Lin
##### llinsh2

### Problem 1

# Part a)

grades = matrix( c(20,16,25,14,23,15,20,19,18,18,22,18,48,40,40,42), ncol=4)
colnames(grades) = c("Test1","Test2","Test3","Final")
grades

# Part b)

grades = rbind(grades,c(10, 15, 14, 30))
grades

# Part c)

grades[2,2] = 17
grades

# Part d)
grades[,"Test3"]

# Part e)
grades[,"Final"][grades[,"Test1"]>16]

# Part f)

grades[,-3]

# Part g)

nrow(grades)

### Problem 2

quiz=c(24,22,17,10,12,13,16,19,15,18,22,21)
diff(quiz)

### Problem 3

A = matrix(c(3,5,6,2,-3,3,6,4,-2),ncol=3)
b = c(44,18,14)

solve(A,b)

### Problem 4

size.2 = matrix(c(130,26,110,24,118,25,112,25),ncol=2,byrow=T)
heights = c(140,155,142,175)
size = cbind(size.2,heights)
size = rbind(size,c(128,26,170))

apply(size,2,sum)

### Problem 5

geoMean = function(x){
	n = length(x)
	product = prod(x)
	gMean = product^(1/n)
	return(gMean)
	}

x = c(2,6,9,17,39)
geoMean(x)
