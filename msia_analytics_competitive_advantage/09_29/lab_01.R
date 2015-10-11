
#Numerical vector

MyNumVector <- c(1,2,3)

#String vector
MyStrVector <- c("MS", "Analytics", "Northwestern")

#Sequence of numbers
MyVec <- seq(1,10)
MyVec2 <- seq(5,1)
MyVec3 <- seq(25,30)

#Vector of zeros or ones
MyZeroVec <- rep(0,10)
MyOneVec <- rep(1,20)

#Length of vector
length(MyVec)

# Creating matrix
MyMat1 <- matrix(c(1,2,3,4,5,6), nrow=3)
MyMat2 <- matrix(c(1,2,3,4,5,6), nrow=3, byrow=T)
MyMat3 <- matrix(1:8, nrow=4)

# Dimension of matrix
dim(MyMat1)
dim(MyMat3)

#Referencing elements of vector
MyVec[3]
MyVec[5:7]

#Assigning values to vector
MyVec[5] <- 50
MyVec[9:10] <- 1000

#Referencing elements of matrix
MyMat1[1,2]
MyMat1[1:2,1:2]

#Assigning values to matrix
MyMat1[1,2] = 50
MyMat1[1:2,1:2] = 1000

X <- c(1,3,5,7,9); Y <- c(10, 20,30,40,50); A <- 2; B <- 4;

# Summation
C = A+B
sum(X)
Z = X+Y

# Partial Summation
sum(X[3:5])

# Average and Standard Deviation
mean(X)
sd(X)

# Sqrated Root
sqrt(C)
sqrt(sum(Z))

# Ex 1
Set1 <- seq(1,10);
Set2 <- seq(101,110); 
SetAvg <- c(mean(Set1),mean(Set2)); 
SetAvg;
print(SetAvg)

# Reading from text

# Change working directory

# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015_NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/09_29"
setwd(file.path(main,course, datafolder))

tb1 <- read.table("ex_header.txt", header=T)
tb2 <- read.table("ex_no_header.txt", header=F)
tb3 <- read.table("ex_header.txt", header=F)
tb4 <- read.table("ex_no_header.txt", header=T)

tb1
tb2
tb3
tb4

# Write data

write.table(tb1,"ex_write1.txt", sep="\t")
write.table(tb1,"ex_write2.txt", sep="\t", row.names=F)
write.table(tb1,"ex_write3.txt", sep="\t", col.names=F)

# functions and looping

MyFunc <- function(x){
  variance <- sd(x)^2;
  answer <- variance + sum(x);
  answer
}

MyFunc(tb1$gpa)

# If statement
number = 2
if (number != 1){
  result = F;}
else{
  result = T;
}
result

x <- -5
if(x > 0){
  print("Non-negative number")
} else {
  print("Negative number")
}

# For statement
x = 1:10
sum = 0
for(i in 1:length(x)){
  sum = sum + x[i]
}

# While

i <- 1
while(i <= length(x)){
  sum <- sum + x[i]
  i <- i+1
}

# Exercise 2

myAvg <- function(mytb){ 
  sum = 0; cnt = 0; 
  for(i in 1:length(mytb[,1])){
    if(mytb[i,1]>21){
      sum = sum + mytb[i,2]; 
      cnt = cnt + 1; 
    } 
  }
  avg = sum / cnt; print(avg); 
}

myAvg(tb1)
mean(tb1$gpa[tb1$age>21])

# Simple plots
plot(tb1[,"age"],tb1[,"gpa"])
barplot(tb1[,"age"])

# Exercise 3
myfunc3 <- function(tb){ 
  myvec <- c(0,0); 
  myvec[1] = mean(tb[,1]); 
  myvec[2] = mean(tb[,2]); 
  print(myvec); 
  plot(tb[,1],tb[,2]); }

tb5 <- read.table("height.txt", header=T)

myfunc3(tb5)

# Variance
var(tb5[,1])

# Correlation coefficient
cor(tb5[,1],tb5[,2])
cor(tb5)

# Linear regression
myReg <- lm(tb5[,2] ~ tb5[,1]);

summary(myReg)
plot(tb5[,1],tb5[,2]);
abline(coef(myReg));
