# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015_NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/11_17"
setwd(file.path(main,course, datafolder))
filename = 'web.txt'

web = read.table("web.txt", header=T)
P = as.matrix(web)

# Calculate prob distribution after 3 steps given initial vector a =(1,0,0,0,0,0,0,0)
a = c(1,0,0,0,0,0,0,0)
a %*% P %*% P %*% P
P

# install.packages("expm")
library(expm)
a = c(1,0,0,0,0,0,0,0)
a %*% (P %^%3)

# Calculation of pi using R
# hint1 diag(n) creates identity matrix with size n
# hint2 solve(M) returns inverse of matrix M
# Let Q be the matrix obtained by replacing the last row of (P^t - I)

Q = t(P)- diag(8)
Q[8,] = c(1,1,1,1,1,1,1,1)
rhs = c(0,0,0,0,0,0,0,1)
Pi = solve(Q) %*% rhs
Pi

# Mean first passage time m ij is the expected number of transitions before
# we first reach state j, given we are currently in i

# Mean first passage time to state Start
# B: submatrix of P obtained by deleting the row and column corresponding to state Sart
# m: vector of mij, i diff Start, j = start
# e: vector of 1's 
# m = e + Bm
# m = (I-B)^-1e
B =P[2:8,2:8]
Q = diag(7) - B
e = c(1,1,1,1,1,1,1)
m = solve(Q) %*% e
m

# m is a vector containing the mean first passage time to Start from i=A,B,..
# Hence, the output right contains the mean first passage time to Start from all other pages

# if we want to calculate m ia or to other states
B =P[-2,-2] # delete second row and column p


