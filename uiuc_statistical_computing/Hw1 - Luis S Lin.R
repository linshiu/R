####################
# Class: STAT 428  #
# Date: 02/01/13   #
# Homework: 1      #
# Luis Steven Lin  #
####################

#### Problem 1
#### Write R-code to calculate E(Nsd) 
#### for having ten sons and eight daughters

# Initialize variables
s=10; d=8; p=0.5; q=0.5

# Create matrix and fill first column and row
# Entry in matrix represents E[Nsd]
x = matrix(nrow = s+1, ncol = d+1);
x[1,] = c(1,(1:d)/q)
x[,1] = c(1,(1:s)/p)

# Iterate
for (i in 1:s){
	for (j in 1:d) {
		x[i+1,j+1] = 1 + p*x[i,j+1] + q*x[i+1,j]
		}
	}
# Display Results
noquote("Expected number of children E(Nsd):"); print(x[s+1,d+1], digits = 5)


#### Problem 2
#### Write R function to compute 
#### S2n using recurrence relation

# Find Variance using recursive expression for a given dataset
# v = biased sample variance
# m = sample mean
# N = number of observations 
# x = dataset
# n = iteration index

findVar = function(x){

	# Initialize variables
	N = length(x);
	m = x[1];
	v = 0;

	# Find variance recursively
	for (n in 1:(N-1)){
		# compute mean with n+1 observations
		m = n/(n+1)*m + x[n+1]/(n+1);
		# compute variance with n+1 observations
		v = n/(n+1)*v + 1/n*(x[n+1] - m)^2;
		}

	# Output results
	return(v)
	}

# Test function

data = sample(1:100, 20, replace=T)
findVar(data)

sum((data-mean(data))^2)/length(data)


