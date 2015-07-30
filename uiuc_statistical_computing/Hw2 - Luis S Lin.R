####################
# Class: STAT 428  #
# Date: 02/21/13   #
# Homework: 2      #
# Luis Steven Lin  #
####################

#### Problem 1
#### R code to fnd a root of x^2 = 0 by Newton's method 
#### starting from x0 = 0.5
#### Give a table for the value obtained in the first#### five iterations.

# Define Function

f = function(x){
	return(x^2)}

# Define Derivative

df = function(x){
	return(2*x)}

# Initialize variables
table = matrix(nrow=5,ncol=2)
colnames(table)=c('Iteration','Root estimate')
maxiter = 5
iter=1
err=1
x0 = 0.5
x = x0

# Loop
while( err > 10^-12 & iter <= maxiter) {
	xnew = x - f(x)/df(x)
	err = abs( (xnew-x)/xnew )*100
	x = xnew
	table[iter,1]=iter
	table[iter,2]=xnew
	iter = iter + 1;
	}
table

#### Problem 2
#### R code to fnd a root of x^1/3 = 0 by Newton's method 
#### starting from x0 = 0.5
#### Give a table for the value obtained in the first#### five iterations.

# Define Function

f = function(x){
	return(x^(1/3))}

# Define Derivative

df = function(x){
	return(1/3*x^(-2/3))}

# Initialize variables
table = matrix(nrow=5,ncol=2)
colnames(table)=c('Iteration','Root estimate')
maxiter = 5
iter=1
err=1
x0 = 0.5
x = x0

# Loop
while( err > 10^-12 & iter <= maxiter) {
	xnew = x - f(x)/df(x)
	#err = abs( (xnew-x)/xnew )*100
	x = xnew
	table[iter,1]=iter
	table[iter,2]=xnew
	iter = iter + 1;
	}
table

# Repeat the algorithm by simplifying f/f' expression
table = matrix(nrow=5,ncol=2)
colnames(table)=c('Iteration','Root estimate')
maxiter = 5
iter=1
err=1
x0 = 0.5
x = x0

# Loop
while( err > 10^-12 & iter <= maxiter) {
	xnew = x - 3*x
	err = abs( (xnew-x)/xnew )*100
	x = xnew
	table[iter,1]=iter
	table[iter,2]=xnew
	iter = iter + 1;
	}
table

#### Problem 3

newton = function(x,y){
	# Scale x2 by 100 to make computations more manageable
	x[,2]=x[,2]/100

	# Note: x1 is not scaled because beta1 is the intercept

	beta0 = rep(0.5,ncol(x))
	epsis = 10^-10
	epsia = 1
	maxiter=100
	iter = 0
	while(epsia >= epsis & iter<=maxiter){
		# Lambda
		e = as.vector(exp(x%*%beta0))

		# Gradient
		DL=t(x)%*%(y - e)	

		# Second partial derivatives
		D2L1= as.numeric(-x[,1]^2%*%e)
		D2L2= as.numeric(-x[,2]^2%*%e)
		D2L12 = as.numeric(-(x[,1]*x[,2])%*%e)

		# Hessian
		D2L = matrix(c(D2L1,D2L12,D2L12,D2L2),nrow=2,ncol=2,byrow=T)

		# Newton update
		beta = beta0-as.vector(solve(D2L)%*%DL)
		
		# Tolerance error
		epsia=sum(abs((beta-beta0)/beta)*100)
		beta0=beta
		iter=iter+1
	}

# Scale beta 2 back to original units
beta[2]=beta[2]/100

print(paste("Iterations: ",iter))
print("Estimates for beta1 and beta2:")
return(beta)
}

x1 = rep(1, 17) 
x2 = c(36,531,4233,8682,7164,2229,600,164,57,722,1517,1828,1539,2416,3148,3465,1440)
y = c(0,0,130,552,738,414,198,90,56,50,71,137,178,194,290,310,149)
x = cbind(x1,x2) 

newton(x,y)

# Use R built-in function to compare

data= data.frame(x2,y)
glm(y~.,data=data,family=poisson)

# Using single value decomposition

newton2 = function(x,y){
	# Scale x2 by 100 to make computations more manageable
	x[,2]=x[,2]/100

	# Note: x1 is not scaled because beta1 is the intercept

	beta0 = rep(0.5,ncol(x))
	epsis = 10^-10
	epsia = 1
	maxiter=100
	iter = 0
	while(epsia >= epsis & iter<=maxiter){
		# Lambda
		e = as.vector(exp(x%*%beta0))

		# Use single value decomposition
		W=diag(-e)
		ystar = y - e
		my.M = t(x)%*%W%*%x
		my.svd=svd(my.M)
		my.svdinv=my.svd$u%*%diag(1/my.svd$d)%*%my.svd$v
		
		beta=beta0-my.svdinv%*%t(x)%*%ystar

		# Tolerance error
		epsia=sum(abs((beta-beta0)/beta)*100)
		
		beta0=beta
		iter=iter+1
	}

# Scale beta 2 back to original units
beta[2]=beta[2]/100

print(paste("Iterations: ",iter))
print("Estimates for beta1 and beta2:")
return(beta)
}

newton2(x,y)
