####################
# Class: STAT 428  #
# Date: 05/01/13   #
# Homework: 4      #
# Luis Steven Lin  #
####################


#### Problem 1 ###########################################
# Newton Method

# Newton method to find root
# Inputs: function, derivative, initial guess
#         maximum number of iterations

newton = function(number,x0,maxiter){

	# Define Function
	f = function(x){x^(2)-number} 

	# Define Derivative
	df = function(x){2*x}      

	# Initialize variables
	maxiter = maxiter
	iter=1
	err=1
	x = x0

	# Loop
	while( err > 10^-12 & iter <= maxiter) {
		xnew = x - f(x)/df(x)
		err = abs( (xnew-x)/xnew )*100
		x = xnew
		iter = iter + 1;	
	}

	# Results
	print(paste("Iterations:", iter))
	print(paste("Root of ", number, "is:"))
	return(x)
}

# Find root of 897 using newton
newton(897,30,100)  

# Check with built-in R function
sqrt(897)

#### Problem 2 ###########################################
#### Part a) MLE using Newton

x = c(1.4413414, 1.0559832, 1.2363259, 0.2419938, 0.4254919,
0.1195233, 1.1289144, 0.2114744, 1.4926782, 0.5879979)

newton = function(f,df,x0,maxiter){

	# Initialize variables
	maxiter = maxiter
	iter=1
	err=1
	x = x0

	# Netwon loop

	while( err > 10^-12 & iter <= maxiter) {
		xnew = x - f(x)/df(x)
		err = abs( (xnew-x)/xnew )*100
		x = xnew
		iter = iter + 1;	
	}
	return(list(x=x,iter=iter))
}

weibullMLE = function(x,k0,maxiter,print=FALSE){

	n = length(x)
	lnx = log(x)

	# Define first derivative of log likelihood

	dL1 = function(k){ n/k + sum(lnx) - sum((x^k)*lnx) }

	# Define second derivative of log likelihood

	dL2 = function(k){ -n/k^2 - sum((x^k)*lnx^2) }

	# Newton to find root of first derivative
	
	mle = newton(dL1,dL2,k0,maxiter)
	
	# Results
	if (print==TRUE){
		print(paste("Iterations:", mle$iter))
		print(paste("MLE estimate of k: ", mle$x))}
	return(mle$x)
}
 
# Test
k = weibullMLE(x,1,100,print=TRUE)
k

#### Part b) Simulate data from a Weibull distribution 
#### with parameter k in R and  use function from part 
#### a to find the MLE of k for this simulated random sample x

set.seed(1)
k = 2
n = 100
x = rweibull(n,k)

k = weibullMLE(x,1,100,print=TRUE)
k

#### Part c) Generate 1000 such samples of X in a loop.
####  For each sample, find the MLE for k, say k and record
#### it in a vector. Plot a histogram of the k after the loop runs

n=1000
k = 1:n
for (i in 1:n){
	set.seed(i)
	x = rweibull(100,2)
	k[i] = weibullMLE(x,1,1000);
	}
hist(k,density=30)

#### Part d) What distribution does this histogram
#### of k (obtained in c) look like?

library(MASS)

x=k
weibMLE = fitdistr(k,"weibull")$estimate
normalMLE = fitdistr(k,"normal")$estimate
gammaMLE = fitdistr(x,"gamma")$estimate

hist(k,freq=FALSE,ylim=c(0,3),density=20)

curve(dweibull(x,weibMLE["shape"],weibMLE["scale"]),col=2,lwd=2,add=TRUE)
curve(dnorm(x,normalMLE["mean"],normalMLE["sd"]),col=3,lwd=2,add=TRUE)
curve(dgamma(x,gammaMLE["shape"],gammaMLE["rate"]),col=4,lwd=2,add=TRUE)

legend(2.4,3,c("weibull","normal","gamma"),col=c(2,3,4),lty=1,lwd=2)


#### Problem 3 ###########################################

set.seed(1)
N = 100 #Number of observations
p = .5 #Mixing parameter
lambdas = c(1.2, 4.8) #Exponential rate parameter for each distribution
Z = sample(c(1,2), N, replace = TRUE, prob=c(1-p,p))
X = sapply(Z,function(c) rexp(1,rate = lambdas[c])) #goes through each
#entry in Z and applies the function to it.
hist(X) #it is pretty hard to see a mixture of exponentials

EM = function(X,print=FALSE){
	Max.iter=10000
	L=c(1,5)
	tol=.Machine$double.eps^0.5
	L.old=L+1
	for(j in 1:Max.iter){
	#Estep
		f1=dexp(X,L[1])
		f2=dexp(X,L[2])	
		p1=f1/(f1+f2)
		p2=1-p1
    	#Mstep (note that the mean is the inverse of the pararmeter)
	phat=1/(sum(p1)/length(p1))
	L[1]=1/(sum(p1*X)/sum(p1))
	L[2]=1/(sum(p2*X)/sum(p2))
	if (print==TRUE){
	print(paste("Iteration: ", j, ", lamda1: ", round(L[1],5), 
			", lamda2: ", round(L[2],5)))   } 	
    	#stop?
   	if(sum(abs(L-L.old)/L.old)<tol) break
    	L.old=L
    }
	return(L)
 }

ret = EM(X,print=TRUE)
ret

# With larger N

set.seed(1)
N = 100000 #Number of observations
p = .5 #Mixing parameter
lambdas = c(1.2, 4.8) #Exponential rate parameter for each distribution
Z = sample(c(1,2), N, replace = TRUE, prob=c(1-p,p))
X = sapply(Z,function(c) rexp(1,rate = lambdas[c])) #goes through each
#entry in Z and applies the function to it.

ret = EM(X,print=TRUE)
ret

