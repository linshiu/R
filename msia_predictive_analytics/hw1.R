
# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_420_Predictive_Analytics"
datafolder = "Data"
setwd(file.path(main,course, datafolder))

#### Problem 2 ##############################################
# Import data

filename = "HW1_data.csv"
mydata = read.csv(filename,header = T)
EZK = mydata


#### Part a: initial estimates for nonlinear regression

fit = lm(1/y~1/x, data=EZK)
y = 1/EZK$y
x = 1/EZK$x
fit = lm(y~x)

g0_i = 1/fit$coeff[1]
g1_i = fit$coeff[2]/fit$coeff[1]

names(g0_i)=NULL
names(g1_i)=NULL
g0_i 
g1_i

#### Part b: 
y = EZK$y
x = EZK$x

# Use nlm function

fn = function(p) {
  yhat = p[1]*x/(p[2]+x)
  sum((y-yhat)^2)} 

out = nlm(fn,p=c(g0_i,g1_i),hessian=TRUE)

theta = out$estimate  #parameter estimates
theta


# Use nls function

fn2 = function(x,p) p[1]*x/(p[2]+x)
out_nls = nls(y~fn2(x,p),start=list(p=c(g0_i,g1_i)),trace=TRUE)
summary(out_nls)

#### Problem 3 ##############################################

# Part a
# Calculate the observed Fisher information matrix and 
# the covariance matrix of the estimated parameter vector

# using the Hessian produced by nlm
# Based on this, calculate the standard errors of the estimated parameters.

MSE = out$minimum/(length(y) - length(theta))  #estimate of the error variance
InfoMat = out$hessian/2/MSE  #observed information matrix
CovTheta = solve(InfoMat)
SE = sqrt(diag(CovTheta))  #standard errors of parameter estimates
MSE
InfoMat
CovTheta
SE

# Part b
# Calculate the covariance matrix of gamma hat using the vcov() 
# function applied to the output of nls(), and based on 
# this calculate the standard errors of the estimated parameters. 
# Do the results agree with Part (a)?

CovTheta_nls = vcov(out_nls )
SE_nls = sqrt(diag(CovTheta_nls))
CovTheta_nls
SE_nls

# yes they agree with part a

# Part c
# Using the results of Part (a), calculate two-sided 
# 95% CIs on the parameters gamma0 and gamma1. Compare this with the results
# of the confint.default() function applied to the output of nls().

alpha = 0.05
df = length(y)-length(theta)
t_crit = abs(qt(alpha/2,df,lower.tail=TRUE))
z_crit = abs(qnorm(alpha/2, 0,1,lower.tail = T))


confint_nlm_t = matrix(
  c(theta[1]-t_crit*SE[1],theta[1]+t_crit*SE[1], theta[2]-t_crit*SE[2],theta[2]+t_crit*SE[2]),
  ncol = 2,
  byrow = TRUE)

rownames(confint_nlm_t) = c("p1","p2")
colnames(confint_nlm_t) = c(paste(alpha/2*100,"%"),paste((1-alpha/2)*100,"%"))

confint_nlm_z = matrix(
  c(theta[1]-z_crit*SE[1],theta[1]+z_crit*SE[1], theta[2]-z_crit*SE[2],theta[2]+z_crit*SE[2]),
  ncol = 2,
  byrow = TRUE)

rownames(confint_nlm_z) = c("p1","p2")
colnames(confint_nlm_z) = c(paste(alpha/2*100,"%"),paste((1-alpha/2)*100,"%"))

confint_nlm_t
confint_nlm_z
confint.default(out_nls)



#### Problem 4 ##############################################

# This is a repeat of Problem (3), but using bootstrapping to 
# calculate the standard errors and confidence intervals. 
# You can use the boot() command in R (requires the boot package 
# to be loaded with the library(boot) command). Use at least 20,000 bootstrap replicates.

# Part a
# Calculate and plot bootstrapped histograms of gamma0 and gamma1, 
# and calculate the corresponding bootstrapped standard errors.

library(boot)   #need to load the boot package

set.seed(1);
EZKfit = function(Z,i,theta0) {
  Zboot = Z[i,]
  x = Zboot[[2]] ;y = Zboot[[1]]
  fn = function(p) {yhat = p[1]*x/(p[2]+x); sum((y-yhat)^2)} 
  out = nlm(fn,p=theta0)
  theta = out$estimate}  #parameter estimates


EZKboot = boot(EZK, EZKfit, R=20000, theta0=c(g0_i,g1_i))
CovTheta = cov(EZKboot$t)
SE = sqrt(diag(CovTheta))
EZKboot
CovTheta
SE
plot(EZKboot,index=1)  #index=i calculates results for ith parameter
boot.ci(EZKboot,conf=c(.9,.95,.99),index=1,type=c("norm","basic"))

# Part b
# Calculate "crude" two-sided 95% CIs on gamma0 and gamma1 using 
# the normal approximation to their bootstrapped distributions.

# which theta to use theta, EZKboot$t0, apply(EZKboot$t,2,mean) ?




confint_nlm_z = matrix(
  c(theta[1]-z_crit*SE[1],theta[1]+z_crit*SE[1], theta[2]-z_crit*SE[2],theta[2]+z_crit*SE[2]),
  ncol = 2,
  byrow = TRUE)

# Part c
# Calculate the reflected two-sided 95% CIs on gamm0 and gamm1 
# (this corresponds to the type = "basic" option of the boot.ci() function).

confint_nlm_z = matrix(
  c(theta[1]-z_crit*SE[1],theta[1]+z_crit*SE[1], theta[2]-z_crit*SE[2],theta[2]+z_crit*SE[2]),
  ncol = 2,
  byrow = TRUE)

# Part d
# Do the CIs in part (c) agree with those in part (b)? 
# Relate this to the histograms you see in part (a).

#### Problem 5 ##############################################




#### Problem 6 ##############################################

# Use nlm function

fn = function(p) {
  yhat = p[1]+p[2]*sqrt(x)
  sum((y-yhat)^2)} 

out = nlm(fn,p=c(g0_i,g1_i),hessian=TRUE)

theta = out$estimate  #parameter estimates
theta


# Use nls function

fn3 = function(x,p) p[1]+p[2]*sqrt(x)
out_nls2 = nls(y~fn3(x,p),start=list(p=c(g0_i,g1_i)),trace=TRUE)
summary(out_nls2)

AIC(out_nls)
AIC(out_nls2)

# Model 1 is better lower AIC

#### Problem 7 ##############################################

CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m<-floor(n/K)  #approximate size of each part
  r<-n-m*K  
  I<-sample(n,n)  #random reordering of the indices
  Ind<-list()  #will be list of indices for all K parts
  length(Ind)<-K
  for (k in 1:K) {
    if (k <= r) kpart <- ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart<-((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] <- I[kpart]  #indices for kth part of data
  }
  Ind
}

Nrep<-20 #number of replicates of CV
K<-18  #K-fold CV on each replicate
n=nrow(EZK)
#y<-MLC$Efficiency
SSE<-matrix(0,Nrep,2)
for (j in 1:Nrep) {
  Ind<-CVInd(n,K)
  yhat1<-y;
  yhat2<-y;
  for (k in 1:K) {
    out<-nls(y~fn2(x,p),start=list(p=c(g0_i,g1_i)),trace=TRUE)
    yhat1[Ind[[k]]]<-as.numeric(predict(out,EZK[Ind[[k]],]))
    out<-lm(y~sqrt(x))  #some second model to compare
    yhat2[Ind[[k]]]<-as.numeric(predict(out,EZK[Ind[[k]],]))
  } #end of k loop
  SSE[j,]=c(sum((y-yhat1)^2),sum((y-yhat2)^2))
} #end of j loop
SSE
apply(SSE,2,mean)


#### Problem 8 ##############################################
