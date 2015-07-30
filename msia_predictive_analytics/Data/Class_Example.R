
# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_420_Predictive_Analytics"
datafolder = "Data"
setwd(file.path(main,course, datafolder))

#### Problem 2 ##############################################
# Import data
filename = "MLC.csv"
mydata = read.csv(filename,header = T)
MLC = mydata


#### Part a: initial estimates for nonlinear regression

fit = lm(y~x, data=MLC)

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
x1<-MLC$Location;x2<-MLC$Week;y<-MLC$Efficiency


fn = function(p) {
  yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2)
  sum((y-yhat)^2)} 

out = nlm(fn,p=c(1,0,-.5,-.1),hessian=TRUE)

theta = out$estimate  #parameter estimates
theta


# Use nls function

fn2 <- function(x1,x2,p) p[1]+p[2]*x1+p[4]*exp(p[3]*x2)

out_nls = nls(y~fn2(x1,x2,p),start=list(p=c(1,0,-.5,-.3)),trace=TRUE)
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
SE_nls = sqrt(diag(CovTheta))
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
MLC<-read.table("MLC.csv",sep=",",header=TRUE)
MLCfit<-function(Z,i,theta0) {
  Zboot<-Z[i,]
  x1<-Zboot[[1]];x2<-Zboot[[2]];y<-Zboot[[3]]
  fn <- function(p) {yhat<-p[1]+p[2]*x1+p[4]*exp(p[3]*x2); sum((y-yhat)^2)} 
  out<-nlm(fn,p=theta0)
  theta<-out$estimate}  #parameter estimates
MLCboot<-boot(MLC, MLCfit, R=5000, theta0=c(1,-.05,-.14,-.55))
CovTheta<-cov(MLCboot$t)
SE<-sqrt(diag(CovTheta))
MLCboot
CovTheta
SE
plot(MLCboot,index=1)  #index=i calculates results for ith parameter
boot.ci(MLCboot,conf=c(.9,.95,.99),index=1,type=c("norm","basic"))


# Part b
# Calculate "crude" two-sided 95% CIs on gamma0 and gamma1 using 
# the normal approximation to their bootstrapped distributions.

theta = 
  
 theta[1]-z_crit*SE[1]
 theta[1]+z_crit*SE[1]

  
  confint_nlm_z = matrix(
    c(theta[1]-z_crit*SE[1],theta[1]+z_crit*SE[1], theta[2]-z_crit*SE[2],theta[2]+z_crit*SE[2]),
    ncol = 2,
    byrow = TRUE)

# Part c
# Calculate the reflected two-sided 95% CIs on gamm0 and gamm1 
# (this corresponds to the type = "basic" option of the boot.ci() function).

q = quantile(MLCboot$t[,1], c(alpha/2, 1-alpha/2))
theta[1]-(q[2]-theta[1])
theta[1]+(theta[1]-q[1])



         
confint_nlm_z = matrix(
  c(theta[1]-z_crit*SE[1],theta[1]+z_crit*SE[1], theta[2]-z_crit*SE[2],theta[2]+z_crit*SE[2]),
  ncol = 2,
  byrow = TRUE)

# Part d
# Do the CIs in part (c) agree with those in part (b)? 
# Relate this to the histograms you see in part (a).

# yes if not skewed


#### Problem 5 ##############################################

Yhat0<-MLCboot$t0
Yhatboot<-MLCboot$t
e<-rnorm(nrow(Yhatboot), mean=0, sd=sqrt(MSE))  #MSE is from the earlier analysis
Yboot<-Yhatboot-e
SEY<-sqrt(var(Yboot))
Yquant<-quantile(Yboot,prob=c(.025,.975))
L<-2*Yhat0-Yquant[2]
U<-2*Yhat0-Yquant[1]
hist(Yboot,100)
c(L,U)
SEY

