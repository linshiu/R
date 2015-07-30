#########################
# STAT 425
# Homework 3
# Date: 10/3/11
# Author: Luis Lin
#########################

#Install Package

install.packages("faraway", lib="F:/R")
library("faraway",lib.loc="F:/R")
install.packages("lmtest", lib="F:/R")
library("zoo",lib.loc="F:/R")
library("lmtest",lib.loc="F:/R")
install.packages("nlme", lib="F:/R")
library("nlme",lib.loc="F:/R")

getwd()
setwd("F:/R")

#########################

#Question 1

X=c(21,20,19,18,17,16,15)
Y=c(17.26,17.07,16.37,16.40,16.13,16.17,15.98)
SD=c(1.988,1.938,1.896,2.037,1.654,1.594,1.763)

# Part a
g= lm(Y ~ X, weights=SD^-2)
plot(Y~X,xlab="X: parent diameter", 
         ylab="Y: mean offspring diamter")     	       
abline(g)
summary(g)

# Part b

beta1=summary(g)$coeff[2]
SE_beta1=0.03815
t_stat1=(beta1-1)/SE_beta1
t_stat1
pvalue=2*pt(t_stat1,5)
pvalue

gr=lm(Y~offset(X), weights=SD^-2)
anova(gr,g)

# Part c

sigma=summary(g)$sigma
chi_stat=sigma^2*(length(X)-2)
pvalue=1-pchisq(chi_stat,length(X)-2)

sigma
chi_stat
pvalue

#########################

#Question 2

library(faraway)
salmonella  # display the data
attach(salmonella)

# part a and b

X=log(dose+1)
fit0=lm(colonies~X)
summary(fit0)

fit1=lm(colonies~factor(X))
summary(fit1)

Rsq = summary(fit0)$r.sq
PureError = summary(fit1)$sigma^2 
Rsq
PureError 


# black circle: observed data points (we have multiple obs at the same x-value);
# blue line: LS line by treating X as a numerical predictor;
# red square: fitted value from a model with the linear assumption 
#                       (where we treat X as a categorical predictor)

plot(X, colonies, ylab="colonies", xlab="log(dose+1)");
abline(fit0, col="blue")
points(X, fit1$fitted, col="red", pch=2, cex=2)

# part c and d

rss.fit0=sum(fit0$res^2) 
rss.fit1=sum(fit1$res^2)
myF=(rss.fit0-rss.fit1)/(16-12)/(rss.fit1/12)
pvalue= 1-pf(myF, 4, 12)
rss.fit0
rss.fit1
myF
pvalue
anova(fit0, fit1)

#########################
#Question 3

help(divusa)
data(divusa)

# divusa data
g=lm(divorce ~ unemployed + femlab + marriage + birth + military, divusa)
n=nrow(divusa)  # sample size
cor(g$res[-1], g$res[-n])

# two graphical displays for correlated errors
# (1) plot res vs. time
# (2) plot res[i] vs res[i+1]

par(mfrow=c(1,2))
plot(divusa$year, g$res, xlab="Year", ylab="Residuals"); abline(h=0)
plot(g$res[-n], g$res[-1], xlab=expression(hat(e)[i]), ylab=expression(hat(e)[i+1])); abline(0,1)

# Durbin-Watson test
library(lmtest)
dwtest(divorce ~ unemployed + femlab + marriage + birth + military, data=divusa)


# Use nlme package
library(nlme)

g2=gls(divorce ~ unemployed + femlab + marriage + birth + military, correlation = corAR1(form=~year), method="ML", data=divusa)
# Now you should see "Generalized least squares fit by maximum likelihood..."
intervals(g2)  # you can use the CI for rho to do hypothesis testing. 
summary(g2)
summary(g)

#########################

#Question 4

# Election data
data=read.table(file="F:/R/elec2000.csv", sep=",",header=TRUE)

# Part a
Y=data$BUCHANAN
X=data$BUSH
fit0=lm(Y[-50]~X[-50])
summary(fit0)

plot(Y~X,xlab="X: Bush Votes",ylab="Y: Buchanan Votes", 
     main="Regreesion line excluding Palm Beach County")
abline(fit0)

# Part b
plot(X[-50], fit0$res,xlab="X[-50]",ylab="Residuals")
abline(h=0)

# Part c

logY=log(Y)
logX=log(X)
fit1=lm(logY[-50]~logX[-50])
summary(fit1)

par(mfrow=c(1,2))
plot(logX,logY,xlab="logX: Bush Votes",ylab="logY: Buchanan Votes", 
     main="Regression model excluding Palm Beach County")
abline(fit1)
plot(logX[-50], fit1$res,xlab="logX[-50]",ylab="Residuals")
abline(h=0)

# Part d

lx=logX[-50]
ly=logY[-50]
fitx=lm(ly~lx)
lxPalm=data.frame(lx=logX[50])
logPI=predict.lm(fitx,lxPalm,level=0.95,interval="prediction")
PI=exp(logPI)
PI
Y[50]

# Part e

fit3=lm(logY~logX)
jack = rstudent(fit3)
names(jack)=data$COUNTY
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
identify(1:nrow(data),jack,data$COUNTY)

n=nrow(data)
n
p=2
alpha=0.05
tcrit=qt(alpha/(2*n),n-p-1)
outliers=jack[abs(jack)>abs(tcrit)]
tcrit
outliers


#########################

#Question 5

library(faraway)
data(sat)

g = lm(total~expend + salary + ratio + takers, data=sat)
summary(g)

### part (a) Check the constant variance assumption for the errors

#plot res vs fitted
par(mfrow=c(1,2))
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals")
abline(h=0)
plot(g$fit,abs(g$res),xlab="Fitted",ylab="|Residuals|")

#regression res vs fit
summary(lm(abs(g$res) ~ g$fit))

# residuals vs predictors xi

par(mfrow=c(2,2))
plot(sat$expend,g$res,xlab="Expend",ylab="Residuals")
abline(h=0)
plot(sat$salary,g$res,xlab="Salary",ylab="Residuals")
abline(h=0)
plot(sat$ratio,g$res,xlab="Ratio",ylab="Residuals")
abline(h=0)
plot(sat$takers,g$res,xlab="Takers",ylab="Residuals")
abline(h=0)

# Compare and test the variances in these groups

var.test(residuals(g)[sat$takers>40],residuals(g)[sat$takers<40])

  #alternative
	#h=var(g$res[sat$takers > 40])/var(g$res[sat$takers <40])
	#df=table(sat$takers> 40)
	#1-pf(h,df[2]-1,df[1]-1)


# part (b) Check the normality assumption.

## Q-Q Plots
par(mfrow=c(1,2))
qqnorm (residuals (g), ylab="Residuals")
qqline (residuals (g))
qqnorm(rstudent(g),ylab="Studentized residuals")
abline(0,1)

## Shapiro test
shapiro.test (residuals (g))

# part (c) Check for large leverage points.

ginf = influence (g)
lev = ginf$hat
names(lev) = states


par(mfrow=c(1,2))
states = row.names(sat)
halfnorm(influence(g)$hat,labs=states, ylab="Leverages", main="Half-norm")
plot(ginf$hat,ylab="Leverages",main="Index plot of Leverages")
abline(h=2*5/nrow(sat))
identify(1:50,lev,states)

lev[lev > 2*5/nrow(sat)]

# # alternative 
x = model.matrix(g)
lev = hat(x)
plot(lev,ylab="Leverages",main="Index plot of Leverages")
abline(h=2*5/nrow(sat))
sum(lev)

# part (d) Check for outliers.

jack = rstudent(g)
names(jack)=states
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
identify(1:nrow(sat),jack,states)

n=nrow(sat)
n
p=5
alpha=0.05
tcrit=qt(alpha/(2*n),n-p-1)
outliers=jack[abs(jack)>abs(tcrit)]
max(abs(jack))
abs(tcrit)
outliers

# part (e) Check for inuential points.

cook =cooks.distance(g)
par(mfrow=c(1,2))
plot(cook,ylab="Cooks distances")
identify(1:50,cook,states)
halfnorm(cooks.distance(g), labs=states, nlab=3, ylab="Cook statistics") 

# Linear Regression without the highest cook distance data point
gl = lm(total~expend + salary + ratio + takers,subset=(cook < max(cook)))
summary(gl)
summary(g)
 
# Plot changes of coefficients when cases are excluded
ginf = influence (g)

par(mfrow=c(2,2))
plot(ginf$coef [,2], ylab="Change in expend")
identify (1:50, ginf$coef [, 2], states)
plot(ginf$coef [,3], ylab="Change in salary")
identify (1:50, ginf$coef [, 3], states)
plot(ginf$coef [,4], ylab="Change in ratio")
identify (1:50, ginf$coef [, 4], states)
plot(ginf$coef [,5], ylab="Change in takers")
identify (1:50, ginf$coef [, 5], states)

# part (f) Check the structure of the relationship between the predictors and the response.

# y vs xi

pairs(~expend+ratio+salary+takers+total,data=sat)

# residuals vs predictors xi
# same as part a.

par(mfrow=c(2,2))
plot(sat$expend,g$res,xlab="Expend",ylab="Residuals")
abline(h=0)
plot(sat$salary,g$res,xlab="Salary",ylab="Residuals")
abline(h=0)
plot(sat$ratio,g$res,xlab="Ratio",ylab="Residuals")
abline(h=0)
plot(sat$takers,g$res,xlab="Takers",ylab="Residuals")
abline(h=0)

# residual vs predicted yhat
# sames as part a.

plot(g$fit,g$res,xlab="Fitted",ylab="Residuals")
abline(h=0)

# partial regressio plots

d_e = lm(total~ salary + ratio + takers ,sat)$res
m_e = lm(expend~ salary + ratio + takers ,sat)$res
gexpend=lm(d_e~m_e )

d_s = lm(total~ expend + ratio + takers ,sat)$res
m_s = lm(salary~ expend + ratio + takers ,sat)$res
gsalary=lm(d_s~m_s)

d_r = lm(total~ expend + salary + takers ,sat)$res
m_r = lm(ratio~ expend + salary + takers ,sat)$res
gratio=lm(d_r~m_r)

d_t = lm(total~expend + salary + ratio ,sat)$res
m_t = lm(takers~expend + salary + ratio ,sat)$res
gtakers=lm(d_t~m_t)

par(mfrow=c(2,2))

plot(m_e,d_e,xlab="expend residuals",ylab="total residuals",
main="Partial Regression")
abline(gexpend)

plot(m_s,d_s,xlab="salary residuals",ylab="total residuals",
main="Partial Regression")
abline(gsalary)

plot(m_r,d_r,xlab="ratio residuals",ylab="total residuals",
main="Partial Regression")
abline(gratio)

plot(m_t,d_t,xlab="takers residuals",ylab="total residuals",
main="Partial Regression")
abline(gtakers)

# partial residual plots

par(mfrow=c(2,2))
prplot(g,1)
prplot(g,2)
prplot(g,3)
prplot(g,4)

## Alternative way
par(mfrow=c(2,2))

plot(sat$expend,g$res+g$coef['expend']*sat$expend,xlab="expend",
ylab="total(adjusted)",main="Partial Residual")
abline(0,g$coef['expend'])

plot(sat$salary,g$res+g$coef['salary']*sat$salary,xlab="salary",
ylab="total(adjusted)",main="Partial Residual")
abline(0,g$coef['salary'])

plot(sat$ratio,g$res+g$coef['ratio']*sat$ratio,xlab="ratio",
ylab="total(adjusted)",main="Partial Residual")
abline(0,g$coef['ratio'])

plot(sat$takers,g$res+g$coef['takers']*sat$takers,xlab="takers",
ylab="total(adjusted)",main="Partial Residual")
abline(0,g$coef['takers'])

## examine takers

g1 = lm(total~expend + salary + ratio + takers,subset=(takers > 40))
g2 = lm(total~expend + salary + ratio + takers,subset=(takers < 40))
summary(g1)
summary(g2)

# Alternative quick plot
par(mfrow=c(2,2)); plot(g)
