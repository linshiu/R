#########################
# STAT 425
# Homework 4
# Date: 10/22/11
# Author: Luis S. Lin
#########################

#Install Package

library(faraway)
library(MASS)

#########################
#Question 1

##Part a

data(aatemp)
attach(aatemp)

#right hockey stick function
rhs = function(x) ifelse(x < 1930,0,x-1930)

#broken line regression, constant before 1930 and linear after 
gb = lm(temp ~ rhs(year), aatemp)

#report coefficients
gb$coeff

#plot data
plot(aatemp$year,aatemp$temp,xlab="Year",ylab="Temperature")
abline(v=1930) 

#plot fitted model
x = seq(1854,2000,by=1)
py = gb$coef[1]+gb$coef[2]*rhs(x)
lines(x,py,lty=2)

##Part b

#ordinary linear model
g = lm(temp ~ year, aatemp)

#report r squared of both models
summary(gb)$r.sq
summary(g)$r.sq

##Part c

#Use poly 10
summary(lm(temp~poly(year,10)))

#Model with power 5 selected
g1=lm(temp ~ poly(year,5),aatemp)
summary(g1)

#plot data
plot(aatemp$year,aatemp$temp,xlab="Year",ylab="Temperature")

#plot fitted model
grid = seq(1854,2000,by=1)
lines(grid,predict(g1,data.frame(year=grid)))

#predict temperature
new=data.frame(year=2020)
pred.PI = predict(g1, new, level=0.95, interval="prediction")
pred.PI

#########################
#Question 2

data(ozone)
attach(ozone)

## part a

g = lm(O3 ~ temp + humidity + ibh, ozone) # fit model
summary(g)$r.sq # r-squared


## part b

par(mfrow=c(1,2));
boxcox(g,plotit=T) # plotit=T is the default setting
boxcox(g,plotit=T,lambda=seq(0,0.6,by=0.1)) # zoom-in

O3.trans=boxcox(g, lambda=seq(-2, 2, length=400))
lambda.hat = O3.trans$x[O3.trans$y == max(O3.trans$y)] 
lambda.hat # use 0.25, fourth root transformation

tmp=O3.trans$x[O3.trans$y > max(O3.trans$y) - qchisq(0.95, 1)/2];
CI=range(tmp) # 95% CI.
CI
1>CI[1] & 1<CI[2] # Check contains 1 

## part c

O3.lambda.25 = O3^.25
g.trans = lm(O3.lambda.25 ~ temp + humidity + ibh)
r2.trans=summary(g.trans)$r.sq
r2.trans

#########################
#Question 3

data(cornnit)
attach(cornnit)

## part a

g.a = lm(yield ~ nitrogen, cornnit) # fit 

plot(yield ~ nitrogen,xlab="nitrogen",ylab="yield") # plot
abline(g.a)

g1=lm(yield~factor(nitrogen)) # lack of fit test
anova(g.a, g1) 

## part b

g.b = lm(yield ~ log(nitrogen+1), cornnit)

plot(yield ~ log(nitrogen+1),xlab="log(nitrogen+1)",ylab="yield")
abline(g.b)

g2=lm(yield~factor(log(nitrogen+1)))
anova(g.b, g2)

## part c

library(car)

new.nitrogen=nitrogen+1;
boxTidwell(yield ~ new.nitrogen)
trans.nitrogen = new.nitrogen^(-.2)
g.c=lm(yield~trans.nitrogen,cornnit)

plot(yield ~ trans.nitrogen,xlab="trans.nitrogen",ylab="yield")
abline(g.c)

g3=lm(yield~factor(trans.nitrogen))
anova(g.c, g3)

## part d

summary(g.b)$r.sq
summary(g.c)$r.sq


#########################
#Question 4

data(prostate)
attach(prostate)

help(prostate)  #info
dim(prostate)   #dimension 
names(prostate) #variable names
prostate[1:5,]  #first 5 observations
prostate$svi=as.factor(prostate$svi); #categorical

n=dim(prostate)[1]; #number of observations
p=dim(prostate)[2]; #number of parameters including intercept
fullfit=lm(lpsa~., data=prostate) # full regression model
summary(fullfit)

### Model selection: leve-wise searching algorithm with AIC, BIC, and Cp

install.packages("leaps") #install package leaps
library(leaps) #load package leaps

# Compute RSS using regsubsets function with the following inputs
	#Model matrix (with no intercept column)
	#Data (response)
	#Include intercept
	#Number of subsets of each size to record = 2
	#Maximum size of subsets to examine = p
	#Exhaustive Search
	#Use exhaustive search

library(leaps)
RSSleaps=regsubsets(model.matrix(fullfit)[,-1],
prostate[,p],int=T,nbest=2,nvmax=p, really.big=T,method=c("ex"))

sumleaps=summary(RSSleaps,matrix=T)
# performs an exhaustive search over models, and gives back the best 2 models
# (with low RSS) of each size.

names(sumleaps) # components returned by summary(RSSleaps)

sumleaps$which # A logical matrix indicating which elements are in each model

# Create vector of model sizes 
# (include the intercept, so model size = 2 is intercept + 1 predictor)
msize=apply(sumleaps$which,1,sum);
msize=as.numeric(msize)

# Plot RSS vs model size
plot(msize, sumleaps$rss,xlab="model size", ylab="RSS");

# Calculate Cp, AIC and BIC for all models

Cp=sumleaps$rss/(summary(fullfit)$sigma^2) + 2*msize - n;
AIC = n*log(sumleaps$rss/n) + 2*msize;
BIC = n*log(sumleaps$rss/n) + msize*log(n);

# Compare calculations
cbind(Cp, sumleaps$cp)
cbind(BIC, sumleaps$bic)
BIC-sumleaps$bic

# It seems regsubsets uses a formula for BIC different from the one we used.
# But the two just differ by a constant, so won't affect the result.

# Plot Criterias (scale by maximum value) vs model size 
par(mfrow=c(1,3));

plot(msize, Cp/abs(max(Cp)), xlab="model size", ylab="Cp")
plot(msize, AIC/abs(max(AIC)), xlab="model size", ylab="AIC")
plot(msize, BIC/abs(max(BIC)), xlab="model size", ylab="BIC")

# Plot Criterias vs model size with zoom
par(mfrow=c(1,3));
plot(msize[msize>=3 & msize<=7], Cp[msize>=3 & msize<=7]/abs(max(Cp)), xlab="model size", ylab="Cp")
plot(msize[msize>=3 & msize<=7], AIC[msize>=3 & msize<=7]/abs(max(AIC)), xlab="model size", ylab="AIC")
plot(msize[msize>=3 & msize<=7], BIC[msize>=3 & msize<=7]/abs(max(BIC)), xlab="model size", ylab="BIC")

# Dsiplay the models with the lowest 3 values for each criterion
# order(Cp)[1:3] finds the locations of the 3 lowest values in the criterion

sumleaps$which[order(Cp)[1:3],]
Cp[order(Cp)[1:3]] 

sumleaps$which[order(AIC)[1:3],]
AIC[order(AIC)[1:3]] 

sumleaps$which[order(BIC)[1:3],]
BIC[order(BIC)[1:3]] 

##The best model returned by Cp, AIC, and BIC
# To find model for each criteria:
	#Find the location of the minimum value of the criteria
	#Find the model in the RSS matrix corresponding to this location
	#Find the variables names corresponding to this model

# Cp
varid.Cp =sumleaps$which[order(Cp)[1],]
model.Cp = names(prostate)[1:p-1][varid.Cp[-1]]

# AIC
varid.AIC=sumleaps$which[order(AIC)[1],]
model.AIC=names(prostate)[1:p-1][varid.AIC[-1]]

#BIC
varid.BIC=sumleaps$which[order(BIC)[1],]
model.BIC=names(prostate)[1:p-1][varid.BIC[-1]]

# Models
model.Cp
model.AIC
model.BIC

### Alternative: 

## Model selection: greedy algorithm
step(fullfit, direction="both")
step(fullfit, direction="backward")
step(lm(lpsa~1, data=prostate), scope=list(upper=fullfit, lower=~1),
direction="forward")

help(step)
n=length(prostate[,1])
step(fullfit, direction="both", k=log(n)) # BIC


#########################
#Question 5

data(trees)
attach(trees)
names(trees)

fullfit = lm( log(Volume) ~ Girth + Height + 
				    I(Girth^2) + I(Height^2) + 
			          Girth:Height , data=trees)
summary(fullfit)

fit.A = lm( log(Volume) ~ Girth + Height + I(Girth^2) + I(Height^2), data=trees)
summary(fit.A)

fit.B = lm( log(Volume) ~ Girth + Height + I(Girth^2) , data=trees)
summary(fit.B)

fit.C = lm( log(Volume) ~ Girth + Height + I(Height^2) , data=trees)
summary(fit.C)



n=length(trees[,1])
step(fullfit, direction="both")
step(fullfit, direction="both", k=log(n))

