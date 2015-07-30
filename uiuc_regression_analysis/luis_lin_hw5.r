#########################
# STAT 425
# Homework 5
# Date: 10/31/11
# Author: Luis Lin
#########################

#Install Packages - needed depending on computer

install.packages("faraway", lib="E:/R")
library("faraway",lib.loc="E:/R")
install.packages("lmtest", lib="E:/R")
library("zoo",lib.loc="E:/R")
library("lmtest",lib.loc="E:/R")
install.packages("nlme", lib="E:/R")
library("nlme",lib.loc="E:/R")


getwd()
setwd("E:/R")

###########################################################
#Question 1

######### Part a and b: Report best model by Cp and BIC

data(teengamb)
attach(teengamb)

help(teengamb)  #info
dim(teengamb)   #dimension 
names(teengamb) #variable names
teengamb[1:5,]  #first 5 observations
teengamb$sex=as.factor(teengamb$sex); #categorical

n=dim(teengamb)[1]; #number of observations
p=dim(teengamb)[2]; #number of parameters including intercept
g=lm(gamble ~ sex +status + income + verbal + sex:status + sex:income
+ sex :verbal, data=teengamb)

summary(g)

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

RSSleaps=regsubsets(model.matrix(g)[,-1],
teengamb[,p],int=T,nbest=2,nvmax=p, really.big=T,method=c("ex"))

sumleaps=summary(RSSleaps,matrix=T)
# performs an exhaustive search over models, and gives back the best 2 models
# (with low RSS) of each size.

names(sumleaps) # components returned by summary(RSSleaps)

sumleaps$which # A logical matrix indicating which elements are in each model

# Create vector of model sizes 
# (include the intercept, so model size = 2 is intercept + 1 predictor)
msize=apply(sumleaps$which,1,sum);
msize=as.numeric(msize)

# Calculate Cp, and BIC for all models

Cp=sumleaps$rss/(summary(g)$sigma^2) + 2*msize - n;
BIC = n*log(sumleaps$rss/n) + msize*log(n);

# Plot Criterias (scale by maximum value) vs model size 
par(mfrow=c(1,2));

plot(msize, Cp/abs(max(Cp)), xlab="model size", ylab="Cp")
plot(msize, BIC/abs(max(BIC)), xlab="model size", ylab="BIC")

# Dsiplay the models with the lowest 3 values for each criterion
# order(Cp)[1:3] finds the locations of the 3 lowest values in the criterion

sumleaps$which[order(Cp)[1:3],]
Cp[order(Cp)[1:3]] 

sumleaps$which[order(BIC)[1:3],]
BIC[order(BIC)[1:3]] 

##The best model returned by Cp, AIC, and BIC
# To find model for each criteria:
	#Find the location of the minimum value of the criteria
	#Find the model in the RSS matrix corresponding to this location
	#Find the variables names corresponding to this model

# Cp
varid.Cp =sumleaps$which[order(Cp)[1],]
model.Cp = names(g$coeff)[varid.Cp][-1]

#BIC
varid.BIC=sumleaps$which[order(BIC)[1],]
model.BIC = names(g$coeff)[varid.BIC][-1]

# Models
model.Cp
model.BIC

################ Part c: compare models

g.Cp = g=lm(gamble ~ income + verbal + sex:income, data=teengamb)
g.BIC = g=lm(gamble ~ income + sex:income, data=teengamb)

anova(g.Cp)

#nested model, compare with F-test (g.BIC is the "reduced" model) 
anova(g.BIC,g.Cp)

#or alternative look at p-value of verbal in g.Cp (since g.BIC is nested)
summary(g.Cp)

#since p-value > 0.05, cannot reject null (reduced model is better)
#the addition of variable verbal does not have a significan effect
#prefer simpler model by BIC

############### Part d: interpret final model

summary(g.BIC)
# 0 = male (reference), 1 = female
# parameters represent the difference between female and this ref level
# compared to the reference level, the female group reduces the effect of income on gambling
# an increase in income (explanatory variable) increases gambling (response)
# however this effect is greater for males than for female

#model.matrix (g.BIC)

plot (gamble ~ income, pch=as.character (sex), teengamb)              #plot data
abline(g.BIC$coeff[1],g.BIC$coeff[2]+min(sex)*g.BIC$coeff[3])         #plot for male
abline(g.BIC$coeff[1],g.BIC$coeff[2]+max(sex)*g.BIC$coeff[3], lty=2)  #plot for female
legend("topleft", lty=c(1,2), legend=c("Male","Female"))              #add legend


###########################################################
#Question 2

######### Part a: report outliers and high-influential points

data(infmort)
help(infmort)
attach(infmort)
g=lm(mortality ~ . , data=infmort)
model.matrix(g)
summary(g)

unique(region)                         #find levels of region

countries=row.names(infmort)
countries[is.na(mortality)]            #missing values
which(is.na(mortality))                #corresponding rows
countries=countries[!is.na(mortality)] #eliminate countries with missing values

#or alternative

new.infmort=na.omit(infmort)           #new data without missing
countries = row.names(new.infmort)

### Check for outliers.

n=nrow(new.infmort);n                   #data points
p=length(g$coeff)                       #predictors plus intercept
alpha=0.05
tcrit=abs(qt(alpha/(2*n),n-p-1))        #critical value

jack = rstudent(g)
names(jack)=countries
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
abline(h=tcrit);abline(h=-tcrit)
identify(1:n,jack,countries)

tcrit
outliers=jack[abs(jack)>abs(tcrit)]; outliers

#  Check for inuential points.

cook =cooks.distance(g)
names(cook)=countries
highinf=cook[cook>=1]
highnumber=length(highinf)

par(mfrow=c(1,2))
plot(cook,ylab="Cooks distances")
identify(1:n,cook,countries)
halfnorm(cooks.distance(g), labs=countries, nlab=highnumber, ylab="Cook statistics") 

highinf

######### Part b: box-cox 

install.packages("MASS") #install package MASS
library(MASS) #load package leaps

par(mfrow=c(1,2));
boxcox(g,plotit=T) # plotit=T is the default setting
boxcox(g,plotit=T,lambda=seq(-1,0,by=0.1)) # zoom-in

mortality.trans=boxcox(g, lambda=seq(-2, 2, length=400))

lambda.hat = mortality.trans$x[mortality.trans$y == max(mortality.trans$y)] 
lambda.hat
lambda.hat = round(lambda.hat,2); lambda.hat # use -0.20

tmp=mortality.trans$x[mortality.trans$y > max(mortality.trans$y) - qchisq(0.95, 1)/2];
CI=range(tmp) # 95% CI.
CI
1>CI[1] & 1<CI[2] # Check contains 1 

## transformed response

new.mortality = mortality^lambda.hat
g = lm(new.mortality ~ region + income + oil, infmort)
summary(g)

### Check for outliers.

jack = rstudent(g)
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
abline(h=tcrit);abline(h=-tcrit)
identify(1:n,jack,countries)

tcrit
outliers=jack[abs(jack)>abs(tcrit)]; outliers

#  Check for inuential points.

cook =cooks.distance(g)
highinf=cook[cook>=1]

par(mfrow=c(1,2))
plot(cook,ylab="Cooks distances")
identify(1:n,cook,countries)
halfnorm(cooks.distance(g), labs=countries, nlab=highnumber, ylab="Cook statistics") 

highinf

######### Part c: intepret coefficient

par(mfrow=c(1,2))
plot(income, new.mortality, type="n",main="oil export");
points(income[oil==levels(oil)[1]], new.mortality[oil==levels(oil)[1]], col=as.numeric(region[oil==levels(oil)[1]]))
legend("bottomright", lty=rep(1,4), col=1:4, legend=levels(region))
mycoef=coef(g)
abline(mycoef[1], mycoef[5], col=1, lty=1);
abline(sum(mycoef[c(1,2)]), mycoef[5], col=2, lty=1)
abline(sum(mycoef[c(1,3)]), mycoef[5], col=3, lty=1)
abline(sum(mycoef[c(1,4)]), mycoef[5], col=4, lty=1)

plot(income, new.mortality, type="n",main="no oil export");
points(income[oil==levels(oil)[2]], new.mortality[oil==levels(oil)[2]], col=as.numeric(region[oil==levels(oil)[2]]))
legend("bottomright", lty=rep(1,4), col=1:4, legend=levels(region))
mycoef=coef(g)
abline(sum(mycoef[c(1,6)]), mycoef[5], col=1, lty=1);
abline(sum(mycoef[c(1,2,6)]), mycoef[5], col=2, lty=1)
abline(sum(mycoef[c(1,3,6)]), mycoef[5], col=3, lty=1)
abline(sum(mycoef[c(1,4,6)]), mycoef[5], col=4, lty=1)

## after adjusting for the effect variable in slope, the effect of categorical variable
## compared to the reference level, the high level...than the.....
# coefficient of categorical variable = distance between regression line, effect of the categorical variable
# interpretation of the effect of the categorical vraible depend on the other variable if different slopes
# for the same value of the other variable, the ones with high level of the categorical variable are xx (coeff) larger/smaller
# for an increase in other variable, both are xx larger....


g2 = lm(new.mortality ~ region + income + oil + income:region + income:oil, infmort)
g3 = lm(new.mortality ~ region + income + oil + income:region , infmort)
g4 = lm(new.mortality ~ region + income + oil + income:oil, infmort)

anova(g,g2)
anova(g,g3)
anova(g,g4)

## only income:oil interaction since income:region only significant in the presence of oil

###########################################################
#Question 3

data(chickwts)
attach(chickwts)
?chickwts
names(chickwts)
attributes(feed)

######### Part a: plot

stripchart(weight~feed, method="jitter",vertical=TRUE, cex=0.6, pch=1, chickwts, col="blue")
boxplot(weight~feed, data=chickwts, outline=FALSE, add=TRUE)

######### Part b: differences in weights bewteen feed types

g = lm(weight~feed,chickwts)
summary(g) # or anova(g)

# Ho: all effect are zero
# Ha: at least one effect is non-zero
# Test statistic: F=
# Distribution under Ho: F(5,65)
# Decision: reject null since  p-value <0.05 for the F-test 
# Conlusion: there is some difference in weight between the feed types

######### Part c: diagnostics

## QQ plot

qqnorm (residuals (g))

## Residuals vs fitted plot

plot(g$fit,g$res,xlab="Fitted",ylab="Residuals",
main="Residual-Fitted plot")

plot(jitter(g$fit),g$res,xlab="Fitted",ylab="Residuals",
main="Jittered plot")

## Levene's test

summary(lm( abs(g$res) ~feed))

# or alternative
library(car)
leveneTest(weight,feed)

# Since the p-value is large,  conclude that there is no 
# evidence of a non-constant variance

###########################################################
#Question 4

data(infmort)
attach(infmort)
attributes(infmort)

######### Part a: TukeyHSD

# one-way ANOVA
g = lm(income~region,infmort)
summary(g)

# Pairwise differences
TukeyHSD(aov(income ~ region, infmort))

# Asia-Africa, Americas-Africa,Americas-Asia are not significant since
# their corresponding intervals contain zero (cannot reject true difference = 0)
# alternatively look at p-values > 0.05

######### Part b: transformation

# check data

par(mfrow=c(2,2))

g = lm(income~region,infmort)
plot(income~region,xlab="Region",ylab="Income", main="No transformation")
plot(jitter(g$fit),g$res,xlab="Fitted",ylab="Residuals",
main="No transformation")

# suggestion: use log transformation

g = lm(log(income)~region,infmort)
plot(log(income)~region,xlab="Region",ylab="Income", main="LOG transformation")
plot(jitter(g$fit),g$res,xlab="Fitted",ylab="Residuals",
main="LOG transformation")

# one-way ANOVA with tranformed response
g = lm(log(income)~region,infmort)
summary(g)

# Pairwise differences
TukeyHSD(aov(log(income) ~ region, infmort))

# Asia-Africa, Americas-Asia are not significant since
# their corresponding intervals contain zero (cannot reject true difference = 0)
# alternatively look at p-values > 0.05



