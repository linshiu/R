#########################
# STAT 425              #
# Homework 7            #
# Date: 12/02/11        #         
# Author: Luis Lin      #
#########################

###Install Packages - needed depending on computer

install.packages("lmtest", lib="K:/R/R-2.13.1/library")
library("zoo",lib="K:/R/R-2.13.1/library")
library("lmtest",lib="K:/R/R-2.13.1/library")

install.packages("nlme", lib="K:/R/R-2.13.1/library")
library("nlme",lib="K:/R/R-2.13.1/library")

install.packages("leaps",lib="K:/R/R-2.13.1/library")
library("leaps",lib="K:/R/R-2.13.1/library")

install.packages("faraway", lib="K:/R/R-2.13.1/library")
library("faraway",lib="K:/R/R-2.13.1/library")

install.packages(lib="K:/R/R-2.13.1/library")
library("MASS",lib="K:/R/R-2.13.1/library") 

getwd()
setwd("K:/R")

library(faraway)
library(MASS)
library(class)

#######################################################################
### PROBLEM 1

### Part a: Fit a binomial regression with Class as the response and  
### the other nine variables. Report the residual deviance and 
### associated degrees of freedom. Can this information be used to 
### determine if this model fits the data? Explain.

# Class is binary, can use directly as response var. in glm function

g0=glm(Class~., data=wbca, family="binomial")
summary(g0)
deviance(g0)
df.residual(g0)

# Residual deviance: 89.464  on 671  degrees of freedom
# No, this information cannot be used to determine if the model fits
# the data well since there is only 1 observation at each location

### Part b: Use AIC as the criterion to determine the best subset 
### of variables. (Use the step function.)

g=step(g0)
summary(g)
names(g$coef)[-1]

# Best subset of variables: 
# Adhes, BNucl, Chrom, Mitos, NNucl, Thick, UShap  

### Part c: Use the reduced model to predict the outcome for a new 
### patient with predictor variables 1, 1, 3, 2, 1, 1, 4, 1, 1 
### (same order as above). Give a confidence interval for your 
### prediction.

newdata=wbca[1,-1]; # to keep original names for data frame
newdata[1,]=c(1,1,3,2,1,1,4,1,1);
new.pred=predict(g, newdata, se=T); new.pred
ilogit(new.pred$fit)
ilogit(c(new.pred$fit - 1.96*new.pred$se.fit, new.pred$fit +
	   1.96*new.pred$se.fit))

# Note: predict returns the predicted value in the scale of 
# the linear predictor

# Prediction: 0.992
# Confidence Interval: (0.978,0.997)

### Part d: Suppose that a cancer is classified as benign if
### p > 0.5 and malignant if p < 0.5. Compute the number of 
### errors of both types that will be made if this method is
### applied to the current data with the reduced model.

# Note: fitted value on the scale of the response variable
yhat= g$fitted.values; 

# determine if prediction is benign (1) or malignant (0)
yhat=yhat>0.5

# create contingency table
table(yhat, wbca$Class)

# errors
class0Errors=sum(wbca$Class==0 & yhat!=0); class0Errors
class1Errors=sum(wbca$Class==1 & yhat!=1); class1Errors
totalErrors=sum(yhat != wbca$Class); totalErrors

# error percentage
n=length(wbca$Class)
PercentError=round(totalErrors/n*100,2);PercentError

# 0 if malignant, 1 if benign
# FALSE(0) if malignant, TRUE(1) if benign

# Class 0: p<0.5 (malignant)
# Class 1: p>0.5 (benign)

# Number of class 0 errors = 11
# Number of class 1 errors = 9
# Total number of errors = 20

### Part e: split the data into two parts | assign every third
### observation to a test set and the remaining two thirds of 
### the data to a training set. Compare the prediction accuracy 
### (on the test data) from the following four models:

n=dim(wbca)[1]
library(class)
totalErrors.AIC=rep(0,25)
PercentError.AIC=rep(0,25)
totalErrors.AIC.knn=rep(0,25)
PercentError.AIC.knn=rep(0,25)
totalErrors.BIC=rep(0,25)
PercentError.BIC=rep(0,25)
totalErrors.BIC.knn=rep(0,25)
PercentError.BIC.knn=rep(0,25)

# repeat experiment 25 times
for(i in 1:25) {

# split the data: test data (1/3 obs), training data (2/3 obs)
test.id=sample(1:n, round(n/3));
test.data=wbca[test.id,]; train.data=wbca[-test.id,]

# use training data to determine model
g0=glm(Class~., data=train.data, family="binomial")

# Errors for AIC and logistic regression
g=step(g0, trace=0)
yhat=predict(g, newdata=test.data, type="response")
yhat=yhat>0.5
totalErrors.AIC[i]=sum(yhat != test.data[,1]);
PercentError.AIC[i]=round(totalErrors.AIC[i]/dim(test.data)[1]*100,2)

# Errors for  1-NN (knn with k=1) with variables selected by AIC
var.names=names(g$coef)[-1]
yhat=knn(train.data[,var.names], test.data[,var.names],train.data[,1])
totalErrors.AIC.knn[i]=sum(yhat != test.data[,1]); 
PercentError.AIC.knn[i]=round(totalErrors.AIC.knn[i]/dim(test.data)[1]*100,2)

# Errors for BIC and logistic regression
g=step(g0, k=log(dim(train.data)[1]), trace=0)
yhat=predict(g, newdata=test.data, type="response")
yhat=yhat>0.5
totalErrors.BIC[i]=sum(yhat != test.data[,1]);
PercentError.BIC[i]=round(totalErrors.BIC[i]/dim(test.data)[1]*100,2)

# 1-NN (knn with k=1) with variables selected by BIC
var.names=names(g$coef)[-1]
yhat=knn(train.data[,var.names], test.data[,var.names],train.data[,1])
totalErrors.BIC.knn[i]=sum(yhat != test.data[,1]); 
PercentError.BIC.knn[i]=round(totalErrors.BIC.knn[i]/dim(test.data)[1]*100,2)
}

totalErrors=data.frame('AIC'=totalErrors.AIC,
			     'BIC'=totalErrors.BIC,
			     'AIC.knn'=totalErrors.AIC.knn,
		           'BIC.knn'=totalErrors.BIC.knn)

PercentErrors=data.frame('AIC'=PercentError.AIC,
			       'BIC'=PercentError.BIC,
			       'AIC.knn'=PercentError.AIC.knn,
		             'BIC.knn'=PercentError.BIC.knn)

totalErrors
PercentErrors
summary(totalErrors)
summary(PercentErrors)

colMeans(totalErrors)
colMeans(PercentErrors)

# Note: predict with type='Response' returns value in scaled response
# Results shown above
# The number of errors (and percent errors) is higher for 1NN and BIC
# predictions

#######################################################################
### PROBLEM 2

data(pima)
attach(pima)

### Part a: Perform simple graphical and numerical summaries of the 
### data. Can you find any obvious irregularities in the data? 
### If you do,  take appropriate steps to correct the problems.

summary(pima)

# Glucose,Diastolic,Triceps,Insulin & Bmi have minimum values of zero.
# No blood pressure is not good for the health — something must
# be wrong.

# Kernel Densities Estimates and Sorted data vs index
par(mfrow=c(1,2))
plot(density(pima$diastolic,na.rm=TRUE))
plot(sort(pima$diastolic),pch=".")

sort(pima$diastolic)[1:40]

# First 36 values are zero
# It seems likely that the zero has been used as a missing value code
# Code zero values as NA

pima$diastolic[pima$diastolic == 0] = NA
pima$glucose[pima$glucose == 0] = NA
pima$triceps[pima$triceps == 0] = NA
pima$insulin[pima$insulin == 0] = NA
pima$bmi[pima$bmi == 0] = NA

# Variable test is categorical
pima$test = factor(pima$test)

# Numerical Summary
summary(pima)

# Graphical Summaries

# Kernel Densities Estimates and Sorted data vs index
par(mfrow=c(1,2))
plot(density(pima$diastolic,na.rm=TRUE))
plot(sort(pima$diastolic),pch=".")

# Plots
par(mfrow=c(1,2))
plot(diabetes ~ diastolic,pima)
plot(diabetes ~ test,pima)

### Part b: Fit a model with the result of the diabetes 
### test as the response and all the other variables as
### predictors. Can you tell whether this model fits the data?

# Test is binary, can use directly as response var. in glm function

g0=glm(test ~., data=pima, family="binomial")
summary(g0)

# No, cannot used deviance to determine if the model fits
# the data well since there is only 1 observation at each location

### Part c: What is the difference in the odds of testing positive 
### for diabetes for a woman with a BMI at the first quartile 
### compared with a woman at the third quartile, assuming that all
### other factors are held constant? Give a confidence interval for
### this difference.

# Compute first and third quartile bmi
bmi.Q25=as.numeric(quantile(pima$bmi,na.rm=TRUE,0.25))
bmi.Q75=as.numeric(quantile(pima$bmi,na.rm=TRUE,0.75))
diff.bmi = bmi.Q75-bmi.Q25

# computer the factor difference
odds.diff = exp(coef(g0)["bmi"]*diff.bmi)
odds.diff = round(as.numeric(odds.diff),3)
odds.diff

# alternative 1
x0=rep(1,dim(pima)[2])
x0[7]=bmi.Q25
eta0=sum(x0*g0$coeff)

x1=rep(1,dim(pima)[2])
x1[7]=bmi.Q75
eta1=sum(x1*g0$coeff)
exp(eta1-eta0)

# alternative 2
newdata.25=pima[1,-dim(pima)[2]]; # to keep original names for data frame
newdata.25[1,]=rep(1,dim(pima)[2]-1)
newdata.25['bmi']=bmi.Q25
new.pred.25=predict(g0, newdata.25, se=T);
odds.25=exp(new.pred.25$fit)

newdata.75=pima[1,-dim(pima)[2]]; # to keep original names for data frame
newdata.75[1,]=rep(1,dim(pima)[2]-1)
newdata.75['bmi']=bmi.Q75
new.pred.75=predict(g0, newdata.75, se=T);
odds.75=exp(new.pred.75$fit)

odds.75/odds.25

# compute confidence interval

# standard error of bmi coefficient
se.bmi = summary(g0)$coeff['bmi','Std. Error']

# upper and lower estimates of bmi coefficient
low.bmi = as.numeric(g0$coeff['bmi'] - 1.96*se.bmi)
up.bmi = as.numeric(g0$coeff['bmi'] + 1.96*se.bmi)

# upper and lower estimates of difference 
odds.diff.low=round(exp(low.bmi*diff.bmi),3)
odds.diff.up=round(exp(up.bmi*diff.bmi),3)

# estimated difference
odds.diff.low
odds.diff
odds.diff.up

# Note: differnce is the ratio of odds in Q3 over  odds in Q1.
# Thus, the estimated multiplicative difference is 1.9, 
# with confience interval of (1.167,3.094).
# Interpretation: The odds of testing positive for diabetes 
# for a woman with a BMI at the third quartile is 1.9 times more
# than that of a woman at the first quartile. In other words,
# being in the third quartile increases the odds of testing
# positive by a factor of 1.9 compared to the first quartile. 

# NOTE: for better interpretability of difference in odds, 
# the difference is computed as the multiplicative difference,
# that is, the factor change in odds by changing bmi
# log(odds) = intercept + coeff.bmi*bmi + ... + coeff.var1*var1
# For change only in bmi: odd2/odd1 = exp(coeff.bmi(bmi2-bmi1))
# since other termms cancel out because same for bmi2 and bmi1

# Interpretation of coefficients: 
# a unit increase in bmi with other variables fixed, increases the
# log-odds of success by coeff(bmi) or increases the odds of success
# by a factor of exp (coeff(bmi)). Thus, a incrase of bmi2-bmi
# increases the odds by a factors of exp (coeff(bmi2-bmi1))

### Part d: Do women who test positive have higher diastolic 
### blood pressures? Is the diastolic blood pressure signicant 
### in the regression model? Explain the distinction between the 
### two questions and discuss why the answers are only apparently 
### contradict?

t.test(pima$diastolic[pima$test==1],pima$diastolic[pima$test==0])

# p-value < 0.05, reject null hypothesis that difference in means 
# are zero. Thus, true difference in means is not equal to zero. 
# Which means women who test positive have higher diastolic 
# blood pressure on average since the difference in means
# is significant  

summary(g0)$coef['diastolic','Pr(>|z|)']

# The p-value of the diastolic coefficient in the full regression
# model is greater than 0.05. Thus, it is not significant. 
# The model indicates that there is not a statistical significant 
# relationship between diastolic and the test.

# However, answers don't contradict because the difference in means
# refers to the marginal effect, while the coefficient in the
# regression model refers to the join effect, that is conditioned
# with other variables. Since variables can be correlated with
# each other, the effect might not be significant in the presence
# of othe variables when they are accounted for. 

summary(glm(test~diastolic, data=pima, 
	   family="binomial"))$coef['diastolic','Pr(>|z|)']

# Using diastloic as the only predictor shows it is significant, 
# so there is no contradiction. 

### Part e: Perform diagnostics on the regression model, reporting 
### any potential violations and any suggested improvements to the model.
### Hint: produce the halfnorm plot for residuals and calculate the 
### estimated dispersion.

halfnorm(residuals(g0))

(sigma2 = sum(residuals(g0,type="pearson")^2) /df.residual(g0))

# Halfnorm: no single outlier is apparent.
# Dispersion Parameter Estimate: very close to 1, normal assumption fine


### Part f: Predict the outcome for a woman with predictor 
### values 1, 99, 64, 22, 76, 27, 0.25, 25 (same order as in the dataset). 
### Give a confidence interval for your prediction.

newdata=pima[1,-dim(pima)[2]]; # to keep original names for data frame
newdata[1,]=c(1, 99, 64, 22, 76, 27, 0.25, 25);
new.pred=predict(g0, newdata, se=T); new.pred
ilogit(new.pred$fit)
ilogit(c(new.pred$fit - 1.96*new.pred$se.fit, new.pred$fit +
	   1.96*new.pred$se.fit))

# Prediction = 0.0457
# Confidence interval : (0.0250,0.0821)
# ilogit = pi = P(Yi = 1 | X = xi)
# A 4.57% predicted chance of testing positive for the given women profile

#######################################################################
### PROBLEM 3

data(aflatoxin)

### Part a: Build a model to predict the occurrence of liver cancer.
### Compute the ED50 level.

# Construct a two-column matrix with the first column representing the
# number of “successes” tumor and the second column the number of
# “failures” total-tumor

g0=glm(cbind(tumor,total-tumor)~dose, data=aflatoxin, family="binomial")
summary(g0)

# ED50: effective dose for which there will be a 50% chance of success
# log(p/(1-p)) = logit(p) = b0 + b1x , solving for x: 
# x=(logit(p)-b0)/b1. For p=1/2, logit(1/2)=0, so x = -b0/b1

g0$coef
ED50 = as.numeric(- g0$coef[1]/g0$coef[2]); ED50

# ED50 = 33.7005

### Part b: Can you tell whether this model fits the data? 
### Hint: lack of fit test; check whether you should use deviance 
### or scaled deviance.

# Compute Dispersion Parameter

dp = sum(residuals(g0,type="pearson")^2) /df.residual(g0); dp

# Since dp=0.532 is not close to 1 and dp<1, indicates underdispersion

pchisq(deviance(g0),df.residual(g0),lower=FALSE)

# p-value>>.05: no evidecce of lack of fit, current model fits 
# data sufficiently well

#######################################################################
### PROBLEM 4

data(discoveries)

### The dataset discoveries lists the numbers of \great" inventions 
### and scientific discoveries in each year from 1860 to 1959. Has the 
### discovery rate remained constant over time?

year=1860:1959
g=glm(discoveries ~ year, family="poisson")
summary(g)$coef['year','Pr(>|z|)']

# When comparing Poisson models with overdispersion, an F-test rather
# than a chi-squared test should be used
# The drop1 function tests each predictor relative to the full. 

drop1(g, test="F")

# p-value<.05, We see that year as predictor is significant.

dp=sum(residuals(g, typp="pearson")^2)/df.residual(g); dp
(g$null.deviance - g$deviance)/dp

# dp>1: evidence that p-value in regression was overestimated
# since didn't scale parameter (overestimate p-value means
# that the p-value was smaller than it should be)

summary(g, dispersion=dp)$coef['year','Pr(>|z|)']

# p-value <0.05 when estimated dispersion parameter used.
# Effect of year stat significant, which implies rate of 
# discovery not constant over time

#######################################################################
### PROBLEM 5

data(dvisits)

### Part a: Build a Poisson regression model with doctorco as the resp. 
### and sex, age,agesq, income, levyplus, freepoor, freerepa, illness, 
### actdays,hscore, chcond1,and chcond2 as possible predictor variables 
### Considering the deviance of this model,does the model fit the data?

g = glm(doctorco ~ sex + age + agesq + income + levyplus + freepoor + 
                   freerepa + illness + actdays + hscore + chcond1 +
			 chcond2 ,family=poisson, dvisits)
summary(g)
deviance(g)
df.residual(g)

pchisq(g$deviance,df.residual(g),lower=FALSE)

dp=sum(residuals(g, typp="pearson")^2)/df.residual(g); dp

# Residual deviance is about right for the corresponding degree of
# freedom, also p-value >> .05, which indicates no evidence of 
# lack of fit, that is the model fits the data well. 

### Part b: Plot the residuals and the fitted values why are there
### lines of observations on the plot?

# note: predict function returns values in untransformed original 
# scale of the response variable, while g$fit returns value in the 
# transformed scale. For poisson, exp(predict(g))=g$fit
# g$residuals are computed using the untransformed scale of
# the response, that is g$residuals=dvisits$doctorco-predict(g)

par(mfrow=c(1,2))
plot(g$fit,residuals(g),xlab="Fitted values in the scale of the response",
ylab="Deviance Residuals")

# find the number of unique responses

unique.resp.n=table(dvisits$doctorco);unique.resp.n
unique.resp.id=as.numeric(names(unique.resp.n));unique.resp.id
n=length(unique.resp.id)

# set plotting area
plot(g$fit,residuals(g),xlab="Fitted values in the scale of the response",
ylab="Deviance Residuals", type="n") 
legend("topright", col=(unique.resp.id+1), 
pch=(unique.resp.id+1), legend=unique.resp.id)

# plot with different color for each unique response value
for(i in 1:n) {
ID=(dvisits$doctorco==unique.resp.id[i])
points(g$fit[ID],residuals(g)[ID], col=i, pch=i)
}

# There are 10 lines of observation, each line corresponding to a 
# unique value (0,1,...10) of the response variable. 
# Since residuals invovles a difference in some form between 
# the fitted values and actual values, which are discrete in this
# case,  then for a unique value of the response (eg. 0), 
# the residual formula will follow the same curve since the respose
# value is just "scaled" by the fitted value. Thus, 
# for different unique values of the response, the residuals 
# will follow a different curve because the actual value is 
# diffrent for each unique value of the response. 
# So for example, for a fitted value of 1, the residuals for
# each unique value of the response are different by 
# dvisits$doctorco, the unique value of the response.  

### Part c: Use backward elimination with a critical p-value of 5%
### to reduce the model as much as possible. Report your model.

# fit full model, and drop least significant variable
# use drop1 command (F-test more appropriate for dispersion)
# repeat with updated model until all variables significant at 5%

fit=g
F=drop1(fit,test="F")[-1,]
names=row.names(F)
#determine insignificant
test=F[,'Pr(F)']>.05
#return insignificant
notsig=F[test,'Pr(F)']
names=names[test]
#return the least significant position
id=order(notsig,decreasing=TRUE)[1] 
#return the variable name of the least significant effect
names[id] # drop agesq

fit=update(fit,.~. -agesq)
F=drop1(fit,test="F")[-1,]
names=row.names(F)
test=F[,'Pr(F)']>.05
notsig=F[test,'Pr(F)']
names=names[test]
id=order(notsig,decreasing=TRUE)[1] 
names[id] # drop freerepa

fit=update(fit,.~. -freerepa)
F=drop1(fit,test="F")[-1,]
names=row.names(F)
test=F[,'Pr(F)']>.05
notsig=F[test,'Pr(F)']
names=names[test]
id=order(notsig,decreasing=TRUE)[1] 
names[id] # drop levyplus

fit=update(fit,.~. -levyplus)
F=drop1(fit,test="F")[-1,]
names=row.names(F)
test=F[,'Pr(F)']>.05
notsig=F[test,'Pr(F)']
names=names[test]
id=order(notsig,decreasing=TRUE)[1] 
names[id] # cannot drop any more variables

g1=fit
summary(g1)
row.names(summary(g1)$coeff)

# For the response variable doctorco, the model includes
# the intercept and predictors: sex,age,income,freepoor,
# illness, actdays, hscore, chcond1, chcond2

### Part d: What sort of person would be predicted to visit the 
### doctor the most under your selected model?

# A person with a profile such that it will increase the 
# response, would be predicted to vist the docter more. 
# In other words, larger values of the variables for positive 
# coefficients and smaller values of the variables for
# negative coefficients. Thus, this would be a person with 
# the following characteristics:
# Sex: Female (since F = 1, and M=1)
# Age: Older
# Income: low - less than 200 
# Freepoor: not covered by goverment because of low income,
#           recent immigrant or unemployed 
# Illness: with 5 or more illnesses in past 2 weeks
# Actdays: Higher number of days of reduced activity 
# 	     in past two weeks due to illness or injury
# Hscore: high score on General health questionnaire score 
#         using Goldberg's method (high score = bad health)
# Chcond1: with chronic conditions(s) but not limited in activity
# Chcond2: with chronic condition(s) and limited in activity

### Part e: For the last person in the dataset, compute the 
### predicted probability distribution
### i.e., give the probability they visit 0,1,2, etc. times.

# Find the last persion
personID = dim(dvisits)[1];personID

# Retrieve profile
person=dvisits[personID,]

# Find the predicted number of visits to the doctor
prediction=predict(g1, person); prediction
# This is the expected log count of viists to the doctor
lambda = round(exp(as.numeric(prediction)),3);lambda
# This is the exptected count of visits to the doctor

# Probabilty distribution: Poisson with mean lambda=0.156
# lambda^k/k*exp(-lambda)

k=0:4
p=round(dpois(k, lambda),4)
p=cbind(p)
row.names(p)=k

#probabilities for number of visits
p
