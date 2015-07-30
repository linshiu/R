#########################
# STAT 425
# Homework 1
# Date: 9/9/11
# Author: Luis Lin

#########################
#Install Package

install.packages("faraway", lib="E:/R")
library("faraway",lib.loc="E:/R")

#########################
#Problem 4

library(faraway)
help(teengamb)
attach(teengamb)
names(teengamb)
dim(teengamb)
summary(teengamb)

plot(density(teengamb$gamble,na.rm=TRUE))
plot(gamble~income,teengamb)
plot(gamble~sex,teengamb)
plot(income~sex,teengamb)


#########################

#Problem 5

gamble.full.lm=lm(gamble~., data=teengamb)
summary(gamble.full.lm)
names(gamble.full.lm)

residual=gamble.full.lm$res
sort(residual)
order(residual) 

mean(residual)
median(residual)

cor(residual, gamble.full.lm$fit)
cor(residual, teengamb) 

gamble.full.lm$coeff

attach(teengamb)
x=data.frame(sex=0,status=mean(status),
             income=mean(income),verbal=mean(verbal))

predict.lm(gamble.full.lm,x,level=0.95,interval="prediction")

y=data.frame(sex=0,status=max(status),
             income=max(income),verbal=max(verbal))

predict.lm(gamble.full.lm,y,level=0.95,interval="prediction")

gamble.income.lm=lm(gamble~income, data=teengamb)
summary(gamble.income.lm)
anova(gamble.income.lm, gamble.full.lm)

detach(teengamb)

##########################

#Problem 6

help(prostate)
attach(prostate)

#Question a and b
model1=lm(lpsa~lcavol, data=prostate)
summary(model1)

#Question c
#Note: this is an exact copy of the instructor's code since I still not familiar 
#with most functions and syntax in R

# sequentially add the variables in "varlist"
# record the corresponding standard error and R-square
varlist=c("lcavol", "lweight", "svi", "lbph", "age", "lcp", "pgg45", "gleason");
m=length(varlist)
mysd=rep(0, m); myR2=rep(0, m);
mymodel='lpsa ~ 1';
for(i in 1:m){
	mymodel=paste(mymodel,'+',varlist[i])
	tmp.lm=lm(mymodel, data=prostate)
	mysd[i]=summary(tmp.lm)$sigma;
	myR2[i]=summary(tmp.lm)$r.squared;
}
par(mfrow=c(1,2))
plot(1:m, myR2, ylab="R-square");
plot(1:m, mysd, ylab="Residual Standard Errors")


#Question d and e
#Note: this is an exact copy of the instructor's code since I still not familiar 
#with most functions and syntax in R
prostate.lm1=lm(lpsa~lcavol, data=prostate);
prostate.lm2=lm(lcavol~lpsa, data=prostate);

plot(lcavol, lpsa, ylab="lpsa", xlab="lcavol");
title("Scatter Plot of the Prostate Data")
abline(prostate.lm1, col="red", lty=1, lwd=2)

a=coef(prostate.lm2); yrange=range(lpsa);
lines((yrange*a[2]+a[1]), yrange, col="blue", lty=2, lwd=2)
legend("topleft", legend=c("fitted line from `lpsa~lcavol'", "fitted line from `lcavol~lpsa'"), col=c("red", "blue"), lty=c(1,2))

points(mean(lcavol), mean(lpsa), pch=2, col="green")

mean(lcavol)
predict(prostate.lm1,data.frame(lcavol=mean(lcavol)))
mean(lpsa)
predict(prostate.lm2,data.frame(lpsa=mean(lpsa)))

detach(prostate)


##########################

#Problem 7

help(sat)
attach(sat)

sat.lmodel1=lm(total~expend+ratio+salary, data=sat)
summary(sat.lmodel1)
anova(sat.lmodel1)# optional

sat.lmodel2=lm(total~expend+ratio+salary+takers, data=sat)
summary(sat.lmodel2)
anova(sat.lmodel1, sat.lmodel2)

se.takers=0.2313
coeff.takers=sat.lmodel2$coef[5]
t=coeff.takers/se.takers
tsquared=t^2

rss.sat.lmodel1=sum(sat.lmodel1$res^2)
rss.sat.lmodel2=sum(sat.lmodel2$res^2)
Fstat=(rss.sat.lmodel1-rss.sat.lmodel2)/(rss.sat.lmodel2/45)

tsquared
Fstat

detach(sat)

