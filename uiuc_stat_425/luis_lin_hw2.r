##############################
## STAT 425 - Homework #2   ##
## Date: 9/20/11            ##
## Author: Luis Lin         ##
##############################

######## QUESTION 2 ##########

# part a.

air=c(24, 31, 32, 39, 47, 47, 35, 76, 95, 85);  # counts from airplane
heli=c(30, 30, 33, 38, 58, 58, 48, 75, 85, 55); # counts from helicopter
lm1=lm(air~heli)
plot(heli, air, main="Manatee Counts", xlab="From Helicopter", ylab="From Airplane")
abline(lm1, lwd=2)

# part b.

summary(lm1)

RSS=sum(lm1$res^2)
SSx=sum((heli-mean(heli))^2)
df=8
SE_beta1=sqrt(RSS/df/SSx)
beta1=lm1$coeff[2]
t_stat=beta1/SE_beta1
t_crit=qt(0.975,df)
t_stat
t_crit
t_stat>t_crit

g=lm(air~offset(heli));   #set coefficient of helicopter variable to 1
anova(g,lm1)

t_stat1=(beta1-1)/SE_beta1
t_stat1
t_crit
t_stat1>t_crit

# part c.

lm1=lm(air~heli)
new=data.frame(heli=seq(28, 88, 0.5));
pred.PI = predict(lm1, new, level=0.95, interval="prediction")
pred.CI = predict(lm1, new, level=0.95, interval="confidence")

n=10;
x.mean=mean(heli); sxx=sum((heli-x.mean)^2);
half.width=summary(lm1)$sigma*sqrt(2*qf(0.95, 2, n-2))*sqrt(1/n+(new$heli-x.mean)^2/sxx);
band.CI=cbind(pred.CI[,1]-half.width, pred.CI[,1]+half.width)

plot(c(28, 88), range(air, pred.PI, pred.CI, band.CI), type="n") 
# set the plotting region, but don't plot any points.
points(heli, air, xlab="Manatee Counts from Helicopter", ylab="From Airplane")
abline(lm1)
lines(new$heli, pred.PI[,2], lty=1, col="red")
lines(new$heli, pred.PI[,3], lty=1, col="red")
lines(new$heli, pred.CI[,2], lty=2, col="blue")
lines(new$heli, pred.CI[,3], lty=2, col="blue")
lines(new$heli, band.CI[,1], lty=3, col="green");
lines(new$heli, band.CI[,2], lty=3, col="green");

# part d.


lm2=lm(air~heli-1)
plot(heli, air, main="Manatee Counts", xlab=" From Helicopter", ylab="From Airplane")
abline(lm2, lwd=2)
abline(0,1,lwd=2, lty=2, col="red")  
legend("topleft", lty=c(1,2), col=c("black", "red"), 
	 legend=c("Estimated regression line (through the origin)", "The line of Y=X"))

summary(lm2)
g2=lm(air~offset(heli)-1);   #set coefficient of helicopter variable to 1
anova(g2,lm2)

# Optional from Instructor's R code notes
new=data.frame(heli=seq(28, 88, 0.5));
pred.PI = predict(lm2, new, level=0.95, interval="prediction")
pred.CI = predict(lm2, new, level=0.95, interval="confidence")

plot(c(28, 88), range(air, pred.PI, pred.CI), type="n") 
# set the plotting region, but don't plot any points.
points(heli, air, xlab="Manatee Counts from Helicopter", ylab="From Airplane")
abline(lm2)
lines(new$heli, pred.PI[,2], lty=2, col="red")
lines(new$heli, pred.PI[,3], lty=2, col="red")
lines(new$heli, pred.CI[,2], lty=2, col="blue")
lines(new$heli, pred.CI[,3], lty=2, col="blue")

######## QUESTION 3 ##########

n=40; x=runif(n, -5,5); y=1+2*x+3*rnorm(n);
x.quan=quantile(x); # return the 5-number summary
id=(1:n)[x < x.quan[2]]; 
id=c(id, (1:n)[x > x.quan[4]]) 

# id: a subset of the n obs whose x-values are outside
# the 25% and 75% quantiles of x.
length(id)			# should equal 20=n/2	

myfit0=lm(y~x);					
myfit1=lm(y[id]~x[id])
myfit2=lm(y[-id]~x[-id])

summary(myfit0)
summary(myfit1)
summary(myfit2)

# produce 4 plots arranged in a 2x2 table
par(mfrow=c(2,2))
plot(x,y, xlab="", ylab="", main='myfit0');
abline(myfit0);

plot(x,y,xlab="", ylab="", type="n" ,main='myfit1');
points(x[id],y[id],col=2);
abline(myfit1, lty=2, col=2);

plot(x,y,xlab="", ylab="", type="n", main='myfit2');
points(x[-id],y[-id],col=3);
abline(myfit2, lty=3, col=3); 

plot(x,y, xlab="", ylab="",  main='all models');
abline(myfit0);
abline(myfit1, lty=2, col=2);
abline(myfit2, lty=3, col=3);

# comapre RSS and TSS

Beta0 = round(c(myfit0$coeff[1],myfit1$coeff[1],myfit2$coeff[1]),3)
Beta1 = round(c(myfit0$coeff[2],myfit1$coeff[2],myfit2$coeff[2]),3)
RSS = round(c(sum(myfit0$res^2),sum(myfit1$res^2),sum(myfit2$res^2)),3)
TSS = round(c(sum((y-mean(y))^2),sum((y[id]-mean(y[id]))^2),sum((y[-id]-mean(y[-id]))^2)),3)
Ave_RSS = round(c(RSS[1]/length(y),RSS[2]/length(id), RSS[3]/length(id)),3)
Ave_TSS = round(c(TSS[1]/length(y),TSS[2]/length(id), TSS[3]/length(id)),3)
RSS_TSS_ratio = round(RSS/TSS,3)
Rsq = round(1-RSS/TSS,3)
report=rbind(RSS,TSS,Ave_RSS,Ave_TSS,RSS_TSS_ratio,Rsq)
Beta0
Beta1
report



### Separate plots
plot(y~x, pch=16,xlab='x',ylab='y',main='myfit0');
abline(myfit0,lty=1, lwd=2, col='red')

plot(y[id]~x[id], pch=16,xlab='x[id]',ylab='y[id]',main='myfit1'); 
abline(myfit1,lty=1, lwd=2, col='blue')

plot(y[-id]~x[-id], pch=16,xlab='x[-id]',ylab='y[-id]',main='myfit2');
abline(myfit2,lty=1, lwd=2, col='green')



######## QUESTION 4 ##########

# Analyze the Berkeley Guidance Study data
# Data from the Berkeley guidance study of children born in 1928-29 in Berkeley, CA. 
# BGSall.txt contains all the data (70 girls and 66 boys)

# Variable description: 
# Sex: 0 = males, 1 = females
# WT2: Age 2 weight (kg)
# HT2: Age 2 height (cm)
# WT9: Age 9 weight (kg)
# HT9: Age 9 height (cm)
# LG9: Age 9 leg circumference (cm)
# ST9: Age 9 strength (higher value = stronger)
# WT18: Age 18 weight (kg)
# HT18: Age 18 height (cm)
# LG18: Age 18 leg circumference (cm)
# ST18: Age 18 strength (higher value = stronger)
# Soma: Somatotype, a seven-point scale, as a measure of fatness 
#       (1 = slender, 7 = fat), determined using a photograph 
#       taken at age 18

# Download BGSall.txt and make sure it's in your R's working directory.
BGS=read.table(file="BGSall.txt", header=T)
BGS  # display all the data

#part a

lm1.boy=lm(Soma~HT2 + WT2 + HT9 + WT9 , subset=(Sex==0), data=BGS)
summary(lm1.boy)
lm1.boy.rsquared=summary(lm1.boy)$r.squared
lm1.boy.coeff=lm1.boy$coeff
lm1.boy.sigma=summary(lm1.boy)$sigma
round(lm1.boy.rsquared,4)
round(lm1.boy.coeff,4)
round(lm1.boy.sigma,4)
cor(BGS$WT2[BGS$Sex==0],BGS$HT2[BGS$Sex==0])
cor(BGS$WT2[BGS$Sex==0],BGS$HT9[BGS$Sex==0])
cor(BGS$WT2[BGS$Sex==0],BGS$WT9[BGS$Sex==0])
cor(BGS$WT9[BGS$Sex==0],BGS$HT2[BGS$Sex==0])
cor(BGS$WT9[BGS$Sex==0],BGS$HT9[BGS$Sex==0])

#part b

lm2.boy=lm(Soma~HT2 + WT2 + I(HT9-HT2) + I(WT9-WT2) , subset=(Sex==0), data=BGS)
summary(lm2.boy)
lm2.boy.rsquared=summary(lm2.boy)$r.squared
lm2.boy.coeff=lm2.boy$coeff
lm2.boy.sigma=summary(lm2.boy)$sigma
round(lm2.boy.rsquared,4)
round(lm2.boy.coeff,4)
round(lm2.boy.sigma,4)

#part c
lm3.boy=lm(Soma~HT9 + WT9 , subset=(Sex==0), data=BGS)
summary(lm3.boy)
anova(lm3.boy,lm1.boy)

#part d
lm1.girl=lm(Soma~HT2 + WT2 + HT9 + WT9, subset=(Sex==1), data=BGS)
summary(lm1.girl)
lm1.girl.rsquared=summary(lm1.girl)$r.squared
lm1.girl.coeff=lm1.girl$coeff
lm1.girl.sigma=summary(lm1.girl)$sigma
round(lm1.girl.rsquared,4)
round(lm1.girl.coeff,4)
round(lm1.girl.sigma,4)
cor(BGS$WT2[BGS$Sex==1],BGS$HT2[BGS$Sex==1])
cor(BGS$WT2[BGS$Sex==1],BGS$HT9[BGS$Sex==1])
cor(BGS$WT2[BGS$Sex==1],BGS$WT9[BGS$Sex==1])
cor(BGS$WT9[BGS$Sex==1],BGS$HT2[BGS$Sex==1])
cor(BGS$WT9[BGS$Sex==1],BGS$HT9[BGS$Sex==1])

#part e
lm3.girl=lm(Soma~HT9 + WT9, subset=(Sex==1), data=BGS)
summary(lm3.girl)
anova(lm3.girl,lm1.girl)



