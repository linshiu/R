#### Homework 1
#### Text-Book Problems: 2.10, 2.12
#### My Book Problems: 2.1, 2.2, 2.3, 2.4

#### Setup ##############################################

# Install packages if needed
# install.packages("ggplot2")
# install.packages("grid")
# install.packages("gridExtra")
# install.packages("XLConnect")
# install.packages("corrplot")
# install.packages("Hmisc")


# Load packages
library(ggplot2)
library(grid)
library(gridExtra)
library(XLConnect)
library(corrplot)
library(Hmisc)

# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_401_Statistical Methods for Data Mining"
datafolder = "Data"
setwd(file.path(main,course, datafolder))
  
#### Problem 1 - 2.10 ##############################################

# Import data
filename = "P052.txt"
mydata = read.table(filename, sep="\t",header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Part a 

# Compute covariance
cov(mydata$Husband, mydata$Wife)

## Part b 

# Convert to inches
mydata_inches = mydata/2.54
head(mydata_inches)

# Compute covaraince
cov(mydata_inches$Husband, mydata_inches$Wife)

## Part c

# Compute correlation coefficinet
cor(mydata$Husband, mydata$Wife)

## Part d

# Compute correlation coefficient
cor(mydata_inches$Husband, mydata_inches$Wife)

## Part e

# Change wife heights to 5 cm less than husband's
mydata_5short = mydata
mydata_5short$Wife = mydata$Husband -5
head(mydata_5short)

# Compute correlation coefficinet
cor(mydata_5short$Husband, mydata_5short$Wife)

## Part f

# Ans: Either variable can be used as the response variable in this case 
#      because we are trying to fit a model that relates heights and not looking for predicting
#      anything in particular
#      Let X = height of husband (predicotr), Y = height of wife (reponse) for this model

## Part g

fit1 = lm(Wife ~ Husband, data = mydata)

ggplot(mydata,aes(x=Husband, y = Wife)) + geom_point(size = 3) + 
  stat_smooth(method = 'lm', se= FALSE)

summary(fit1)

summary(fit1)$coef["Husband","Pr(>|t|)"]

# Ans: p-value < 0.05, reject null hypothesis that the slope is zero at 0.05 level

## Part h
summary(fit1)$coef["(Intercept)","Pr(>|t|)"]

# Ans: p-value < 0.05, reject null hypothesis that the intercept is zero at 0.05 level


#### Problem 2 - 2.12 #################################################
# Import data
filename = "P054.txt"
mydata = read.table(filename, sep="\t",header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Part a

plot1= ggplot(mydata,aes(x=Daily, y = Sunday)) + geom_point(size = 3) +
        xlab("Daily Circulation") + ylab("Sunday Circulation") +
        stat_smooth(method = 'lm', se= FALSE, formula=y~x)
        
plot1

# Ans: Yes the scatterplot suggests a strong linear relationship between Daily
#      and Sunday circulation. This makes sense since people that tend to read
#      the daily news would be interested in the news for Sunday

## Part b

fit1 = lm(Sunday~Daily,data=mydata)
summary(fit1)
 

## Part c

confint(fit1, level=0.95)

## Part d
summary(fit1)$coef["Daily","Pr(>|t|)"]

# Ans: p-value < 0.05, reject null hypothesis that the slope is zero at 0.05 level
#      The conclusion for the test of the slope indicates a strong positive linear 
#      relationship between sunday and daily circulation. Or in other words, daily circulation
#      is a statsically significant predictor of sunday circulation.
#      Alternatively,the same conclusion is reached since the 95% CI for the slope does not include zero. 

## Part e
summary(fit1)$r.squared

# Ans: about 92% of the variability in Sunday circulation is accounted by daily circulation

## Part f

newdata = data.frame(Daily=500)
predict(fit1, newdata, interval="confidence",level=0.95 ) 

## Part g
p_500 = predict(fit1, newdata, interval="prediction",level=0.95 ) 
p_500


# Ans: The interval in (f) is confidence interval of the mean Sunday circulation for 
#      a daily circulation of 500K, while the interval in (g) is a prediction interval
#      of a point-estimate or next observation of a Sunday circulation for a daily circulation of 500K.
#      The interval in (g) is therefore wider because accounts for the mean uncertainty in the mean
#      in addition to the scatter. 

## Part h
newdata = data.frame(Daily=2000)
p_2000 = predict(fit1, newdata, interval="prediction",level=0.95 ) 
p_2000
summary(mydata$Daily)

((p_2000[,"upr"]-p_2000[,"lwr"])/(p_500[,"upr"]-p_500[,"lwr"])-1)*100

# Ans: This interval is much wider (~41% wider) than g scince is further away from 
#      the center of observations.
#      It is unlikely to be accurate because a daily circulation of 2,000,000  is outside 
#      the range of observation (max is 1209).

#### Problem 3 - 2.1 ############################################

prime = data.frame(x=rep(10,10)^(1:10),
                   y=c(4,25,168,1229,9592,78498,664579,5671455,50847534,455052512))

prime$p = prime$y/prime$x
prime

## Part a

plot1 = 
  ggplot(prime,aes(x=x, y = p)) + 
  geom_point(size = 3)

plot2 =
  ggplot(prime,aes(x=1/log10(x), y = p)) + 
  geom_point(size = 3)  +
  stat_smooth(method = 'lm', se= FALSE, formula=y~x)

grid.arrange(plot1,plot2,ncol=2, main = "Proportion vs. x and transformations")

prime$x_transf = 1/log10(prime$x)
prime$x_transf2 = 1/log(prime$x)

# Ans: Linearizing transformation: x= 1/log10(x)

## Part b
fit1 = lm(p~x_transf,data=prime)

summary(fit1)
conf_b = confint(fit1, level=0.95)


# My model: p(x) = b*1/log_10(x) + a, a~0
# Theory: p(x) = 1/log_e(x)
# Express Theory in terms of log_10(x) -> using identity: log_e(x)= log_10(x)/log_10(e)
# so p(x) = log_10(e)*1/log_10(x)

b_theory = log10(exp(1))

conf_b
b_theory

# Note that both the intercept = 0 and theoretical slope = 0.434 fall inside their respective confidence intervals,
# so one cannot reject the null that intercept = 0 and slope = 0.43. This implies that the empirical model
# matches the theoretical model
# 
# Note: alternatively, one can fit same model but use natural log of x, and in that case the slope will
# be compared to the theoretical slope = 1 (since p(x)=1*1/log(x)), reaching the same conclusion

fit2 = lm(p~x_transf2,data=prime)

summary(fit2)
conf_b = confint(fit2, level=0.95)
conf_b


#### Problem 4 - 2.2 ############################################

# Import data
filename = "IBM_Apple_SP500.csv"
mydata = read.csv(filename,header = T, stringsAsFactors = F)

# Change name for SP500
colnames(mydata)[2] = "SP500"

# Convert % to numeric
for (i in 2:4){
  mydata[i] = as.numeric(sub("%", "", mydata[[i]]))/100
}

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Part a
plot1= 
  ggplot(mydata,aes(x=SP500, y = IBM)) + 
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se= FALSE, formula=y~x)

plot2= 
  ggplot(mydata,aes(x=SP500, y = Apple)) + 
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se= FALSE, formula=y~x)

grid.arrange(plot1,plot2,ncol=2, main = "Rate of return IBM, Apple vs S&P 500")

# Ans: For both IBM and Apple, there seems to be a strong linear relationship betwen their rate
#      of return and that of the SP500. The scatter plot look very similar, so it is not clear
#      from the plot whether IBM or Apple has a stronger linear linear relationship. There seems
#      to be a little bit more variability in Apple's data. 

## Part b
lm.IBM = lm(IBM~SP500,mydata)
lm.Apple = lm(Apple~SP500,mydata)
summary(lm.IBM)
summary(lm.Apple)

lm.Apple$coeff["SP500"]/lm.IBM$coeff["SP500"]

# ANS: The magintue of beta(Apple) is about 67% higher than that of beta(IBM), suggesting 
#      Apple had a higher expected return relative to S&P 500 compared to IBM (for the same change
#      in the S&P 500 rate of return, Apple had on average a larger change in its rate of return
#      compared to IBM)

## Part c

cor_matrix = cor(mydata[2:4])
cor_matrix 

cor_coeff = cor_matrix[1,2:3]
cor_coeff

sd = apply(mydata[,2:ncol(mydata)], 2, function(x) sd(x))
sd

beta = cor_coeff*sd[2:3]/sd[1]
beta

beta.lm= c(lm.IBM$coeff['SP500'],lm.Apple$coeff['SP500'])
names(beta.lm) = names(beta)
beta.lm

# ANS: beta is the coefficient of the regression where ite symbolizes the ratio of the return
#      on the stock (APPLE and IBM) to return on benchmark stock (S&P 500). So a larger slope
#      menas a higher expected return. Beta is also proportional to the ratio of the 
#      standard deviation of the stock to standard devation of the benchmark.Thus, a larger
#      ratio, which implies higher volatility of the stock with respect to the benchmark,
#      translates into a higher expected return. So a higher expected return is riskier and 
#      accompanied by higher volatility. 
#      In this case both Apple and IBM have similar correlations with the S&P 500, but Apple
#      has much more variablity (almost twice the sd) than IBM, which is reflected in a larger
#      beta of Apple vs. IBM. 

#### Problem 5 - 2.3 ############################################

# Import data
filename = "beef.csv"
mydata = read.csv(filename,header = T, stringsAsFactors = F)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Estimate price elasticities


# Power law for demand-price relationship in economics
# y = demand , x = price
# y = a*x^b -> ln(y) = ln(a)+b*ln(x)
# 100b = perecentage change in demand due to 1% change in price = price elasticity
# b<0 means price increaes, demand decreases.

vars = list(chuck=names(mydata)[grepl("chuck", names(mydata))],
            porter=names(mydata)[grepl("porter", names(mydata))],
            rib=names(mydata)[grepl("rib", names(mydata))])
                     
models.lm  = lapply(vars, function(x) {
  lm(substitute(log(j) ~ log(i), list(i = as.name(x[2]),j = as.name(x[1]))), data = mydata)})

summary.lm = lapply(models.lm, summary)
summary.lm

coeff = c(summary.lm$chuck$coeff["log(chuck_price)","Estimate"],
          summary.lm$porter$coeff["log(porter_price)","Estimate"],
          summary.lm$rib$coeff["log(rib_price)","Estimate"])

names(coeff)= names(summary.lm)

# Price elasticities estimates
coeff 

# ANS: Order in terms of price/quality: Porter > Rib > Chuck
#      Order in terms of magnitude of elasticity: Porter > Rib > Chuck
#      The order makes sense since the higher the price the more elastic since
#      consumers are more price sensitive for expensive items and are willing to give them up more
#      readily when prices rise compared to items that are a necessity. Also for the same percent
#      change in price, the absolute change in price for more expensive products is greater, so
#      it is expected to have higher impact on the demand. 
#      As expected, the sign is negative, indicating an increase in price
#      is expected to have a reduction in demand (law of demand)
#      All coefficients have magnitude > 1, suggesting a highly elastic demand,
#      which makes sense since steak is not considered a necessity. 
#
#      100b = perecentage change in demand due to 1% change in price = price elasticity

#      price elasticity higher for cheaper items?
#      rib eye more expensive so order doesn;t make sense
#      multiply by 100 or 10

# % change if increase 10% in price
coeff*10

#     # A 10% increase in price would result in 13.7%, 25.7% and 14.5% reduction in demand
#     for chuck, porer and rib cuts respectively


#### Problem 6 - 2.4 ############################################

# Import data
filename = "Smoking-Cancer Data.xlsx"
mydata = readWorksheet(loadWorkbook(filename),sheet=1)
names(mydata) = c("state","smoked","bladder","lung","kidney","leukemia")

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Scatter plots

plot1 = 
  ggplot(mydata,aes(x=smoked, y = bladder)) + 
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se= FALSE)

plot2 =
  ggplot(mydata,aes(x=smoked, y = lung)) + 
  geom_point(size = 3) +
  stat_smooth(method = 'lm', se= FALSE) 
              
plot3 =
  ggplot(mydata,aes(x=smoked, y = kidney)) + 
  geom_point(size = 3)  +
  stat_smooth(method = 'lm', se= FALSE)

plot4 =
  ggplot(mydata,aes(x=smoked, y = leukemia)) + 
  geom_point(size = 3)  +
  stat_smooth(method = 'lm', se= FALSE)

grid.arrange(plot1,plot2,plot3,plot4,ncol=2,
  main = "number of deaths due to each type of cancer versus cigarettes smoked")

# bladder and lung look a little bit linear
# leukemia doesn't look linear
# kidney looks linear except for a few outliers


# outliers

boxplot(mydata$leukemia, main="xx")$out
boxplot(mydata$bladder, main="xx")$out
boxplot(mydata$kidney, main="xx")$out
boxplot(mydata$lung, main="xx")$out


## Correlation Analysis
cor_p = cor(mydata[-1],method="pearson")
cor_s = cor(mydata[-1],method="spearman")
cor_p
cor_s 

corrplot(cor_p,method="number")
rcorr(as.matrix(mydata[-1]))