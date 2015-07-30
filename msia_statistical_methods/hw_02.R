#### Homework 2
#### Text-Book Problems: 3.12, 3.13, 3.15
#### My Book Problems: 3.1, 3.2, 3.3

#### Setup ##############################################

# Install packages if needed
# install.packages("ggplot2")
# install.packages("grid")
# install.packages("gridExtra")
# install.packages("XLConnect")
# install.packages("corrplot")
# install.packages("Hmisc")
# install.packages("car")

# Load packages
library(ggplot2)
library(grid)
library(gridExtra)
library(XLConnect)
library(corrplot)
library(Hmisc)
library(car)


# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_401_Statistical Methods for Data Mining"
datafolder = "Data"
setwd(file.path(main,course, datafolder))

#### Problem 1 - 3.1 ##############################################

# Verify 

x = c(1,2,3,4,5)
y = c(2,6,7,9,10)


X = cbind(rep(1,5),x)
t(X)%*%X
solve(t(X)%*%X)
t(X)%*%y

solve(t(X)%*%X)%*%t(X)%*%y

fit = lm(y~x)
summary(fit)

#### Problem 2 - 3.2 ##############################################

x1 = c(-1,-1,1,1)
x2 = c(1,-1,1,-1)
X = cbind(rep(1,4),x1,x2,x1*x2)
colnames(X)[4]="x1x2"
y = c(110,120,130,150)

## Part a)

X

## Part b)

# X'X
t(X)%*%X

# inv(X'X)
solve(t(X)%*%X)

# beta
solve(t(X)%*%X)%*%t(X)%*%y

# check
fit = lm(y~ x1 + x2 +x1*x2)
summary(fit)

# The inverse is easy to calculate because X'X is a diagonal matrix as a result of
# orthogonality, so the inverse is a diagonal matrix with the inverse (e.g 1/4 in this case) of 
# its corresponding entries.

## Part c)

# Reference: http://www.ats.ucla.edu/stat/mult_pkg/faq/general/effect.htm

# The equation is y = 127.5 + 12.5x1 -7.5x2 -2.5x1x2
# The intercept represents the overall mean of BP. Ther overall mean is 127.5

# check intercept
mean(y)

# With (-1,0,+1) coding, the coefficients represent the distance between the factor levels and the overall mean. Thus, the
# coefficient b1 of x1 represents the effect of x1 (age) when x2 = 0 (gender).  Since x2 = 0 at the mean of the two 
# categories of x2, b1 is a main effect.  It's the effect of x1 at the mean value of x2. More specifically, the coefficients
# of each of the effect variables is equal to the difference between the mean of the group coded 1 and the grand mean. 
# So in this case, 12.5  represents the deviation of the BP mean of old patient (x1=1) from the overall BP mean.
# The mean for old patient is 12.5 higher than that of the overall mean (or mean of young patient is -12.5 lower than that of the overall mean)
# It can also be interpreted as half the average difference between old and young.

# check beta 1
mean(y[x1==1])-mean(y)
mean(y)-mean(y[x1==-1])
mean(y[x1==1]-y[x1==-1])/2


# The coefficient b2 of x2 represents the effect of x2 (gender) when x1 = 0 (age). Since x1 = 0 at the mean of the two 
# categories of x2, b2 is a main effect.  It's the effect of x2 at the mean value of x1. More specifically, the coefficients
# of each of the effect variables is equal to the difference between the mean of the group coded 1 and the grand mean. 
# So in this case, -7.5  represents the deviation of the BP mean of female from the overall BP mean.
# The mean for female patient is 7.5 lower than that of the overall mean (or mean of male patient
# is 7.5 higher than that of the overall mean). It can also be interpreted as half the average difference between female and male.

# check beta 2
mean(y[x2==1])-mean(y)
mean(y)-mean(y[x2==-1])
mean(y[x2==1]-y[x2==-1])/2

# The equation is y = 127.5 + 12.5x1 -7.5x2 -2.5x1x2

# The coefficient of x1*x2 captures the interaction beetween age and gender (like a crossproduct). One can think of 
# the interaction coefficient as an additional effect of age (or gender) depending on gender (age).
# For example, from the equation, when x1=1,x2=1 (old,female), the deviation from the overall mean 
# is now 12.5-7.5-2.5 = 2.5, where the intercation term had an effect of -2.5 in the deviation.
# But changing the gender of the old patient to male (x1=1,x2=-1), the 
# deviation from the overall mean is now 12.5+7.5+2.5 = 22.5. where the intercation term had an 
# an oppositive effect of +2.5 in the deviation.

# More specifically, the coefficient represents the average difference of the differenes between old-young within female
# and old-young within male (or difference of the differenes between female-male within old 
# and female-male within young)

# It is also half the differene between the age effect for males and the age effect for females. 

# check beta 3
((y[x1==1&x2==1]-y[x1==-1&x2==1])/2-(y[x1==1&x2==-1]-y[x1==-1&x2==-1])/2)/2
  
# beta(x1) > 0 implies that old patients tend to hava a higher BP on average than do young on average
# beta(x2) <0 implies that female patients tend to have a lower BP on average than do male
# beta(x3) <0 implies a negative interaction between age and gender, when older (highve level), female (high level) has the effect
# of reducing the BP, while male (low level) has the effect of increasing the BP. 


## Part d)

# df of error
n = length(y)
p = 3
df = n - (p+1)
df

## Part e)

# Assumption: ther might be typo in the book. Assume n = # patients per group, so 4*n total patients
# Thus, n=1 and 4*1 patients (and not n =4 as it says in first part)

# If the sample size is 4n, then the X'X matrix will be a diagonal matrix with its entries
# equal to (4n) and its inverse will be a diagonal matrix with its entries equal to 1/(4n).
# So the current matrix for n=1 will be multiplied by n, and the current inverse matrix for n = 1
# will be multiplied by 1/n for the case of n>0. 

# The X'y vector  will be equal to a coulmn vector with 4 entries, the first one equal to
# the sum of y's, the second equal to the the sum of BP from old patients minus
# the sum of BP from young patients, the thrid equal ot the sum of BP from female
# minus the sum of BP from male patients, and the fourth one the difference of the differenes 
# between old-young within female and old-young within male (or difference of the differenes
# between female-male within old and female-male within young)

# The error df will be 4n-(p+1) = 4n-(3+1) = 4n-4, where n = # patients per group

# check

x1 = append(x1,x1)
x2 = append(x2,x2)
X = cbind(rep(1,8),x1,x2,x1*x2)
y = c(110,120,130,150,110,120,130,150)

# X
X

# X'X
t(X)%*%X

# inv(X'X)
solve(t(X)%*%X)

# X'y
t(X)%*%y

sum(y)
sum(y[x1==1])-sum(y[x1==-1])
sum(y[x2==1])-sum(y[x2==-1])
(sum(y[x1==1&x2==1])-sum(y[x1==-1&x2==1]))-(sum(y[x1==1&x2==-1])-sum(y[x1==-1&x2==-1]))
(sum(y[x1==1&x2==1])-sum(y[x1==1&x2==-1]))-(sum(y[x1==-1&x2==1])-sum(y[x1==-1&x2==-1])) 

# beta
solve(t(X)%*%X)%*%t(X)%*%y

n = length(y)
p = 3
df = n - (p+1)
df


#### Problem 3 - 3.3 ##############################################

# Import data
filename = "Cobb-Douglas+production+function+data.csv"
mydata = read.csv(filename,header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Part a)
mydata= log(mydata)
mydata = mydata[,2:4]
colnames(mydata)=c("log_capital","log_labor","log_output")
fit = lm(log_output~log_capital+log_labor,mydata)
summary(fit)

plot1 = 
  ggplot(mydata,aes(x=log_capital, y = log_output)) + 
  geom_point(size = 3)

plot2 =
  ggplot(mydata,aes(x=log_labor, y = log_output)) + 
  geom_point(size = 3)

grid.arrange(plot1,plot2,ncol=2, main = "log(output) vs. log(capital) and log(labor)")


plot1 = 
  ggplot(mydata,aes(x=log_capital, y = fit$resid)) + 
  geom_point(size = 3)

plot2 =
  ggplot(mydata,aes(x=log_labor, y = fit$resid)) + 
  geom_point(size = 3)

grid.arrange(plot1,plot2,ncol=2, main = "resid vs. log(capital) and log(labor)")

#plot(fit)

ggplot(mydata,aes(x=fit$fitted, y = fit$resid)) + 
  geom_point(size = 3)

# Plotting log(output) versus log(capital) and log(labor) show that a strong linear relationship
# between log(output) and log(capital), and log(output) and log(labor)

# Looking at the p-values for the coefficients, all of them are <0.05, indicating that are signinficantly
# different than zero. The F-statistic all shows that jointly all coefficients are significantly different
# than zero. The R^2 is 0.83, which is a decent value for the data in which the model is accounting 
# 83% of the variation in the response. The correlation coefficeint sqrt(0.83)=0.91 shows also a strong 
# linear association between the response variable and predictors.

# The residual plots also show no pattern, indicating that the model assumptions seemed to be satisfied
# and fit seems to be good. 

# The positive coefficients show that increasing labor or capital leads to an increase in the firm's output

## Part b)

newdata = log(data.frame(log_capital=500,log_labor=200))
exp(predict(fit, newdata,interval="predict"))

# Alternatively, keep the data original, fit log in lm, and for input for predict use original (non-log transformed)
# but still need to exponentiate result. Note cannot use mydata$capital, etc for predict
# filename = "Cobb-Douglas+production+function+data.csv"
# mydata = read.csv(filename,header = T)
# 
# fit = lm(log(output)~log(capital)+log(labor),mydata)
# summary(fit)
# newdata = data.frame(capital=500,labor=200)
# exp(predict(fit, newdata,interval="predict"))


## Part c)
# beta 1 + beta2 =1 is one more constraint that makes the full model more restricted
# so use F test to compare the full model and reduced model to test the null hypothesis that beta 1 + beta2 =1 
# Start by substituting beta1 = 1- beta2 in full model: log(y) = b0 + (1-b2)*log(capital) + b2*log(labor) ->
# log(y)-log(capital) = b0 + b2(log(labor)-log(capital)). Fit a regression model and compare to full model.

mydata$log_output_capital = mydata$log_output-mydata$log_capital
mydata$log_labor_capital = mydata$log_labor-mydata$log_capital
head(mydata)

fit_reduced = lm(log_output_capital~log_labor_capital ,mydata)
summary(fit_reduced)

anova(fit)
df_full = anova(fit)["Residuals","Df"]
RSS_full = anova(fit)["Residuals","Sum Sq"]

anova(fit_reduced)
df_red = anova(fit_reduced)["Residuals","Df"]
RSS_red = anova(fit_reduced)["Residuals","Sum Sq"]

F_stat = ((RSS_red-RSS_full)/(df_red-df_full))/(RSS_full/(df_full))
F_crit = qf(.95,df1=df_red-df_full,df2=df_full)
F_stat
F_crit
F_stat > F_crit
pf(F_stat,df1=df_red-df_full,df2=df_full,lower.tail = FALSE)

# F statistic > Fcritical (or p-value < 0.05).
# Threrefore reject Null that beta1 + beta2 = 1, so ject constatn returns to scale for this data

# Or one can use built in function in R in package car:
library(car)
linearHypothesis(fit, "log_labor + log_capital = 1")


#### Problem 4 - 3.13 ##############################################

F_stat = (23665352/4)/(22657938/88)
F_stat

F_crit = qf(.95,df1=4,df2=88)
F_crit

beta = c(3526.4,722.5,90.02,1.2690,23.406)
x = c(1,1,12,10,15)
beta%*%x

beta = c(3526.4,722.5,90.02,1.2690,23.406)
x = c(1,0,12,10,15)
beta%*%x

#### Problem 4 - 3.15 ##############################################

# Import data
filename = "P088.txt"
mydata = read.table(filename,header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Part a

fit = lm(Sales~Age + HS + Income + Black + Female + Price,mydata)
summary(fit)

summary(fit)$coeff["Female","Pr(>|t|)"]

# p-value = 0.85 > 0.05, do not reject null hypothesis that the coefficient
# for females is zero. So female might not be needed in the model

## Part b

fit_reduced = lm(Sales~Age  + Income + Black + Price,mydata)
summary(fit_reduced)

anova(fit)
df_full = anova(fit)["Residuals","Df"]
RSS_full = anova(fit)["Residuals","Sum Sq"]

anova(fit_reduced)
df_red = anova(fit_reduced)["Residuals","Df"]
RSS_red = anova(fit_reduced)["Residuals","Sum Sq"]

F_stat = ((RSS_red-RSS_full)/(df_red-df_full))/(RSS_full/(df_full))
F_crit = qf(.95,df1=df_red-df_full,df2=df_full)
F_stat
F_crit
F_stat > F_crit
pf(F_stat,df1=df_red-df_full,df2=df_full,lower.tail = FALSE)

anova(fit,fit_reduced)

# p-value > 0.05, cannot reject null hypothesis that both coefficients
# HS and Female are equal to zero. So variables might not be needed in model.


## Part c
confint(fit, level=0.95)["Income",]

## Part d
fit = lm(Sales~Age + HS  + Black + Female + Price ,mydata)
summary(fit)
summary(fit)$r.squared

## Part e
fit = lm(Sales~Age +Income + Price ,mydata)
summary(fit)
summary(fit)$r.squared

## Part f

fit = lm(Sales~Income ,mydata)
summary(fit)
summary(fit)$r.squared
