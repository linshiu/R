#### Homework 3
#### From the text-book: Exercise 3.14, 5.6, 5.9, 5.10
#### From my book: Problem 3.4 and  Problem 3.5 

#### (The missing reference (??) in the exercise is to 
#### Problem 2.5 from Chapter 2.In problem 3.4(d) of my
#### book, "YrsEm" should be replaced by "PriorYr".
#### Marketing should be Purchase in Table 3.15.

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

#### 3.4 ##############################################

# Import data
filename = "Employee+Salaries.csv"
mydata = read.csv(filename,header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

# Change column name for log salary

colnames(mydata)[colnames(mydata)=="log.Salary."]="log_salary"

### Part a

fit1 = lm(log_salary ~ YrsEm + PriorYr + Education + Super + Female + 
                      Advertising + Engineering + Sales, data =mydata)
summary(fit1)

# This fitted equation matches the one given in the book

### Part b

fit2 = lm(log_salary ~ YrsEm + PriorYr + Education + Super + Male + 
           Advertising + Engineering + Marketing, data =mydata)
summary(fit2)

# The new coefficiens for Male, Advertisting, Engineering and Marketing
# are highlighted after using Female and Sales as base categories
# As expected for engineering: 0.0937783-0.0057292 = 0.0880491
# And male: (-0.0230683)= -0.0230683

### Part c

# For model in part a), the coefficient of engineering is the effect 
# of engineering on the salary compared to marketing after accounting 
# for other predictors. So the difference in salary between engineering 
# and marketing is not significant.
# For model in part b), the coefficient of engineering is the effect
# of engineering on the salary compared to sales after accounting
# for other predictors. So the difference in salary between engineering
# and sales is significant. 

# average difference between group coded 1 and group coded 0

mean(mydata$log_salary[mydata$Engineering==1])-mean(mydata$log_salary[mydata$Marketing==1])
mean(mydata$log_salary[mydata$Advertising==1])-mean(mydata$log_salary[mydata$Marketing==1])
mean(mydata$log_salary[mydata$Sales==1])-mean(mydata$log_salary[mydata$Marketing==1])
mean(mydata$log_salary[mydata$Female==1])-mean(mydata$log_salary[mydata$Male==1])

# If the coefficient of a dummy variable is nonsignificant, it tells 
# you that the effect of the dummary variable on the response compared 
# to the base category after accounting for other predictors is not significant. 
# Then you can combine nonsignificant to the base category. 

### Part d

fit3 = lm(log_salary ~ YrsEm  + Education  + 
           Advertising + Engineering + Sales, data =mydata)
summary(fit3)

anova(fit1,fit3)
# THe F-test to compare the full model vs. reduced model shows that the null
# hypothesis that PriorYr, Super and Female are zero cannot be rejected, so conclude
# that the coefficients are not significantly different than zero. Thus, 
# the reduced model is preferred (the variables in the reduced model adequately 
# explain the variation as the variables in the full model)

# Fitting the new model the coefficients of YrsEm, Education and Sales remain
# significant, and the coefficients of Advertising and Engineering
# remain insignificant at 0.05 level. 
# The R^2 remains about the same at 0.86. This tells us that Female, Super 
# and PriorYr were not contributing much to explaining the variation in salary.
# Thus, the model after dropping the variables seems to be preferred. 

# Because Advertisting and Engineering are insignificant compared to 
# the base (Marketing), then it might be a good idea to combine 
# Advertising, Engineering and Marketing into one "Other" category. 
# So you would need only 1 dummy variables representing Sales vs. "Other". 
# would represent Sales vs. 


#### 3.14 ##############################################

((38460756-22657938)/(91-88))/(22657938/88)
qf(.05,df1=3,df2=88,lower.tail = FALSE)

#### 5.6 ####

## Part a

# r = sqrt(SSR/SST) = sqrt(SSR/(SSR + SSE))
SSR = 4604.7
SSE = 1604.44

r = sqrt(SSR/(SSR+SSE))
r

## Part b

# The estimated price of an American car with a 100 hp engine is: (answers in thousands) 

# Using Model 1
-6.107 + 0.169*100

# Using Model 2
-4.117 + 0.174*100 -3.162*1

# Using Model 3
-10.882 + 0.237*100 +2.076*1 - 0.052*100*1

## Part c

# Model 2

# With respect to "Others", Japan has the the lowest price compared to Germany and USA
# because the coefficient is the most negative.The negative coefficient means
# that the price of Japan is lower than "Others"
# Thus, least expensive car is from Japan.

# Model 3

# Cannot hold horsepower constant because there is an interaction
# between country and horsepower, so the least expensive car depends
# on the horsepower (we cannot estimate the country effect independent of HP)

# For example, if HP = 100

# Others
-10.882 + 0.237*100 + 2.076*0 + 4.755*0 + 11.774*0 -0.052*0*100 - 0.077*0*100  - 0.095*0*100 
# USA
-10.882 + 0.237*100 + 2.076*1 + 4.755*0 + 11.774*0 -0.052*1*100  - 0.077*0*100  - 0.095*0*100 
# Japan
-10.882 + 0.237*100 + 2.076*0 + 4.755*1 + 11.774*0 -0.052*0*100  - 0.077*1*100  - 0.095*0*100 
# Germany
-10.882 + 0.237*100 + 2.076*0 + 4.755*0 + 11.774*1 -0.052*0*100  - 0.077*0*100  - 0.095*1*100 
# So least expensive is USA, followed by  Japan, "Others", and Germany

# For example, if HP = 1000

# Others
-10.882 + 0.237*1000 + 2.076*0 + 4.755*0 + 11.774*0 -0.052*0*100 - 0.077*0*1000  - 0.095*0*1000 
# USA
-10.882 + 0.237*1000 + 2.076*1 + 4.755*0 + 11.774*0 -0.052*1*100  - 0.077*0*1000  - 0.095*0*1000 
# Japan
-10.882 + 0.237*1000 + 2.076*0 + 4.755*1 + 11.774*0 -0.052*0*100  - 0.077*1*1000  - 0.095*0*1000 
# Germany
-10.882 + 0.237*1000 + 2.076*0 + 4.755*0 + 11.774*1 -0.052*0*100  - 0.077*0*1000  - 0.095*1*1000 
# So least expensive is Germany, followed by Japan, USA and "Others"


## Part d

# Compare model 3 vs model 2
# Ho: Reduced model (2) is adequate
# H1: Full model (3) is adequate

# Alternatively,
# Ho: HP*USA = HP*Japan = HP*Germany = 0
# H1: At least one of HP*USA, HP*Japan or HP*Germany is different than zero

# F = ((RSS_reduced - RSS_full)/(df_reduced-df_full))/(RSS_full/df_full)

((1390.31-1319.85)/(85-82))/(1319.85/82)
qf(.05,df1=3,df2=82,lower.tail = FALSE)

# Since F_stat = 1.459 < F_crit = 2.716,  the null hypothesis that 
# the interaction terms are zero cannot be rejected, so conclude that the cooefficients 
# are not significantly different than zero. Thus, the reduced model is preferred 
# (the variables in the reduced model adequately explain the variation as the 
# variables in the full model). Conclusion: there is not a significant interaction
# between country and HP.

## Part e

# Compare model 2 vs model 1
# Ho: Reduced model (1) is adequate
# H1: Full model (2) is adequate

# Alternatively,
# Ho: USA = Japan = Germany = 0
# H1: At least one of USA, Japan or Germany is different than zero

# F = ((RSS_reduced - RSS_full)/(df_reduced-df_full))/(RSS_full/df_full)

((1604.44-1390.31)/(88-85))/(1390.31/85)
qf(.05,df1=3,df2=85,lower.tail = FALSE)

# Since F_stat = 4.364 > F_crit = 2.712,  reject the null hypothesis that 
# the interaction terms are zero, so conclude that the cooefficients 
# are  significantly different than zero. Thus, the full model is preferred 
# (the variables in the reduced model do not adequately explain the variation as the 
# variables in the full model). Conclusion: given HP, Country is a a signficant
# predictor of car price

## Part f

# Because the p-value > 0.05 for Germany in model 2, the null hypothesis that this
# coefficient is zero cannot be rejected, so conclude that is not significantly 
# different than zero. Thus, there is not a signficant difference of prices 
# between German and Others car, so it is recommended to add Germans to the
# "Other" category. 

## Part g

# Ho: USA = Japan
# H1: USA diff Japan

# One could fit a model by replacing USA = Japan (called reduced model) and compared
# it to the full model (model 2) by using a F-test that will reject or not the
# null hyphothesis.

# Alternatively, you can use a t-test:  t=(USA-Japan)/se(USA-Japan)
# where  se (b1 - b2)= sqrt(var b1 + var b2 - 2cov (b1,b2))
# and df = n-(p+1) = 85


#### 5.9 ##############################################

# Import data
filename = "P160.txt"
mydata = read.table(filename,header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

## Part a

fit0 = lm(V~I + D + W + G:I + P + N, data = mydata)
summary(fit0)


## Part d

fit1 = lm(V~I + D + W + G:I + P + N + D*I + P*I + N*I + N*D + P*D, data = mydata)
summary(fit1)

fit = update(fit1,.~.-I:P)
summary(fit)

fit = update(fit,.~.-P)
summary(fit)

fit = update(fit,.~.-W)
summary(fit)

fit = update(fit,.~.-I:D)
summary(fit)

fit = update(fit,.~.-D:N)
summary(fit)

fit = update(fit,.~.-D:P)
summary(fit)

plot(fit)


step(fit1, direction = "backward")
summary(lm(formula = V ~ I + D + P + N + I:G + I:N + D:P, data = mydata))


#### 5.10 ##############################################

# Import data
filename = "P160.txt"
mydata = read.table(filename,header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

mydata$D1 = (mydata$D==1)*1
mydata$D2 = (mydata$D==-1)*1

fit = lm(V~ I + D1 + D2 + W + G:I + P + N, data = mydata)
summary(fit)

## Part c
# D1 = - D2
# D1 + D2 = 0
linearHypothesis(fit,"D1 + D2 =0 ")
anova(fit0,fit)


