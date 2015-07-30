#### Homework 5
#1. Exercise 9.3 from text book (only do parts (a) and (d)).
#2. Exercise 9.4 from text book (only do parts (a) and (b)).
#3. Exercise 9.5 from text book (only do part (a)).

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
library(MASS)


# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_401_Statistical Methods for Data Mining"
datafolder = "Data"
setwd(file.path(main,course, datafolder))

#### Problem 1 ##############################################
# Import data
filename = "P256.txt"
mydata = read.table(filename,header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

# Fix names
names(mydata)[c(11,12)]=c("X_10","X_11")

#### Part a

# Plot separate

corr = round(cor(mydata[-1]),2)

corrplot(corr,method="number", type="upper")

pairs(mydata[,-1], main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, lower.panel = NULL, panel = panel.smooth, cex.labels=2)

# Alternative
corrplot.mixed(corr, upper = "ellipse", lower = "number")


# Plot combine correlation coefficients matrix and scatter plot
# http://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/iris_plots/
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(cor(x,y), digits=2)) 
}

pairs(mydata[,2:length(mydata)], main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, upper.panel=panel.pearson,lower.panel = panel.smooth)

# The pairwise correlation coefficeints of the predictor vairables
# and the corresponding scatter plots show strong linear relationships
# among pairs of predictors variables, suggesting collinearity.
# (look at high magnitudes for correlation coefficient
# in conjuction for a trend in the scatter plot)
# For example, x_1 has |correlations| of greater than |0.6| with all
# other predictors except x_4. Looking at the scatte plot, a clear 
# linear relationship between x_1 and x_2, x_3, x_8, x_9 and x_10 
# can be seen.
# Other notable correlations from the scatter plots are:
# x_2 with x_3, x_8, x_9 and x_10 
# x_3 with x_8, x_9 and x_10 
# x_8 with x_9 and x_10 
# x_9 with x_10 
# For x_8, x_9 and x_10, the correlations with the other 
# predictors seems to follow a similar patter across
# x_8, x_9 and x_10 for each of the other predictors. 

#### Part b

fit = lm(Y~.,mydata)
summary(fit)

# Compute VIF
library(car)
vif(fit)

# Determine VIF > 10
names(vif(fit))[vif(fit)>10]

# It appears that X1, X2, X3, X7, X8 and X10 are affected by the 
# presence of collinearity because VIF > 10. Thus, there is 
# a multicollinearity problem


#### Problem 2 ##############################################
# Import data
filename = "P160.txt"
mydata = read.table(filename,header = T)
# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

#### part a

# Maximum number of terms (coefficients) in a linear regression
# is the number number of observations in the data.
# So for for this example, you can fit at most 21 terms (including
# intercept, so 20 predictors) since you need df :  
# n-(p+1)>=0, n=21 -> p+1 =< 21 (or 20? need df at least 1)

#### part b

fit = lm(V~mydata$Year+.*.,mydata[-1])
#fit = lm(V~mydata$Year+.^3,mydata[-1])
fit = update(fit,.~.-I:W)
fit = update(fit,.~.-W:P)
fit = update(fit,.~.-W:N)
summary(fit)

# Correlation matrix of predictor variables
corr = cor(model.matrix(fit)[,-1]) # corr for all predictors (e.g. interaction)
# corr = round(cor(mydata[-2]),2)  # corr for variables in dataset

corrplot.mixed(corr, upper = "ellipse", lower = "number")

pairs(corr, main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, lower.panel = NULL, panel = panel.smooth, cex.labels=2)

# Correlation matrix shows strong evidence of collinearity among some
# of the predicors (look at high magnitudes for correlation coefficient
# in conjuction for a trend in the scatter plot)
# For example, I has strong correlation with D, IN, IP, DP, DN

# Compute VIF
library(car)
vif(fit)

# Determine VIF > 10
names(vif(fit))[vif(fit)>10]

# It appears that the predicor variables above are affected by the 
# presence of collinearity because VIF > 10. The only predictors
# that don't exhibit multicollinearity problems are Year and I:D
# Note that also the VIF is very large,  so there is a severe 
# multicollinearity problem.

# Condition number
sqrt(kappa(corr,exact=TRUE))
sqrt(max(eigen(corr)$values)/min(eigen(corr)$values))

# A large condition number indicates evidence of strong collinearity
# In this case condition number > 15, so there are 
# harmful effects of collinearity in the data


#### Problem 3 ##############################################
# Import data
filename = "P160.txt"
mydata = read.table(filename,header = T)
# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

# Dummy variable
mydata$D1 = (mydata$D==1)*1
mydata$D2 = (mydata$D==-1)*1

fit = lm(V~ I + D1 + D2 + W + G:I + P + N, data = mydata)
summary(fit)

# Correlation matrix of predictor variables
corr = cor(model.matrix(fit)[,-1]) # corr for all predictors (e.g. interaction)
# corr = round(cor(mydata[-2]),2)  # corr for variables in dataset

corrplot.mixed(corr, upper = "ellipse", lower = "number")

pairs(corr, main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, lower.panel = NULL, panel = panel.smooth, cex.labels=2)

# Correlation matrix shows some evidence of collinearity among some
# of the predicors (look at high magnitudes for correlation coefficient
# in conjuction for a trend in the scatter plot)
# For example, I has moderate correlation with D1 and D2, D1 with D2, and
# W with P

# Compute VIF
library(car)
vif(fit)

# Determine VIF > 10
names(vif(fit))[vif(fit)>10]

# It appears that the none of the predicor variables above are 
# affected by the presense of collinearity because VIF < 10. 
# Thus, there is no multicollinearity problem.

# Condition number
sqrt(kappa(corr,exact=TRUE))
sqrt(max(eigen(corr)$values)/min(eigen(corr)$values))

#kappa(fit)

# A large condition number indicates evidence of strong collinearity
# In this case condition number < 15, so there are no
# harmful effects of collinearity in the data

#### Problem 4 ##############################################
# Import data
filename = "acetylene.csv"
mydata = read.csv(filename,header = T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

### Part a

corr = round(cor(mydata[1:3]),2)
corr

# Plot combine correlation coefficients matrix and scatter plot
# http://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/iris_plots/
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(cor(x,y), digits=2)) 
}

pairs(mydata[,1:3], main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, upper.panel=panel.pearson,lower.panel = panel.smooth)

# Yes, there is a sign of multicollinearity between x1 and x3,
# where it looks like a linear relationship with a strong correlation
# coefficient -0.96.

### Part b

fit = lm(y~ x1 + x2 + x3 + x1*x2 + x1*x3 + x2*x3 + I(x1^2) + I(x2^2) + I(x3^2), mydata)
summary(fit)
library(car)
vif(fit)

vif(fit)>10

# The VIF > 10 for all terms in the model and are very large, so there is a
# multicollinearity problem.

### Part c

centered = apply(mydata[1:3],2,function(x) x-mean(x))
centered = cbind(centered,mydata$y)
colnames(centered)[4]="y"
centered = data.frame(centered)

fit = lm(y~ x1 + x2 + x3 + x1*x2 + x1*x3 + x2*x3 + I(x1^2) + I(x2^2) + I(x3^2), centered)
summary(fit)
library(car)
vif(fit)

vif(fit)>10

# The VIF > 10 for some terms, so there is still multicollinearity but it is 
# less severe (all vif's are lower, and X2 and X2^2 don't exhibit
# multicollinearity)

### Part d

# The assumption that the predictors are linearly independent is violated
# because there is a multicollinearity problem. Thus, the model is not
# valid since the assumption does not hold, and therefore the model cannot 
# give reliable results. 

# exact = TRUE, sqrt (kappa)


