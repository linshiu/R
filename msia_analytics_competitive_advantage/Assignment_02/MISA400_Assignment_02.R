# #######################################################################
# Title: MSIA 400 - Assignment 2
# Date: 10/28/14
# Author: Steven Lin
#########################################################################


# Setup ####

# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015_NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/Assignment_02"

# Problem 1 #############################################################

setwd(file.path(main,course, datafolder))

# Import data
filename = "bostonhousing.txt"
mydata= read.table(filename, header=T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

#### Part a

reg = lm(MEDV ~ ., mydata)
summary(reg)

# INDUS and AGE are least likely to be in the model because their 
# p-values are 0.738 and 0.958 respectively, indicating they are not 
# significant in predicting MEDV given the other varibles are in the
# model (coefficients not significantly different than zero). 

#### Part b

reg.picked = update(reg,.~.-INDUS - AGE)
summary(reg.picked)

#### Part c

# Is the formulate right? Should it be dividing by number of obs?

# Functions to calculate
compute_MSE = function(fit){
  return (round(sum((fit$res)^2)/fit$df.res,3))
}

compute_MAE = function(fit){
  return (round(sum(abs(reg$res))/fit$df.res,3))
}

# List of models
models = list(reg= reg,reg.picked = reg.picked)

results = rbind(sapply(models,compute_MSE),sapply(models,compute_MAE))
rownames(results) = c("MSE","MAE")
results

# The model with the lowest MSE and MAE is preferred, so in this case
# pick reg.picked


#### Part d
#step(reg)
summary(step(reg))

# The model from step(reg) is the same model as reg.picked from 1 b).

# Problem 2 #############################################################

# Import data
filename = "labdata.txt"
mydata= read.table(filename, header=T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

#### Part a

reg = lm(y ~ ., mydata)
summary(reg)

#### Part b

# plot y vs x
pairs(mydata, main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, lower.panel = NULL, panel = panel.smooth, cex.labels=2)

# Select x1

# Load packages
library(ggplot2)

ggplot(mydata,aes(x=x1, y = y)) + 
  geom_point(size = 3) 

#### Part c

mean_x1 = mean(mydata$x1)
mean_x1

reg.piece = lm(y~ (x1<mean_x1)*x1 +x2 + x3 + x4 + x5 + x6 + x7 + x8,mydata)
summary(reg.piece)

reg.piece = lm(y~ . + (x1<mean_x1)*x1 ,mydata)
summary(reg.piece)

# or use package (but uses R adjusts breakpoint you enter)
# install.packages("segmented")
#library(segmented)
#reg1 = lm(y~ x1,mydata)
#reg.piece1 = segmented(reg1, seg.Z= ~x1, psi=mean_x1)
#summary(reg.piece1)

#library(segmented)

#reg.piece1 = segmented(reg, seg.Z= ~x1, psi=mean_x1)
#summary(reg.piece1)

# List of models
models = list(reg= reg,reg.piece= reg.piece)

results = rbind(sapply(models,AIC),sapply(models,BIC),
                sapply(models,function(x) round(summary(x)$adj.r,3)))

rownames(results) = c("AIC","BIC","Adj.R2")
results


# Piecewise regression is better beceause it has a higher
# r-squared, also BIC and AIC is lower 


