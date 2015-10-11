# ############################################################################
# Title: MSIA 400 - Assignment 1
# Date: 10/20/14
# Author: Steven Lin
##############################################################################


# Setup ####

# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015_NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/Assignment_01"
setwd(file.path(main,course, datafolder))

# Import data
filename = "redwine.txt"
redwine = read.table(filename, header=T)


# Look at data
names(redwine)
head(redwine)
nrow(redwine)
summary(redwine)

# Problem 1 ##################################################################

# Calculate the averages of RS and SD by ignoring the missing values
RS_avg = mean(redwine$RS, na.rm = T)
SD_avg = mean(redwine$SD, na.rm = T)
RS_avg
SD_avg

# Problem 2 ##################################################################

# Create vectors of SD.obs and FS.obs by omitting observations 
# with missing values in SD

# T/F of missing values of SD
missing_SD = is.na(redwine$SD)

# Create vectors for SD and FS
SD.obs = redwine$SD[!missing_SD]
FS.obs = redwine$FS[!missing_SD]

# Fit regression
fit1 = lm(SD.obs ~ FS.obs)
summary(fit1)

# Coefficients
fit1$coeff

# Problem 3 ##################################################################

# FS values of the observations with missing SD values.
FS.obs_missing_SD = redwine$FS[missing_SD]
FS.obs_missing_SD
length(FS.obs_missing_SD)

# Estimated SD values using the regression model
SD.est = predict(fit1,data.frame(FS.obs =FS.obs_missing_SD))
SD.est = as.vector(SD.est)
SD.est

# Impute missing values of SD using the created vector.
redwine.imputed = redwine
redwine.imputed$SD[missing_SD] = SD.est
  
# Print out the average of SD after the imputation
mean(redwine.imputed$SD)

# Problem 4 ##################################################################

# T/F of missing values of RS
missing_RS = is.na(redwine.imputed$RS)

# Impute missing values of RS using the average value imputation method
redwine.imputed$RS[missing_RS] = RS_avg
summary(redwine.imputed)

# Print out the average of RS after the imputation
mean(redwine.imputed$RS)

# Problem 5 ##################################################################

# Build multiple linear regression model for the new data set 
# and save it as winemodel.

winemodel = lm(QA ~ .,redwine.imputed)

# Print out the coefficients of the regression model.
coeff = winemodel$coeff
coeff = as.matrix(winemodel$coeff)
colnames(coeff) = 'Coefficient'
coeff


# Problem 6 ##################################################################

# Printout the summary of the model.
summary(winemodel)

# Pick one attribute that is least likely to be related to QA based on p-values.
p = as.matrix(sort(summary(winemodel)$coeff[-1,c("Pr(>|t|)")]))
colnames(p)="p-value"
p

# CA, RS, FS and PH are insignificant at 0.05 level since p-values > 0.05, 
# suggesting that the coefficients are not significantly different than zero
# and the effects on QA are insignificant

# PH has the largest p-value = 0.414, indicating that is the attribute that 
# is least likely to be related to QA based on p-values ("the most insignificant")

# Problem 7 ##################################################################

# Calculate the average and standard deviation of the selected attribute
PH_avg = mean(redwine.imputed$PH)
PH_sd = sd(redwine.imputed$PH)
PH_avg
PH_sd

# boxplot(redwine.imputed$PH)

# Create a new data set after removing observations that is outside of the 
# range [ m - 3s;m  + s3] and name the data set as redwine2.

redwine2 = subset(redwine.imputed, (PH > PH_avg - 3*PH_sd) & (PH < PH_avg + 3*PH_sd))

# Print out the dimension of redwine2 to know how many observations are removed.
dim(redwine2)
dim(redwine2)[1]-dim(redwine)[1]

# 19 observations removed

# Problem 8 ##################################################################

# Build regression model winemodel2 using the new data set from Problem 7
winemodel2 = lm(QA ~ . , redwine2)

# print out the summary.
summary(winemodel2)

# Compare this model with the model obtained in Problem 6 and decide
# which one is better.

# compare r squared
summary(winemodel)$r.sq
summary(winemodel2)$r.sq

# both r.squared are too low, but model 2 has higher r-squared, 
# meaning it explains more variation in QA

# both have 4 insignificant attributes at 0.05 level
# model 1: CA, RS, FS, PH
# model 2: FA, CA, RS, DE

# both model have p-value < 2.2e-16 for the overall fit

# looking at residuals
plot(winemodel)
plot(winemodel2)

# leverage plot shows a higher influence of some data points in model 1

# from the above discussion, both models are not very good, but 
# model 2 seems better than model 1. 

# Pick 5 attributes that is most likely to be related to QA based on p-values
as.matrix(sort(summary(winemodel2)$coeff[-1,c("Pr(>|t|)")]))

# The attributes with 5 lowest p-values (most signficant effect on QA) are:
# AL, VA, SU, CH, SD

