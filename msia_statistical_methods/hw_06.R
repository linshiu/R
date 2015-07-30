#### Homework 6
#Text Book, Chapter 8, Exercises 8.1, 8.4, 8.5
#Text Book, Chapter 11, Exercises 11.5, 11.6 (don't do part (g))

#### Setup ##############################################

# Install packages if needed
# install.packages("ggplot2")
# install.packages("grid")
# install.packages("gridExtra")
# install.packages("XLConnect")
# install.packages("corrplot")
# install.packages("Hmisc")
# install.packages("car")
# install.packages("lawstat")
# install.packages("orcutt")

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
filename = "P219.txt"
mydata = read.table(filename,header = T)

### Part a

library(car)
fit = lm(H~P, data=mydata)
summary(fit)
dw_positive = durbinWatsonTest(fit, alternative = "positive", data = mydata)
dw_2sided = durbinWatsonTest(fit, alternative = "two.sided", data = mydata)
dw_positive
dw_2sided

# Durbin-Watson statistic
dw_positive$dw

# Evidence of autocorrelation is indicated by the deviation of d from 2

# H0: correlation = 0, H1: correlation > 0
# From talbe A.6, with n = 25, p = 1, and significance level 0.05, dL = 1.29, dU = 1.45
# Since d = 0.62 < dL = 1.29, reject null, conclude that value of d is significant
# at 0.05 level, showing that positive autocorrelation is present.
# Similar conclusion is reached for two-sided hypothesis (H1: correlation diff 0)

### Part b

# compute standard residuals
fit_stdres = rstandard(fit)
index_res = seq(1:length(fit_stdres))

# add standard residuals and color code +/- residuals
mydata$stdres =  rstandard(fit)
mydata$res_sign[mydata$stdres>0] = 1
mydata$res_sign[mydata$stdres<0] = -1
mydata$color[mydata$stdres>0]="red"
mydata$color[mydata$stdres<0]="blue"

# plot results

plot(index_res,fit_stdres,ylab="Standardized Residuals",xlab="Index",
     col = mydata$color, pch=16)
abline(0,0, lty=2)
lines(index_res,fit_stdres)

plot(index_res,mydata$res_sign,ylab="Sign Residuals",xlab="Index",
     col = mydata$color, pch=16)
abline(0,0, lty=2)

#  n1 = # of + res, n2 = # of - res
n1 = length(which(fit_stdres>0))
n2 = length(which(fit_stdres<0))

# expected value and standard deviation of number of runs
mu = 2*n1*n2/(n1+n2)+1
sigma = sqrt(2*n1*n2*(2*n1*n2-n1-n2)/((n1+n2-1)*(n1+n2)^2))

# number of runs
mydata$res_sign_lag = c(mydata$res_sign[1],mydata$res_sign[1:length(mydata$res_sign)-1])

mydata$res_sign_change = mydata$res_sign != mydata$res_sign_lag
n_sign_changes = sum(mydata$res_sign_change)
n_runs = n_sign_changes+1

mu
sigma
n_runs

mu-n_runs
(mu-n_runs)/sigma

# The observed number of runs is 6. The deviation of 7.32 from the expected number 
# of runs is more than triple the standard deviation, indicating a significant departure
# from randomness. 

# Using a statistical test:
z = (n_runs-mu)/sigma

# Compute critical value (two-sided)
z_crit = qnorm(.05/2,lower.tail = FALSE)
z
z_crit
abs(z)> z_crit

# Reject null hypothesis that sequence is random and conclude that 
# there is autocorrelation present

# Compute critical value  (one-sided)
z_crit = qnorm(0.05,lower.tail = TRUE)
z
z_crit
z<z_crit

# Positive autocorrelation is manifested by Small values of number of runs 
# and hence small negative values of Z. 
# Reject null hypothesis that sequence is random and conclude that 
# there is positive autocorrelation present

# use package 
# source: http://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm
library(lawstat);


# two.sided
run_test= runs.test(fit_stdres, plot.it = TRUE, alternative = "two.sided")
run_test

# Compute critical value.
qnorm(.05/2,lower.tail = FALSE)

#H0:  the sequence was produced in a random manner
#Ha:  the sequence was not produced in a random manner  

# Test statistic:  Z = -3.0615
# Significance level:  alpha = 0.05
# Critical value (upper tail):  Z1-alpha/2 = 1.96 
# Critical region: Reject H0 if |Z| > 1.96 

# Since the test statistic is greater than the critical value (p-value < 0.05)
# we conclude that the sequence are not random at the 0.05 significance level,
# indicating error terms in the model are correlated and there is a pattern
# in the residuals present. This reconfrims earilier conclusion in (a). 

# positive.correlated
run_test= runs.test(fit_stdres, plot.it = TRUE, alternative = "positive.correlated")
run_test

# Compute critical value.
qnorm(.05,lower.tail = TRUE)


#### Problem 2 ##############################################
# Import data
filename = "P229-30.txt"
mydata = read.table(filename,header = T)

### Part a

fit = lm(DJIA~Time,mydata)
summary(fit)
plot.lm(fit,which=1) # only get residuals vs fitted

plot(mydata$Time,mydata$DJIA, xlab="Time", ylab="DJIA")
abline(fit,col="red")

library(lawstat);

fit_stdres = rstandard(fit)

# two.sided
run_test= runs.test(fit_stdres, plot.it = TRUE, alternative = "two.sided")
run_test

# positive.correlated
run_test= runs.test(fit_stdres, plot.it = TRUE, alternative = "positive.correlated")
run_test

library(car)
durbinWatsonTest(fit)
durbinWatsonTest(fit,alternative="positive")



# The plot DJIA vs Time clearly shows that the linear
# model is not adequate

# Residual plot shows a trend, suggesting presence of auto-
# correlation in the residuals and, thus, the linear
# model does not seem to be adequate as the linear regression
# assumption of independent-errors does not hold. 
# The presence of correlated errors have an impact on estimates, 
# standard errors and statistical tests.

# The graph of residuals show the presence of time dependence in the 
# error term. Autocorrelation might suggest that a time-dependent 
# variable is missing from the model.

# linear trend model??? adequate looks like increasing linearly? or asking
# if function linear ok

### Part b
# Lag functions: 
# http://heuristically.wordpress.com/2012/10/29/lag-function-for-data-frames/
# http://ctszkin.com/2012/03/11/generating-a-laglead-variables/

# Create lag t-1
n = dim(mydata)[1]
mydata_lagged = data.frame(Time_t=mydata$Time[2:n],
                           DJIA_t=mydata$DJIA[2:n],
                           Time_t_1=mydata$Time[1:n-1],
                           DJIA_t_1=mydata$DJIA[1:n-1])
head(mydata_lagged)

# Regress DJIA t vs DJIA t-1

fit_lagged = lm(DJIA_t~DJIA_t_1,mydata_lagged)
summary(fit_lagged)
plot.lm(fit_lagged,which=1) # only get residuals vs fitted

plot(mydata_lagged$DJIA_t_1,mydata_lagged$DJIA_t, xlab="DJIA (t-1)", ylab="DJIA (t)")
abline(fit_lagged,col="red")

library(car)
durbinWatsonTest(fit_lagged,alternative="two.sided")
durbinWatsonTest(fit_lagged)
durbinWatsonTest(fit_lagged,alternative="positive")

library(lmtest)
dwtest(fit_lagged,alternative="greater")
dwtest(fit_lagged,alternative="two.sided")

# The plot DJIA (t) vs DJIA (t-1) clearly shows the linear model is adequate

# The residuals vs Fitted now do not show a trend, so there is 
# no evidence of autocorrelation in the residuals, indicating that
# assumption of uncorrelated residuals (independent-errors assumption) 
# is not violated. 

### Part c

fit = lm(log10(DJIA)~Time,mydata)
summary(fit)
plot.lm(fit,which=1) # only get residuals vs fitted

plot(mydata$Time,log(mydata$DJIA), xlab="Time", ylab="log(DJIA)")
abline(fit,col="red")

library(lmtest)
dwtest(fit,alternative="greater")
dwtest(fit,alternative="two.sided")


# The plot log(DJIA) vs Time clearly shows that the linear
# model is not adequate

# Residual plot shows a trend, suggesting presence of auto-
# correlation in the residuals and, thus, the linear
# model does not seem to be adequate as the linear regression
# assumption of independent-errors does not hold. 
# The presence of correlated errors have an impact on estimates, 
# standard errors and statistical tests.

# The graph of residuals show the presence of time dependence in the 
# error term. Autocorrelation might suggest that a time-dependent 
# variable is missing from the model.

# Lag functions: 
# http://heuristically.wordpress.com/2012/10/29/lag-function-for-data-frames/
# http://ctszkin.com/2012/03/11/generating-a-laglead-variables/

# Regress log(DJIA t) vs log(DJIA t-1)

fit_lagged = lm(log10(DJIA_t)~log10(DJIA_t_1),mydata_lagged)
summary(fit_lagged)
plot.lm(fit_lagged,which=1) # only get residuals vs fitted

plot(log(mydata_lagged$DJIA_t_1),log(mydata_lagged$DJIA_t),
     xlab="log(DJIA (t-1))", ylab="log(DJIA (t))")

abline(fit_lagged,col="red")

library(lmtest)
dwtest(fit_lagged,alternative="greater")
dwtest(fit_lagged,alternative="two.sided")

# The plot DJIA (t) vs DJIA (t-1) clearly shows the linear model is adequate

# The residuals vs Fitted now do not show a trend, so there is 
# no evidence of autocorrelation in the residuals, indicating that
# assumption of uncorrelated residuals (independent-errors assumption) 
# is not violated. 

# The conclusions reached in (a) and (b) are similar. This is expected as
# using the log just changes the scale, so if there is autocorrelation
# present there should be after the log transformation as well. 
# No differences are noticed. The coefficients estimates change, but the 
# signficicant tests and R^2 remain almost the same. The plots also 
# show the same patterns. 
# It seems that there is only a change in the scale and decrease in the 
# variability/volatility. 

#### Problem 3 ##############################################

### Part a

# use log or non-log model?
# should it be 129 because 130 -> lag = 129?
# Use 130 days -> so use until DJIA_t_1 = 130
mydata_lagged$log_DJIA_t = log10(mydata_lagged$DJIA_t)
mydata_lagged$log_DJIA_t_1 = log10(mydata_lagged$DJIA_t_1)
fit_lagged = lm(log_DJIA_t~log_DJIA_t_1,mydata_lagged[1:129,])



#fit_lagged = lm(DJIA_t~DJIA_t_1,mydata_lagged[1:129,])
summary(fit_lagged)

# Mean squared error (calcuate using # obs or df residuals?)
MSE_log=sum((fit_lagged$res)^2)/fit_lagged$df.res
MSE_log
anova(fit_lagged)[2,3]

MSE = sum((10^fit_lagged$fitted.values-mydata_lagged$DJIA_t[2:130])^2)/fit_lagged$df.res
MSE
# or summary(fit_lagged)$sigma^2
# MSE for log = 5.641699e-05
# MSE for log 10 = 1.06409e-05

mean((fit_lagged$res)^2)

### Part b

# First day of July 1996 = day 131, so start with DJIA_t_1 (130)

# or DJIA t_1 (131)?

# option 1: use data (not predicted of t becomes t-1 etc)

first_day = 131
last_day = 131+15-1
#newdata = data.frame(log_DJIA_t_1=mydata_lagged$DJIA_t_1[(first_day-1):(last_day-1)])
newdata = data.frame(log_DJIA_t_1=mydata_lagged$log_DJIA_t_1[(first_day-1):(last_day-1)])

predicted_log = predict(fit_lagged,newdata)
predicted = 10^predicted_log
actual_log = log10(mydata$DJIA[first_day:last_day])
actual = mydata$DJIA[first_day:last_day]


#actual = log(mydata$DJIA[first_day:last_day])

pred_error_log = actual_log - predicted_log 
pred_error = actual-predicted

results = data.frame(date = mydata$Date[first_day:last_day],
                     day = mydata$Time[first_day:last_day] ,actual = actual,
                     predicted = predicted, pred_error = pred_error,
                     actual_log = actual_log, predicted_log = predicted_log, 
                     pred_error_log = pred_error_log)

results

plot(results$day,results$actual)
lines(results$day,results$actual, col="red")
par(new=TRUE)
plot(results$day,results$predicted,xlab="",ylab="",ylim=range(results$actual))
lines(results$day,results$predicted, col="blue")

require(ggplot2)


ggplot(results, aes(day)) + 
  geom_point(aes(y=actual),size=3, color = "red") +
  geom_line(aes(y=actual), colour="red") +
  geom_point(aes(y=predicted),size=3, color = "blue")+
  geom_line(aes(y=predicted), colour="blue") +
  scale_colour_manual("Legend",breaks = c("Actual", "Predicted"),
                    values = c("red", "blue"))


### Part c
AVE_Sq_error15 = mean(pred_error^2)
AVE_Sq_error15 

AVE_Sq_error15_log = mean(pred_error_log^2)
AVE_Sq_error15_log 

# MSE for log 0.0001400283

# Much higher than MSE in (a) as expected since part (c) is testing
# data in a new period and in a smaller sample, while MSE (a) is for 
# the data the the model was built on and over a longer time period. 

### Part d

# Use to predict second half (132 days)

# First day of July 1996 = day 131, so start with DJIA_t_1 (130)

# option 1: use data

first_day = 131
last_day = dim(mydata)[1]
#newdata = data.frame(DJIA_t_1=mydata_lagged$DJIA_t_1[(first_day-1):(last_day-1)])
newdata = data.frame(log_DJIA_t_1=mydata_lagged$log_DJIA_t_1[(first_day-1):(last_day-1)])


predicted_log = predict(fit_lagged,newdata)
predicted = 10^predicted_log
actual_log = log10(mydata$DJIA[first_day:last_day])
actual = mydata$DJIA[first_day:last_day]


#actual = log(mydata$DJIA[first_day:last_day])

pred_error_log = actual_log - predicted_log 
pred_error = actual-predicted

results = data.frame(date = mydata$Date[first_day:last_day],
                     day = mydata$Time[first_day:last_day] ,actual = actual,
                     predicted = predicted, pred_error = pred_error,
                     actual_log = actual_log, predicted_log = predicted_log, 
                     pred_error_log = pred_error_log)

results

plot(results$day,results$actual)
lines(results$day,results$actual, col="red")
par(new=TRUE)
plot(results$day,results$predicted,xlab="",ylab="",ylim=range(results$actual))
lines(results$day,results$predicted, col="blue")

require(ggplot2)


ggplot(results, aes(day)) + 
  geom_point(aes(y=actual),size=3, color = "red") +
  geom_line(aes(y=actual), colour="red") +
  geom_point(aes(y=predicted),size=3, color = "blue")+
  geom_line(aes(y=predicted), colour="blue") +
  scale_colour_manual("Legend",breaks = c("Actual", "Predicted"),
                      values = c("red", "blue"))

AVE_Sq_error132 = mean(pred_error^2)
AVE_Sq_error132 

AVE_Sq_error132_log = mean(pred_error_log^2)
AVE_Sq_error132_log 

# MSE for log 6.910339e-05

# Higher than MSE (a), but lower than average squared prediction error for 
# first 15 days of second half of year (part c)


### Part e
mydata$color[mydata$Time<131]="red"
mydata$color[mydata$Time>=131]="blue"

plot(mydata$Time,mydata$DJIA, xlab="Day", ylab="DJIA", col=mydata$color)
legend(0,6500, c("First Half","Second Half"), # puts text in the legend in the appropriate place
      lty=c(1,1), # gives the legend appropriate symbols (lines)
      lwd=c(2.5,2.5), # gives the legend lines the correct color and width
      col=c("red","blue")) 

# From the scater plot we clearly see a difference between the first half 
# of the year and the second half. Because the model used the training data
# for the first half of the year, then the prediction error is larger in 
# (c) and (d) than (a) because (c) and (d) is based on the second part of the year.
# (different data as the training set)
#
# The behavior for the entire second half of the year is similar to the behavior 
# of the entire first half of the year. Because the model was based on an entire
# half of the year, then the predictions errors will be smaller for the 
# entire second half of the year rather than a small portion (e.g. 15 days). 
# The error of a small sample is also larger than a bigger sample. 
# In addition we see that for the first 15 days the DJIA decreases while 
# it increases in a stable manner afterwards, similary to the first half of the year. 


# Problem 4 ####
var_names = c("taxes","bath","lot","living","garage","rooms","bedrooms","age", 
            "fireplaces","sale")

predictor_names = c("intercept","taxes","bath","lot","living","garage","rooms","bedrooms","age", 
             "fireplaces")


# Import data
filename = "P329.txt"
mydata = read.table(filename,header =T)

colnames(mydata)=var_names

## Part a

# Correlation matrix

corr = round(cor(mydata),2)

library(corrplot)
corrplot.mixed(corr, upper = "ellipse", lower = "number")

pairs(mydata[,-1], main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, lower.panel = NULL, panel = panel.smooth, cex.labels=2)

# The pairwise correlation coefficeints of the predictor vairables
# and the corresponding scatter plots show strong linear relationships
# among  some pairs of predictors variables, suggesting collinearity.
# (look at high magnitudes for correlation coefficient
# in conjuction for a trend in the scatter plot)
# In particular, rooms (X6) and bedrooms (X7) are strongly correlated, 
# which makes sense since a bedroom is a room too. Thus, both variables cannot
# be in the model since might cause the non-collinearity assumption
# to be violated.

fit = lm (sale~.,mydata)
summary(fit)

# Compute VIF
library(car)
vif(fit)

# Determine VIF > 10
names(vif(fit))[vif(fit)>10]

# Fitting a linear model with all predictors and computing VIF
# confirms our suspicion. 
# It appears that rooms (X6) is affected by the 
# presence of collinearity because VIF > 10. Thus, there is 
# a multicollinearity problem.
# Do not include all of them because of multicollinearity.
# In addition, if all variables in the model are included,
# none of the variables are significant (p-value > 0.05)


## Part b

fit = lm (sale ~ taxes + rooms + age,mydata)
summary(fit)

plot(fit)

# Compute VIF
library(car)
vif(fit)

# Determine VIF > 10
names(vif(fit))[vif(fit)>10]

# The residuals diagnostics and VIF show no problems.
# The R^2 of 0.77 is OK but not great, and 
# the two predictors in the model (rooms and age)
# are far from being signficant (p-value > 0.05). 
# Thus, this modelwould NOT adequately describe the 
# sale price.

## Part c

# Stepwise regression to determine best model, start with all variables
library(MASS)
fit= lm(sale~., data=mydata)
fit_stepAIC= step(object=fit, direction="both") # AIC
summary(fit_stepAIC)

x = mydata[,1:9] # design matrix or use model.matrix(fullfit)
y = mydata[,10]  # response vector

# function returns best subset function with different criteria
modelSelection = function(x,y){
  # Inputs:
  # x = design matrix
  # y = response vector
  n = length(y) # number of observations
  p = dim(x)[2] # number of predictors
  
  # Variable Selection Using Package 
  library(leaps)
  
  # find the best subset
  reg_exh= regsubsets(x,y, nbest=1, nvmax=n, method="exhaustive")
  #summary(reg_exh,matrix.logical=TRUE)
  #names(reg_exh)
  #names(summary(reg_exh))
  #plot(reg.exh, scale="adjr2")
  #plot(leaps, scale="bic")
  
  # get matrix with models
  models = summary(reg_exh)$which # T/F -> multiply by 1 to get 1/0 (not needed)
  msize=as.numeric(apply(models,1,sum)) # model size
  
  # compute criteria
  cp = summary(reg_exh)$cp; cp = round(cp,3)
  adjr2 = summary(reg_exh)$adjr2; adjr2 = round(adjr2,3)
  aic = n*log(summary(reg_exh)$rss/n) + 2*msize; aic = round(aic,3) 
  bic = n*log(summary(reg_exh)$rss/n) + msize*log(n); bic = round(bic,3)
  # different from regsubsets, just differ by constant
  # bic = summary(reg_exh)$bic; bic = round(bic,3)
  
  # alternative
  # optimizing various criteria
  #leaps(x,y,nbest=1,method="Cp")
  #leaps(x,y,nbest=1,method="adjr2")
  
  # rank by criteria
  rk_cp = as.numeric(factor(cp))
  rk_adjr2 = vector(length=length(adjr2 ))
  rk_adjr2[order(adjr2,decreasing=TRUE)] = 1:length(adjr2 ) # highest is better
  rk_aic  = as.numeric(factor(aic))
  rk_bic = as.numeric(factor(bic))
  
  rk_tot = rk_cp + rk_adjr2 + rk_aic +rk_bic
  
  # create matrix and data frame of results
  results = cbind(msize,models,cp,adjr2,aic,bic,rk_cp,rk_adjr2,
                  rk_aic,rk_bic,rk_tot )
  
  colnames(results)[2]="Int"
  
  results_df = data.frame(results)

  # display results
  results 
  
  # alternative
  # x1 = vector(length=length(cp))
  # x1[order(cp)] = 1:length(cp)
  
  # Models
  cp_model = c("intercept",colnames(x)[models[order(cp)[1],][-1]])
  adjr2_model = c("intercept",colnames(x)[models[order(adjr2,decreasing=TRUE)[1],][-1]])
  aic_model = c("intercept",colnames(x)[models[order(aic)[1],][-1]])
  bic_model = c("intercept",colnames(x)[models[order(bic)[1],][-1]])
  
  cat("best cp model:\n",cp_model,"\n")
  cat("best adjr2 model:\n",adjr2_model,"\n")
  cat("best aic model:\n",aic_model,"\n")
  cat("best bic model:\n",bic_model,"\n")
  
  # Order results
  #results[order(cp),]; # order by Cp
  #results[order(adjr2,decreasing=TRUE),]; # order by adjr2
  #results[order(aic),]; # order by BIC
  #results[order(bic),]; # order by BIC
  
  # alternative
  # sort(cp, decreasing = FALSE,index.return=TRUE)$ix <-> order(cp)
  
  #plots
  
  localenv = environment()
  
    require(ggplot2)
    require(grid)
    require(gridExtra)
  
    
    plot_vector = vector(mode="list",length=4)
    
    plot_vector[[1]] = ggplot(results_df,aes(x=results_df[[1]], y = results_df[[p+3]]),environment = localenv) + 
      geom_point(size = 4) +
      geom_line(aes(y=results_df[[p+3]]), colour="blue") +
      labs(x = colnames(results_df[1]),y = colnames(results_df[p+3])) +
      scale_x_continuous(breaks=msize)+
      geom_point(data=results_df[order(cp)[1], ], aes(x=msize, y=cp), colour="red", size=5)
  
    
    plot_vector[[2]]  = ggplot(results_df,aes(x=results_df[[1]], y = results_df[[p+3+1]]),environment = localenv) + 
      geom_point(size = 4) +
      geom_line(aes(y=results_df[[p+3+1]]), colour="blue") +
      labs(x = colnames(results_df[1]),y = colnames(results_df[p+3+1]))+
      scale_x_continuous(breaks=msize) +
      geom_point(data=results_df[order(adjr2,decreasing=TRUE)[1], ], aes(x=msize, y=adjr2), colour="red", size=5)
    
    plot_vector[[3]]  = ggplot(results_df,aes(x=results_df[[1]], y = results_df[[p+3+2]]),environment = localenv) + 
      geom_point(size = 4) +
      geom_line(aes(y=results_df[[p+3+2]]), colour="blue") +
      labs(x = colnames(results_df[1]),y = colnames(results_df[p+3+2]))+
      scale_x_continuous(breaks=msize) +
      geom_point(data=results_df[order(aic)[1], ], aes(x=msize, y=aic), colour="red", size=5)
    
    plot_vector[[4]]  = ggplot(results_df,aes(x=results_df[[1]], y = results_df[[p+3+3]]),environment = localenv) + 
      geom_point(size = 4) +
      geom_line(aes(y=results_df[[p+3+3]]), colour="blue") +
      labs(x = colnames(results_df[1]),y = colnames(results_df[p+3+3]))+
      scale_x_continuous(breaks=msize) +
      geom_point(data=results_df[order(bic)[1], ], aes(x=msize, y=bic), colour="red", size=5)
    
    
    grid.arrange(plot_vector[[1]],
                 plot_vector[[2]],
                 plot_vector[[3]],
                 plot_vector[[4]],
                 ncol=2, main = "Model Selection")
  

  return(results_df)
   
}

bestSubset = modelSelection(x,y)
bestSubset

# Stepwise regression shows that the best model is: 
# X1 (taxes), X2 (bathrooms), X5 (garage) and X7 (bedrooms). 

# The model by the expert(sales ~ taxes (x1)) is ranked
# 4th in cp, last in adjusted r^2, sixth in aic and 2nd in BIC. Thus, comparing it
# with other models, the assertion by the expert that the building characteristics
# are redundant does not hold. For instance, the most adequate models (include intercept)
# seem to be: 1) X1 (taxes) and X2 (bathrooms), -> best in cp and bic criteria
#             2) X1 (taxes), X2 (bathrooms), X5 (garage) and X7 (bedrooms) -> best in adjr^2 and aic criteria
# In addition to taxes, the two models include bathroom as a signficant predictor 
# of price, and one model includes garage and bedroom as well. 

# Model X1 (taxes), X2 (bathrooms), X5 (garage) and X7 (bedrooms) indicates that
# garage and bedrooms are not significant. Thus, the most adequate model for 
# predicting sales price seems to be X1 (taxes) and X2 (bathrooms). 

# Problem 5 ####

# Import data
filename = "P256.txt"
mydata = read.table(filename,header =T)

# Look at data
names(mydata)
head(mydata)
nrow(mydata)
summary(mydata)

# Fix names
names(mydata)[-1]=sapply(1:11,function(i) paste("X",i,sep=""))

## Part a

corr = round(cor(mydata),2)

library(corrplot)
corrplot.mixed(corr, upper = "ellipse", lower = "number")

pairs(mydata[,-1], main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, lower.panel = NULL, panel = panel.smooth, cex.labels=2)

# The pairwise correlation coefficeints of the predictor vairables
# and the corresponding scatter plots show strong linear relationships
# among  some pairs of predictors variables, suggesting collinearity.
# (look at high magnitudes for correlation coefficient
# in conjuction for a trend in the scatter plot)
# For example, X1 is strongly correlated with X2,X3, X8, X9, X10 and X11, 
# If all of these are included in the model, the the non-collinearity 
# assumption of the predictors might be violated.

fit = lm (Y~.,mydata)
summary(fit)

# Compute VIF
library(car)
vif(fit)

# Determine VIF > 10
names(vif(fit))[vif(fit)>10]

# Fitting a linear model with all predictors and computing VIF
# confirms our suspicion. 
# It appears that X1, X2, X3, X7, X8 and X10 are affected by the 
# presence of collinearity because VIF > 10. Thus, there is 
# a multicollinearity problem if all variables are included.
# Thus, do not include all of them because of multicollinearity
# In addition, if all variables in the model are included,
# none of the variables are significant (p-value > 0.05)


## Part b
x = mydata[,2:12] # design matrix or use model.matrix(fullfit)
y = mydata[,1]  # response vector

n = length(y) # number of observations
p = dim(x)[2] # number of predictors


models = vector(mode="list",length=6)
models[[1]]=lm(Y~X1,mydata)
models[[2]]=lm(Y~X10,mydata)
models[[3]]=lm(Y~X1+X10,mydata)
models[[4]]=lm(Y~X2+X10,mydata)
models[[5]]=lm(Y~X8+X10,mydata)
models[[6]]=lm(Y~X8+X5+X10,mydata)

full =lm(Y~.,mydata) 

# compute the selection model criteria
# input = lm object for desired model and full model
computeCriteria = function(fit,full){
  n = length(summary(fit)$res)
  msize = dim(summary(fit)$coeff)[1]
  RSS = sum(summary(fit)$residuals^2)
  cp = RSS/summary(full)$sigma^2 + 2*msize-n
  adjr2 = summary(fit)$adj.r
  aic = n*log(RSS/n) + 2*msize
  bic = n*log(RSS/n) + msize*log(n)
  
  return (round(c(cp,adjr2,aic,bic),3))
  
}

results = matrix(, nrow = 6, ncol = 4)

for (i in 1:6){
  results[i,]=computeCriteria(models[[i]],full) 
}

colnames(results)=c("cp","adjr2","aic","bic")
rownames(results)=1:6
results 

# get best model from six
rownames(results)[order(results[,"cp"])[1]]
rownames(results)[order(results[,"adjr2"],decreasing=TRUE)[1]]
rownames(results)[order(results[,"aic"])[1]]
rownames(results)[order(results[,"bic"])[1]]

# Among the six regression models, model 6 (X8,X5,X10) is the best
# in predicting Y because it has the highest adjr2, lowest cp,
# lowest aic and second-to-lowest bic (very close to the lowest bic).

# find a better model
bestSubset = modelSelection(x,y)
bestSubset

# Comparing the best models of each model size, we see that the same 
# model (X8,X5,X10) is the best model in terms of adjr2, cp and aic.
# The bic is also very close to the best model. 
# Thus, no other better models can be suggested  (assuming no transformation 
# or higher order terms or interactions terms are considered). 

# Stepwise regression to determine best model, start with all variables
library(MASS)
fit_stepAIC= step(object=full, direction="both") # AIC
summary(fit_stepAIC)

# Using a stepwise regression confirms the conclusions reached above

## Part c

plot1 = 
  ggplot(mydata,aes(x=X1, y = Y)) + 
  geom_point(size = 3) 

plot2 =
  ggplot(mydata,aes(x=X2, y = Y)) + 
  geom_point(size = 3) 

plot3 =
  ggplot(mydata,aes(x=X8, y = Y)) + 
  geom_point(size = 3)  

plot4 =
  ggplot(mydata,aes(x=X10, y = Y)) + 
  geom_point(size = 3)  

grid.arrange(plot1,plot2,plot3,plot4,ncol=2,
             main = "Y vs X1,X2,X8,X10")


# The plots suggets that the relationship between Y and X1,X2,X8 and X10
# (individually) is not linear. It seems that the relationship is 
# hyperbolic (i.e. 1/x)

# Part d

mydata$W = 100/mydata$Y


plot1 = 
  ggplot(mydata,aes(x=X1, y = W)) + 
  geom_point(size = 3) 

plot2 =
  ggplot(mydata,aes(x=X2, y = W)) + 
  geom_point(size = 3) 

plot3 =
  ggplot(mydata,aes(x=X8, y = W)) + 
  geom_point(size = 3)  

plot4 =
  ggplot(mydata,aes(x=X10, y = W)) + 
  geom_point(size = 3)  

grid.arrange(plot1,plot2,plot3,plot4,ncol=2,
             main = "W vs X1,X2,X8,X10")

# The plots now suggets that the relationship between W and X1,X2,X8 and X10
# (individually) is more linear than that between Y and the variables.


## Part e

w = mydata$W  # response vector

models = vector(mode="list",length=6)
models[[1]]=lm(W~X1,mydata)
models[[2]]=lm(W~X10,mydata)
models[[3]]=lm(W~X1+X10,mydata)
models[[4]]=lm(W~X2+X10,mydata)
models[[5]]=lm(W~X8+X10,mydata)
models[[6]]=lm(W~X8+X5+X10,mydata)

full =lm(W~.,mydata) 

# compute the selection model criteria
# input = lm object for desired model and full model

results = matrix(, nrow = 6, ncol = 4)

for (i in 1:6){
  results[i,]=computeCriteria(models[[i]],full) 
}

colnames(results)=c("cp","adjr2","aic","bic")
rownames(results)=1:6
results 

# get best model from six
rownames(results)[order(results[,"cp"])[1]]
rownames(results)[order(results[,"adjr2"],decreasing=TRUE)[1]]
rownames(results)[order(results[,"aic"])[1]]
rownames(results)[order(results[,"bic"])[1]]

# Among the six regression models, model 5 (x8,10) is the best
# in predicting W because it has the highest adjr2, lowest cp,
# lowest aic and lowest bic. This answer is different from (b).

# find a better model
bestSubset = modelSelection(x,w)
bestSubset

# Comparing the best models of each model size, we see that the same 
# model (X8,X10) is the best model in terms of bic, cp and aic.
# The adjr2 is also very close to the best. 
# Thus, no other better models can be suggested  (assuming no transformation 
# or higher order terms or interactions terms are considered). 

# Stepwise regression to determine best model, start with all variables
library(MASS)
fit_stepAIC= step(object=full, direction="both") # AIC
summary(fit_stepAIC)

# Using a stepwise regression confirms shows a different model,
# however this model has insignificant terms and also ranks lower in other
# criteria and also has too many predictors. So the (X8,X10) model is preferred. 
# In conclusion, the transformation of the variable makes a difference
# in variable selection so it should be examined carefully. 

## Part f

# Alternative 5 is best
mydata$X13 = mydata$X8/mydata$X10

fit = lm(Y~X13, mydata)
summary(fit)

ggplot(mydata,aes(x=X13, y = Y)) + 
  geom_point(size = 3)  +
  stat_smooth(method = 'lm', se= FALSE)

# The model seems to be very good in terms
# of R^2 and fit to the data. 

