
# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_420_Predictive_Analytics"
datafolder = "Data"
setwd(file.path(main,course, datafolder))

filename = "HW2_data.csv"
mydata = read.csv(filename,header = T)

library(yaImpute)

#### Problem 2 ##############################################

head(mydata)
mydata = mydata[-1]
head(mydata) # drop ID column

mydata_orig = mydata

# convert rsponse to log10 (change 0 to 1)
hist(mydata$total_cost)
mydata$total_cost[mydata$total_cost == 0] = 1
mydata$total_cost = log10(mydata$total_cost)
hist(mydata$total_cost)

mydata_orig_log = mydata

resp = names(mydata)[1]  # response variable
pred = names(mydata)[-1] # predictor variables

mydata[pred] = sapply(mydata[pred], function(x) (x-mean(x))/sd(x)) #standardize predictors
mydata[resp] = (mydata[resp]-min(mydata[resp]))/(max(mydata[resp])-min(mydata[resp]))


#### a) GAM fit ####

# Fit a GAM model without interactions, and construct 
# plots of the component functions. Which predictors 
# appear to be the most relevant for predicting cost?

library(mgcv)  #stands for "Mixed GAM Computation Vehicle"

# reduce the degrees of freedom if fewer distinct values than default (10)
#s(gender) # 2
#s(drugs)  # 9
#s(complications # 3

# http://www.talkstats.com/showthread.php/39182-GAM-%28generalised-additive-models%29-function
# http://r.789695.n4.nabble.com/gam-error-td2241518.html
# https://stat.ethz.ch/pipermail/r-help/2007-October/143569.html
# https://stat.ethz.ch/pipermail/r-help/2011-November/295047.html


# discrete variables to linear terms 

out=gam(total_cost~s(age)+ gender + s(interventions)+ s(drugs,k=8) +  s(ER_visits) + 
          complications +
          s(comorbidities) + s(duration), 
        data=mydata, family=gaussian(), sp=c(-1,-1,-1,-1,-1,-1)) 

summary(out)
out$sp  ##estimated smoothing parameters for each constituent function 
yhat=predict(out)
plot(yhat,mydata[,resp])  #probably quite a bit of overitting
##
par(mfrow=c(2,4))
plot(out)  #plot component functions
par(mfrow=c(1,1))

# intvn, comorb, and dur, comp, ervis seem 
# The overall average effect (main effect, assuming not interactions) of the
# predictors on the response:
# As intervention increaes, the cost increases the most and then plateaus
# As comp and comrob increaes, the costs slightly increaes and the plateaus
# At high values of ervis, as ervis increae the cost increases at increasing rate
# 
# The higher the edf , the higher complexity of smoothing function
# p-value shows age and drugs are not significant
# Signficance in decreasing order of signifcance based on pvalues:
# Most signifcant to least: como, interv, duration, complications, ervisits

#### b) GAM CV ####

# For the model from part (a), what is the CV estimate
# of the prediction error standard deviation? What are 
# the pros and cons of using n-fold CV, versus say 10-fold CV, for GAMs?

#R commands for creating indices of partition for K-fold CV

CVInd = function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m = floor(n/K)  #approximate size of each part
  r=n-m*K  
  I=sample(n,n)  #random reordering of the indices
  Ind=list()  #will be list of indices for all K parts
  length(Ind)=K
  for (k in 1:K) {
    if (k <= r) kpart = ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart=((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] = I[kpart]  #indices for kth part of data
  }
  Ind
}

# CV to choose the best k 

# Start the clock!
ptm <- proc.time()

Nrep = 1  #number of replicates of CV (if doing n-fold, need only 1 replicate)
n=nrow(mydata) 
K = n  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]
models_max = 1 # number of maximum models to test

SSE = matrix(0,Nrep,models_max) # SSE for each model and each replicate
sderror =matrix(0,Nrep,models_max)

# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  # repeat CV for each model using the same CV partition
  for (m in 1:models_max){
    
    yhat = y; # reset yhat (actually don't need this)
    
    # CV for each model
    for (k in 1:K) {
      train = mydata[-Ind[[k]],c(resp,pred)]
      test = mydata[Ind[[k]],pred]
      #ytrain = mydata[-Ind[[k]],resp]
      
      out=gam(total_cost~ gender + s(age)+s(interventions)+ s(drugs,k=8) +  
                s(ER_visits)  + complications +s(comorbidities) + s(duration), 
              data=train, 
              family=gaussian(), sp=c(-1,-1,-1,-1,-1,-1))
      
      yhat[Ind[[k]]]=predict(out,newdata=test)
      
    } #end of k loop
    
    SSE[j,m] = sum((y-yhat)^2) # store SSE for this model for this CV replicate
    sderror[j,m] = sd(y-yhat)
    
  }# end of m loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

SSE
SSEAve = apply(SSE,2,mean);SSEAve
sderrorAve = apply(sderror,2,mean);sderrorAve

mean(sqrt(SSE[,1]/n))

# or 

mean(sderror[,1])

#### c) predict new case ####
# What is the predicted cost for a person with age=59, 
# gend=0, intvn=10, drugs=0, ervis=3, comp=0, comorb=4, and dur=300?

train = as.matrix(mydata[,c(resp,pred)])
test = matrix(c(59,0,10,0,3,0,4,300),nrow=1,ncol=length(pred))
colnames(test) = pred

# standardize
mean_pred = apply(mydata_orig[pred],2,mean)
sds_pred = apply(mydata_orig[pred],2,sd)
test = apply(test,1,function(x) (x-mean_pred)/sds_pred)
test = t(test) # transpose to get obs in rows

out=gam(total_cost~s(age)+ gender + s(interventions)+ s(drugs,k=8) +  s(ER_visits) + complications +
          s(comorbidities) + s(duration), 
        data=data.frame(train), family=gaussian(), sp=c(-1,-1,-1,-1,-1,-1)) 

yhat =predict(out,newdata=data.frame(test))
fit = yhat*(max(mydata_orig_log[resp])-min(mydata_orig_log[resp]))+min(mydata_orig_log[resp])
fit
fit = 10^fit
fit


# Note this is not the same model as in CV, it includes complications, which had to be removed
# in CV because distinct values for complications becomes equal to 2 in some partitions. 