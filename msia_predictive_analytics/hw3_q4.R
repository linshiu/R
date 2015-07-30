
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

#### a) PPR CV ####

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

Nrep = 20  #number of replicates of CV (if doing n-fold, need only 1 replicate)
n=nrow(mydata) 
K = 10  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]
models_max = 10 # number of maximum models to test

#SSE = matrix(0,Nrep,models_max) # SSE for each model and each replicate
#sderror =matrix(0,Nrep,models_max)

#n iterations , 1.36 per iteraion
Nrep*models_max*K
loops = Nrep*models_max # without folds

# SSE, R2 for each rep, model and lambda
results = c()


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
      
      out = ppr(total_cost~., data=train, nterms=m)  
      
      yhat[Ind[[k]]]=predict(out,newdata=test)
      
    } #end of k loop
    
    #SSE[j,m] = sum((y-yhat)^2) # store SSE for this model for this CV replicate
    #sderror[j,m] = sd(y-yhat)
    
    results = rbind(results,c(j,m,sum((y-yhat)^2),1-var(y-yhat)/var(y),sd(y-yhat)))
    
    loops = loops - 1
    print(paste("Remaining Iterations: ",loops))
    
  }# end of m loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

colnames(results)=c("rep","model","sse","r2","sd")
results = aggregate(cbind(sse,r2,sd)~  model, results, mean )
#results = results[c(2,1,3,4)] # sort
results

param_opt = results[which.min(results[,"sse"]),]
param_opt

modelBest = as.numeric(param_opt["model"])
ntermsBest = modelBest

write.csv(param_opt, file = "P4_best_ppr.csv",row.names=FALSE)
write.csv(results, file = "P4_results_ppr.csv",row.names=FALSE)

#### b) Error + Model + Plots

mean(sqrt(param_opt[,"sse"]/n))

# or 

param_opt[,"sd"]

## fit best model
out = ppr(total_cost~., data=mydata, nterms=ntermsBest)
summary(out)
par(mfrow=c(1,3))
plot(out)  #plot component functions
par(mfrow=c(1,1))


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

yhat =predict(out,newdata=data.frame(test))

fit = yhat*(max(mydata_orig_log[resp])-min(mydata_orig_log[resp]))+min(mydata_orig_log[resp])
fit
fit = 10^fit
fit
 