
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


#### Problem 3 ##############################################

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

#### a) CV best kernel

# Use CV to find the best combination of span and degree (0 for local average, 1 for local linear, and 2 # for local quadratic regression) for a kernel method.

pred2 = pred[c(3,5,7,8)] # loes can take only 4 predictors

## Using CV to Compare Models for the mydata Data
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


## CV to choose the best k 

# Start the clock!
ptm <- proc.time()

Nrep = 10  #number of replicates of CV (if doing n-fold, need only 1 replicate)
n=nrow(mydata) 
K = 10  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]

#lambdas = seq(.02,2,.02)
#models = seq(0,1,2)

lambdas = seq(0.1,1,0.05)
models = c(0,1,2)

#n iterations , 1.36 per iteraion
Nrep*length(lambdas)*length(models)*K
loops = Nrep*length(lambdas)*length(models) # without folds

# SSE, R2 for each rep, model and lambda
results = c()

# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  # repeat CV for each model using the same CV partition
  for (m in models){
    
    yhat = y; # reset yhat (actually don't need this)
    
    for(l in lambdas){
      
      # CV for each model
      for (k in 1:K) {
        train = mydata[-Ind[[k]],c(resp,pred2)] # does not work if you convert to 
        # matrix and then to data frame
        test = mydata[Ind[[k]],pred2]
        #ytrain = mydata[-Ind[[k]],resp]
        
        # allow extrapolation : control = loess.control(surface = "direct")
        # Surface determines whether the fitted surface is computed directly at all 
        # points ("direct") or whether an interpolation method is used ("interpolate"). 
        # The latter, the default, is what most users should use unless special circumstances warrant.
        out=loess(total_cost ~., train ,degree=m, span=l,  control = loess.control(surface = "direct"))
        yhat[Ind[[k]]] = predict(out, newdata = test)
        
        
      } #end of k loop
      
      loops = loops - 1
      print(paste("Remaining Iterations: ",loops))
      
      results = rbind(results,c(j,m,l,sum((y-yhat)^2),1-var(y-yhat)/var(y)))
      
    } # end of l loop
  }# end of m loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

colnames(results)=c("rep","model","lambda","sse","r2")
results = aggregate(cbind(sse,r2)~  lambda + model, results, mean )
results = results[c(2,1,3,4)] # sort
results

param_opt = results[which.min(results[,"sse"]),]
param_opt

modelBest = as.numeric(param_opt["model"])
lambdaBest = as.numeric(param_opt["lambda"])

write.csv(param_opt, file = "P3_best_loess.csv",row.names=FALSE)
write.csv(results, file = "P3_results_loess.csv",row.names=FALSE)

# best = model 1, lambda = 0.3


#### b) Cp best kernel
# Use Cp to find the best combination of span and degree 
# (0 for local average, 1 for local linear, and 2 for local quadratic regression) for a kernel method. # Is this in agreement with what CV said was the best span and degree?


## Use Cp to choose lambda ##

###  first find sigma_hat for a low-bias model  ###
lam_1=c()
sig_hat_1=c()
model_1 = c()
for (model in c(0,1,2))
{
  for (lambda in seq(.02,.2,.02)) 
  {
    out=loess(total_cost ~., mydata[, c(1,4,6,8,9)],degree=model, span=lambda); 
    print(c(lambda,out$s))
    model_1=rbind(model_1,model) #model vector
    lam_1=rbind(lam_1,lambda) #lambdas' vector
    sig_hat_1=rbind(sig_hat_1,out$s) #sig_hats' vector
  } 
}

result = cbind(model_1,lam_1,sig_hat_1)
result
plot(lam_1[1:10],sig_hat_1[1:10]) #plot sig_hat vs lambda for model= 0
plot(lam_1[11:20],sig_hat_1[11:20]) #plot sig_hat vs lambda for model= 1
identify(lam_1[11:20],sig_hat_1[11:20]) # sig_hat for model = 1 is 0.10117629
plot(lam_1[21:30],sig_hat_1[21:30])  #plot sig_hat vs lambda for model= 2
identify(lam_1[21:30],sig_hat_1[21:30]) #sig_hat for model = 1 is 0.21362294

##now find Cp for various lambda###
sig_hat_1=0.10117629
sig_hat_2 = 0.21362294

#for model =1#
for (lambda in c(seq(.1,1,.1)))
{
  out=loess(total_cost ~., mydata[, c(1,4,6,8,9)],degree=1, span=lambda,
            control = loess.control(surface = "direct")
            );
  SSE=sum((mydata[,1]-out$fitted)^2); 
  Cp = (SSE+2*out$trace.hat*sig_hat_1^2)/nrow(mydata); 
  print(c(lambda,Cp))
}
#smallest Cp = 0.009891824, lambda=0.3

#for model = 2#
for (lambda in c(seq(2,4,.1)))
{
  out=loess(total_cost ~., mydata[, c(1,4,6,8,9)],degree=2, span=lambda,
            control = loess.control(surface = "direct")
            );
  SSE=sum((mydata[,1]-out$fitted)^2); 
  Cp = (SSE+2*out$trace.hat*sig_hat_2^2)/nrow(mydata); 
  print(c(lambda,Cp))
}
#smallest Cp = 0.01188193 lambda>1 plateaus at higher values of lambda

##Use Cp to choose the combination: model = 1 and lambda = 0.3

############## (c) #############
y=CRT1[[9]]
yhat=predict(out)
SSE=sum((y-yhat)^2);SSE
1-var(y-yhat)/var(y)
plot(yhat,y)

############ (d) ###############