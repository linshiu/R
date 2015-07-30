
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


#### Problem 1 ##############################################

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


#### a) choose best K ####
# Use n-fold CV to find the best K for predicting 
# cost using K-NN. What are the pros and cons of using 
# n-fold CV, versus say 10-fold CV, for nearest neighbors?

# PLarger K means less bias towards overestimating the true 
# expected error (as training folds will be closer to the total dataset)
# but higher variance and higher running time (as you are getting closer 
# to the limit case: Leave-One-Out CV)
# n-fold equally computational effort. No need to run replicates compared
# to 10-fold. Use more neighbohrs, but only one observation to test so
# higher variance in the prediction

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
KNN_max = 10 # 1:KNN_max models

SSE = matrix(0,Nrep,KNN_max) # SSE for each model and each replicate
sderror =matrix(0,Nrep,KNN_max)

# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  # repeat CV for each model using the same CV partition
  for (KNN in 1:KNN_max){
    
    yhat = y; # reset yhat (actually don't need this)
    
    # CV for each model
    for (k in 1:K) {
      train = as.matrix(mydata[-Ind[[k]],pred])
      test = as.matrix(mydata[Ind[[k]],pred])
      ytrain = mydata[-Ind[[k]],resp]
      
      out = ann(train,test,KNN,verbose=F)
      
      # when n-fold, then test has one observation. indices of out$Knn.. are in one row 
      # and become a vector when subset. As.matrix converts this to matrix with n columns
      # equal to number of knn models. So need to transpose or add ncol = KNN and use matrix function.
      ind = matrix(out$knnIndexDist[,1:KNN],ncol=KNN) # nearest neighbor indices for each obs in test
      
      yhat[Ind[[k]]] = apply(ind,1,function(x) mean(ytrain[x])) # prediction = average of y of neighbors
      
    } #end of k loop
    
    SSE[j,KNN] = sum((y-yhat)^2) # store SSE for this model for this CV replicate
    sderror[j,KNN] = sd(y-yhat)
    
  }# end of KNN loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

SSE
SSEAve = apply(SSE,2,mean);SSEAve
sderrorAve = apply(sderror,2,mean);sderrorAve

R2 = 1-SSEAve/n/var(y)
R2

K_opt = which.max(R2)
K_opt

plot(R2)
plot(yhat,y)

write.csv(K_opt, file = "P1_best_knn.csv",row.names=FALSE)
write.csv(SSEAve, file = "P1_cv_knn.csv",row.names=FALSE)

#### b) error ####
# For the optimal K from part (a), what is the CV 
# estimate of the prediction error standard deviation?

# prediction error standard deviation
# https://books.google.com/books?id=BB8fc1nTo_4C&pg=PA183&lpg=PA183&dq=%22prediction+error+standard+deviation%22&source=bl&ots=q0zS8nHJks&sig=PMgtUdTiJzwV3FebgjoX_L-f7rI&hl=en&sa=X&ei=GCjzVOSvH4GRyATQ-IDgDA&ved=0CCUQ6AEwAzgK#v=onepage&q=%22prediction%20error%20standard%20deviation%22&f=false

mean(sqrt(SSE[,K_opt]/n))

# or 

mean(sderror[,K_opt])

#### c) predict new case ####
# What is the predicted cost for a person with age=59, 
# gend=0, intvn=10, drugs=0, ervis=3, comp=0, comorb=4, and dur=300?

train = as.matrix(mydata[,pred])
test = matrix(c(59,0,10,0,3,0,4,300),nrow=1,ncol=length(pred))
colnames(test) = pred

# standardize
mean_pred = apply(mydata_orig[pred],2,mean)
sds_pred = apply(mydata_orig[pred],2,sd)
test = apply(test,1,function(x) (x-mean_pred)/sds_pred)
test = t(test) # transpose to get obs in rows

out = ann(train,test,K_opt)
ind = matrix(out$knnIndexDist[,1:K_opt],ncol=K_opt)

# approach 1: 
yorig = mydata_orig[,resp] # or use ytrain and unstandardize and unlogged
fit  = apply(ind,1,function(x) mean(yorig[x]))
fit

fit_log  = apply(ind,1,function(x) mean(y[x]))+-min(mydata_orig_log[resp])
fit_log = fit_log*(max(mydata_orig_log[resp])-min(mydata_orig[resp]))+min(mydata_orig_log[resp])
fit_log
10^fit_log
