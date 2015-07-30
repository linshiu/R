
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

#### Problem 5 ##############################################

library(yaImpute)
mydata=read.table("fgl.txt",sep="\t")
mydata_orig = mydata

z=(mydata$type == "WinF") | (mydata$type == "WinNF")
y=as.character(mydata$type)
y[z]="Win"; y[!z]="Other"
mydata=data.frame(mydata,"type_bin"=as.factor(y))  #add a binary factor response column
y[y == "Win"]=1;y[y == "Other"]=0;
mydata=data.frame(mydata,"type01"=as.numeric(y))  #also add a binary numeric response column

pred = names(mydata)[1:9]
resp = names(mydata)[11]

mydata[pred]=sapply(mydata[pred], function(x) (x-mean(x))/sd(x)) #standardize predictors
train=as.matrix(mydata[,pred]); test=as.matrix(mydata[,pred])
ytrain=mydata[,resp]; ytest=mydata[,resp]

#### Part a: KNN ####
K=5
out=ann(train,test,K)
ind=as.matrix(out$knnIndexDist[,1:K],ncol=K)
phat=apply(ind,1,function(x) sum(ytrain[x]=="Win")/length(ytrain[x]))
result = phat
result[phat>=0.5] = "Win"
result[phat<0.5] = "Other"
result = as.factor(result)
result

# plot(phat,jitter(as.numeric(ytest=="Win"),amount=.05))

####can alternatively use the following
library(class)

out=knn(train, test, ytrain, k = 5, prob = F)
out
table(out,result)
sum(out!=result)/length(out)

tab = table(ytest,out)
prop.table(tab,1)
prop.table(tab)
1-sum(diag(prop.table(tab))) # misclassification
sum(ytest!=result)/length(y)

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

Nrep = 1 #number of replicates of CV (if doing n-fold, need only 1 replicate)
n=nrow(mydata) 
K = 3  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]

models = seq(1,10,1)

#n iterations , 1.36 per iteraion
Nrep*length(models)*K

#misclass= matrix(0,Nrep*length(lambdas)*length(models),4) # misclass for each model and each replicate
misclass = c()

# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  # repeat CV for each model using the same CV partition
  for (m in models){
    
    yhat = y; # reset yhat (actually don't need this)
    
    # CV for each model
    for (k in 1:K) {
      train = as.matrix(mydata[-Ind[[k]],pred])
      test = as.matrix(mydata[Ind[[k]],pred])
      ytrain = mydata[-Ind[[k]],resp]
      
      # out = ann(train,test,KNN,verbose=F)
      
      # when n-fold, then test has one observation. indices of out$Knn.. are in one row 
      # and become a vector when subset. As.matrix converts this to matrix with n columns
      # equal to number of knn models. So need to transpose or add ncol = KNN and use matrix function.
      # ind = matrix(out$knnIndexDist[,1:KNN],ncol=KNN) # nearest neighbor indices for each obs in test
      
      # phat=apply(ind,1,function(x) sum(ytrain[x]=="Win")/length(ytrain[x]))
      # result = phat
      # result[phat>=0.5] = "Win"
      # result[phat<0.5] = "Other"
      # result = as.factor(result)
      
      out=knn(train, test, ytrain, m, prob = F); result = out
      yhat[Ind[[k]]] = result # prediction = average of y of neighbors
      
    } #end of k loop
    #tab = table(y,yhat)
    #miss = 1-sum(diag(prop.table(tab)))
    #misclass[j,KNN] = miss
    misclass = rbind(misclass,c(j,m,sum(y!=yhat)/length(y)))
    
    
  }# end of KNN loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm


colnames(misclass) = c("rep","Kneighbors","misclass")
misclass
misclassAVE = aggregate(misclass~ Kneighbors, misclass, mean ) # aggregate reps
misclassAVE = misclassAVE[order(misclassAVE[,"Kneighbors"]),] # sort
misclassAVE

param_opt = misclassAVE[which.min(misclassAVE[,"misclass"]),]
param_opt

write.csv(param_opt, file = "P5_best_knn.csv",row.names=FALSE)

par(mfrow=c(1,1))
plot(misclassAVE)

write.csv(misclassAVE, file = "P5_misclassAVE_knn.csv",row.names=FALSE)


#### Part b: GAM ####

mydata=read.table("fgl.txt",sep="\t")
mydata_orig = mydata

z=(mydata$type == "WinF") | (mydata$type == "WinNF")
y=as.character(mydata$type)
y[z]="Win"; y[!z]="Other"
mydata=data.frame(mydata,"type_bin"=as.factor(y))  #add a binary factor response column
y[y == "Win"]=1;y[y == "Other"]=0;
mydata=data.frame(mydata,"type01"=as.numeric(y))  #also add a binary numeric response column

pred = names(mydata)[1:9]
resp = names(mydata)[11]

mydata[pred]=sapply(mydata[pred], function(x) (x-mean(x))/sd(x)) #standardize predictors
train=as.matrix(mydata[,pred]); test=as.matrix(mydata[,pred])
ytrain=mydata[,resp]; ytest=mydata[,resp]

library(mgcv)  #stands for "Mixed GAM Computation Vehicle"

# reduce the degrees of freedom if fewer distinct values than default (10)
#s(gender) # 2
#s(drugs)  # 9
#s(complications # 3

# http://www.talkstats.com/showthread.php/39182-GAM-%28generalised-additive-models%29-function
# http://r.789695.n4.nabble.com/gam-error-td2241518.html
# https://stat.ethz.ch/pipermail/r-help/2007-October/143569.html
# https://stat.ethz.ch/pipermail/r-help/2011-November/295047.html

fList = list()

f= formula(type_bin~ s(RI)+s(Na)+s(Mg)+s(Al)+s(Si)+s(K)+s(Ca)+s(Ba)+s(Fe))
fList = append(fList,f)
npred = length(all.vars(f)[-1])
sp = rep(-1, npred)
out=gam(fList[[1]] , data=mydata, family=binomial(), sp=sp) 

summary(out) # remove s(Ca)

f = update(f, . ~ . -s(Ca)) 
fList = append(fList,f)
npred = length(all.vars(f)[-1])
sp = rep(-1, npred)
out=gam(f ,data=mydata, family=binomial(), sp=sp) 

summary(out) # remove s(Al)

f = update(f, . ~ . -s(Al)) 
fList = append(fList,f)
npred = length(all.vars(f)[-1])
sp = rep(-1, npred)
out=gam(f ,data=mydata, family=binomial(), sp=sp) 

summary(out) # remove s(Ba)

f = update(f, . ~ . -s(Ba)) 
fList = append(fList,f)
npred = length(all.vars(f)[-1])
sp = rep(-1, npred)
out=gam(f ,data=mydata, family=binomial(), sp=sp)

summary(out) # remove s(Na)

f = update(f, . ~ . -s(Na)) 
fList = append(fList,f)
npred = length(all.vars(f)[-1])
sp = rep(-1, npred)
out=gam(f ,data=mydata, family=binomial(), sp=sp)

summary(out) # remove s(K)

f = update(f, . ~ . -s(K)) 
fList = append(fList,f)
npred = length(all.vars(f)[-1])
sp = rep(-1, npred)
out=gam(f ,data=mydata, family=binomial(), sp=sp)

summary(out) # remove s(Fe)

f = update(f, . ~ . -s(Fe))
fList = append(fList,f)
npred = length(all.vars(f)[-1])
sp = rep(-1, npred)
out=gam(f ,data=mydata, family=binomial(), sp=sp)

summary(out) # all sig

out=gam(fList[[7]] ,data=mydata, family=binomial(), sp=sp)

out$sp  ##estimated smoothing parameters for each constituent function 
phat=predict(out, type="response")

yhat = phat
yhat[phat>=0.5] = "Win"
yhat[phat<0.5] = "Other"
yhat = as.factor(yhat)
yhat

y = mydata[,resp]
table(y,yhat)

tab = table(y,yhat)
prop.table(tab,1)
prop.table(tab)
1-sum(diag(prop.table(tab))) # misclassification
sum(y!=yhat)/length(y) # same

plot(yhat,mydata[,resp])  #probably quite a bit of overitting
##
par(mfrow=c(2,4))
plot(out)  #plot component functions
par(mfrow=c(1,1))

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
K = 3  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]

fList = fList[1:7]
models = seq(1,length(fList),1)

#n iterations , 1.36 per iteraion
Nrep*length(models)*K

#misclass= matrix(0,Nrep*length(lambdas)*length(models),4) # misclass for each model and each replicate
misclass = c()

# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  # repeat CV for each model using the same CV partition
  for (m in models){
    
    yhat = y; # reset yhat (actually don't need this)
    npred = length(all.vars(fList[[m]])[-1]) # number of predictors
    sp = rep(-1, npred) # parameter for GAM
    
    # CV for each model
    for (k in 1:K) {
      train = mydata[-Ind[[k]],c(resp,pred)] # doesn't work with matrix input for fit function
      test = mydata[Ind[[k]],pred]
      #ytrain = mydata[-Ind[[k]],resp]
      
      out=gam(fList[[m]] ,data=train, family=binomial(), sp=sp)
      
      phat=predict(out,newdata=test, type="response")
      
      yhat2 = phat
      yhat2[phat>=0.5] = "Win"
      yhat2[phat<0.5] = "Other"
      yhat2 = as.factor(yhat2)
      
      yhat[Ind[[k]]]=yhat2
      
    } #end of k loop
    
    # tab = table(y,yhat)
    # miss = 1-sum(diag(prop.table(tab)))
    # misclass[j,m] = miss
    misclass = rbind(misclass,c(j,m,sum(y!=yhat)/length(y)))
    
  }# end of m loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

colnames(misclass) = c("rep","func","misclass")
misclass
misclassAVE = aggregate(misclass~ func, misclass, mean ) # aggregate reps
misclassAVE = misclassAVE[order(misclassAVE[,"func"]),] # sort
misclassAVE

param_opt = misclassAVE[which.min(misclassAVE[,"misclass"]),]
param_opt

write.csv(param_opt, file = "P5_best_gam.csv",row.names=FALSE)

par(mfrow=c(1,1))
plot(misclassAVE)

write.csv(misclassAVE, file = "P5_misclassAVE_gam.csv",row.names=FALSE)


#### Part c:  NNET ####


## Read data, convert response to binary, and standardize predictors

mydata = read.table("fgl.txt",sep="\t")
z = (mydata$type == "WinF") | (mydata$type == "WinNF")
y = as.character(mydata$type)
y[z] = "Win"; y[!z] = "Other"
mydata = data.frame(mydata,"type_bin"=as.factor(y))  #add a binary factor response column
y[y == "Win"] = 1;y[y == "Other"] = 0;
mydata = data.frame(mydata,"type01"=as.numeric(y))  #also add a binary numeric response column

pred = names(mydata)[1:9]
resp = names(mydata)[11]

mydata[pred]=sapply(mydata[pred], function(x) (x-mean(x))/sd(x)) #standardize predictors
train=as.matrix(mydata[,pred]); test=as.matrix(mydata[,pred])
ytrain=mydata[,resp]; ytest=mydata[,resp]

## fit nnet

library(nnet)
mydata.nn1 = nnet(type_bin~., mydata[,c(pred,resp)], 
                  linout=F, skip=F, size=10, decay=.05, maxit=1000, trace=F)

phat = as.numeric(predict(mydata.nn1))
y = mydata[[12]] 
yhat = as.numeric(phat >= 0.5)  #classify as 1 if predicted probability >= 0.5
sum(y != yhat)/length(y)  #misclassification rate
summary(mydata.nn1)
plot(phat,jitter(y,0.05))

# alternative approach
y = mydata[,resp]
yhat = predict(mydata.nn1,type="class")
sum(y != yhat)/length(y)

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

Nrep = 20  #number of replicates of CV (if doing n-fold, need only 1 replicate)
n=nrow(mydata) 
K = 3  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]

models = seq(1,15,1)
lambdas = seq(.1,2,.1)

#n iterations , 1.36 per iteraion
Nrep*length(lambdas)*length(models)*K

#misclass= matrix(0,Nrep*length(lambdas)*length(models),4) # misclass for each model and each replicate
misclass = c()

# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  # repeat CV for each model using the same CV partition
  for (m in models){
    
    yhat = y; # reset yhat (actually don't need this)
    
    for(l in lambdas){
      
      # CV for each model
      for (k in 1:K) {
        train = mydata[-Ind[[k]],c(resp,pred)] # does not work if you convert to 
        # matrix and then to data frame
        test = mydata[Ind[[k]],pred]
        #ytrain = mydata[-Ind[[k]],resp]
        
        out = nnet(type_bin~., data =train, 
                   linout=F, skip=F, size=m, decay=l, maxit=1000, trace=F)
        
        yhat[Ind[[k]]] = predict(out, newdata = test,type="class")
        
      } #end of k loop
      
      #tab = table(y,yhat)
      #miss = 1-sum(diag(prop.table(tab)))
      #misclass[j,KNN] = miss
      misclass = rbind(misclass,c(j,m,l,sum(y!=yhat)/length(y)))
      
    } # end of l loop
  }# end of m loop
  
} #end of j loop

# Stop the clock
proc.time() - ptm

colnames(misclass) = c("rep","nodes","lambda","misclass")
misclass
misclassAVE = aggregate(misclass~ nodes + lambda, misclass, mean ) # aggregate reps
misclassAVE = misclassAVE[order(misclassAVE[,"nodes"]),] # sort
misclassAVE

param_opt = misclassAVE[which.min(misclassAVE[,"misclass"]),]
param_opt

write.csv(param_opt, file = "P5_best_nnet.csv",row.names=FALSE)

par(mfrow=c(1,1))
plot(misclassAVE)

write.csv(misclassAVE, file = "P5_misclassAVE_nnet.csv",row.names=FALSE)

#### Part c: KNN,GAM vs NNET ####

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
K = 3  #K-fold CV on each replicate (n-fold)
y = mydata[,resp]

fBest = 6
kBest = 3
lambdaBest = 0.4
nodeBest = 9


#n iterations , 1.36 per iteraion
Nrep*K

#misclass= matrix(0,Nrep*length(lambdas)*length(models),4) # misclass for each model and each replicate
misclass = c()

# repeat CV  Nrep replicates
for (j in 1:Nrep) {
  Ind = CVInd(n,K) # generate CV partition indices
  
  yhat_KNN = y; # reset yhat (actually don't need this)
  yhat_GAM = y; # reset yhat (actually don't need this)
  yhat_NNET = y; # reset yhat (actually don't need this)
  
  npred = length(all.vars(fList[[fBest]])[-1]) # number of predictors
  sp = rep(-1, npred) # parameter for GAM
  
  # CV for each model
  for (k in 1:K) {
    train = mydata[-Ind[[k]],c(resp,pred)] # doesn't work with matrix input for fit function
    test = mydata[Ind[[k]],pred]
    ytrain = mydata[-Ind[[k]],resp]
    
    # KNN (don't know why knn needs same number of cols in test and train)
    out=knn(train[,c(pred)], test, ytrain, kBest, prob = F); result = out
    yhat_KNN[Ind[[k]]] = result # prediction = average of y of neighbors
    
    
    # GAM
    out=gam(fList[[fBest]] ,data=train, family=binomial(), sp=sp)
    phat=predict(out,newdata=test, type="response")
    yhat2 = phat
    yhat2[phat>=0.5] = "Win"
    yhat2[phat<0.5] = "Other"
    yhat2 = as.factor(yhat2)
    yhat_GAM[Ind[[k]]]=yhat2
    
    # NNET
    out = nnet(type_bin~., data =train, 
               linout=F, skip=F, size=nodeBest, decay=lambdaBest, maxit=1000, trace=F)
    
    yhat_NNET[Ind[[k]]] = predict(out, newdata = test,type="class")
    
    
    
    
  } #end of k loop
  
  # tab = table(y,yhat)
  # miss = 1-sum(diag(prop.table(tab)))
  # misclass[j,m] = miss
  
  misclass = rbind(misclass,c(j,sum(y!=yhat_KNN)/length(y),
                              sum(y!=yhat_GAM)/length(y),
                              sum(y!=yhat_NNET)/length(y)))
  
  
} #end of j loop

# Stop the clock
proc.time() - ptm

colnames(misclass) = c("rep","KNN","GAM","NNET")
misclass

misclassAVE = apply(misclass,2,mean)[-1]
misclassAVE

model_opt = which.min(misclassAVE)
model_opt

write.csv(model_opt, file = "P5_best.csv",row.names=FALSE)

par(mfrow=c(1,1))
plot(misclassAVE)

write.csv(misclassAVE, file = "P5_misclassAVE_best.csv",row.names=FALSE)
