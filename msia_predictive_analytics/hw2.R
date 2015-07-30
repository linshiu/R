
# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_420_Predictive_Analytics"
datafolder = "Data"
setwd(file.path(main,course, datafolder))

filename = "HW2_data.csv"
mydata = read.csv(filename,header = T)

#### Problem 1 ##############################################

head(mydata)
mydata = mydata[-1]
head(mydata) # drop ID column

# convert rsponse to log10 (change 0 to 1)
hist(mydata$total_cost)
mydata$total_cost[mydata$total_cost == 0] = 1
mydata$total_cost = log10(mydata$total_cost)
hist(mydata$total_cost)

### Part a

##linear regression with all predictors included

fit = lm(total_cost~.,mydata)
summary(fit)$r.sq # 0.584

# The model does not fit the data well in terms of predictive
# power because of the low R2, which idicates that the model
# explains less than 60% of the variation in the response.

str(fit)
methods(class = lm)
names(summary(fit))


##linear regression including interactions
fit2 = lm(total_cost ~ .^2,data=mydata)
summary(fit2)

##stepwise linear regression
fit0 =lm(total_cost ~ 1,data=mydata)

fitStep = step(fit0, scope=formula(fit2), direction="both", trace=0)
summary(fitStep)

fit_lm = lm(total_cost ~ (. - age - drugs)^2, data = mydata)
fitStep = step(fit0, scope=formula(fit_lm), direction="both", trace=0)
summary(fitStep)
fit_lm = fitStep


#######A function to determine the indices in a CV partition##################
CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m = floor(n/K)  #approximate size of each part
  r = n-m*K  
  I = sample(n,n)  #random reordering of the indices
  Ind = list()  #will be list of indices for all K parts
  length(Ind) = K
  for (k in 1:K) {
    if (k <= r) kpart = ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart = ((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] = I[kpart]  #indices for kth part of data
  }
  Ind
}


##Now use the same CV partition to compare Neural Net and linear reg models###
sdreps = matrix(0,0,1) #run this line once. Run the remaining lines multiple replicates

colnames(sdreps) = c("R2_avg")

maxReps = 10

# Start the clock!
ptm <- proc.time()
    
results = c()

for (rep in 1:maxReps){
  Ind = CVInd(n=nrow(mydata1),10)
  K = length(Ind)
  y = mydata1$total_cost
  yhat = y
  
  for (k in 1:K) {
    out = lm(formula(fit_lm), data= mydata1[-Ind[[k]],] )
    yhat[Ind[[k]]] = as.numeric(predict(out,mydata1[Ind[[k]],]))
  }
  
  e=y-yhat
  R2 = 1 - var(e)/var(y)
  results = c(results,R2)
  
}
R2_avg = mean(results)
sdreps = rbind(sdreps,c(R2_avg)) 
sdreps  

# Stop the clock
proc.time() - ptm

sdreps[which.max(sdreps[,"R2_avg"]),]

### Part b
# Fit linear regression
library(car)
vif(fit) # no multicollinearity problem

summary(fit)
# the statistical significant variables are:
# interventions, ER visits, complications,
# comorbidities and duration.

### Part c
# Diagnostics
par(mfrow=c(2,3))
for (i in 1:6){
  
  plot.lm(fit,which=i)
}

corr = round(cor(mydata),2)

library(corrplot)
corrplot.mixed(corr, upper = "ellipse", lower = "number")


# Assumptions about the predictors
# prefer cook distance since both x and y space

## check the fit (check linearity assumption by plotting residuals against each predictor)


plot_vector = vector(mode="list",length=6)

plot_vector[[1]] = ggplot(mydata,aes(x=mydata[[2]], y = fit$resid)) + 
  geom_point(size = 3) +
  labs(x = colnames(mydata[2]),y = "Residuals")

plot_vector[[2]]  = ggplot(mydata,aes(x=mydata[[3]], y = fit$resid)) + 
  geom_point(size = 3) +
  labs(x = colnames(mydata[3]),y = "Residuals")

plot_vector[[3]]  = ggplot(mydata,aes(x=mydata[[4]], y = fit$resid)) + 
  geom_point(size = 3) +
  labs(x = colnames(mydata[4]),y = "Residuals")

plot_vector[[4]]  = ggplot(mydata,aes(x=mydata[[5]], y = fit$resid)) + 
  geom_point(size = 3) +
  labs(x = colnames(mydata[5]),y = "Residuals")

plot_vector[[5]]  = ggplot(mydata,aes(x=mydata[[6]], y = fit$resid)) + 
  geom_point(size = 3) +
  labs(x = colnames(mydata[6]),y = "Residuals")

plot_vector[[6]] = ggplot(mydata,aes(x=mydata[[7]], y = fit$resid)) + 
  geom_point(size = 3) +
  labs(x = colnames(mydata[7]),y = "Residuals")

grid.arrange(plot_vector[[1]],
             plot_vector[[2]],
             plot_vector[[3]],
             plot_vector[[4]],
             plot_vector[[5]],
             plot_vector[[6]],
             ncol=2, main = "Residuals vs Predictor Variables")

# All plots look random so assumptions about the form of the model 
# (linear in the regression parameters) is satisfied. 

## Can also do the followig:
## check the fit (check linearity assumption by plotting partail regression/added variable plot)
library(car)
avPlots(fit)

## check normality (using qq plot)
qqPlot(fit, main = "Normal Q-Q Plot")

# Alternative code:
fit_stdres = rstandard(fit)

# To get studentized residuals
library(MASS)
stu_res = studres(fit)

qqnorm(fit_stdres,
       ylab = "Standardized Residuals",
       xlab = "Theoretical Quantiles",
       main = "Normal Q-Q Plot");                
qqline(fit_stdres, col="red") 

# formal test: Anderson-Darling test, Shapiro-Wilk test, Kolomogorov-Smirnov

# The plot shows that most points fall along the line, indicating the normality
# assumption of errors is satisfied. However, it looks that there are a fwew outliers

## check Checking Homoscedasticity (using residuals vs. fitted)

ggplot(mydata,aes(x=fit$fitted, y = fit$resid)) + 
  geom_point(size = 3) +
  labs(x = "Fitted",y = "Residuals")

# The plot shows data points are random forming a parallel band, indicating the 
# common variance assumption of errors is valid (Homoscedasticity)

## check independence (not time series data)
# the Durbin-Watson

## Check multicollinearity

corr = round(cor(mydata[-1]),2)
corr

# Plot combine correlation coefficients matrix and scatter plot
# http://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/iris_plots/
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(cor(x,y), digits=2)) 
}

pairs(mydata, main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, upper.panel=panel.pearson,lower.panel = panel.smooth)

# Plot separate
par(mfrow=c(1,1))
corrplot(corr,method="number", type="upper")
pairs(mydata[,-1], main = "Correlation coeffficients matrix and scatter plot", 
      pch = 21, lower.panel = NULL, panel = panel.smooth)

# Check 
library(car)
vif(fit)

# The VIF < 10 for all predictors, so there is no multicollinearity problem. 


#### Problem 2 ##############################################

# Use 10-fold cross-validation to find the best combination of # shrinkage parameter lambda and number of hidden nodes

k = ncol(mydata)  #number of columns (response is in first column)
mydata1 = mydata  #will be standardized and scaled version of data
mydata1[1:k] = sapply(mydata1[1:k], function(x) (x-mean(x))/sd(x)) #standardize predictors and need to standardize response 


pairs(mydata1, cex=.5, pch=16)

#######A function to determine the indices in a CV partition##################
CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m = floor(n/K)  #approximate size of each part
  r = n-m*K  
  I = sample(n,n)  #random reordering of the indices
  Ind = list()  #will be list of indices for all K parts
  length(Ind) = K
  for (k in 1:K) {
    if (k <= r) kpart = ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart = ((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] = I[kpart]  #indices for kth part of data
  }
  Ind
}

library(nnet)
##Now use the same CV partition to compare Neural Net and linear reg models###
sdreps = matrix(0,0,3) #run this line once. Run the remaining lines multiple replicates
colnames(sdreps) = c("size","lambda","R2_avg")

maxSize = 6
minSize = 2
lambdas = seq(0.05,0.1, by =0.01)
#lambdas = c(0.1)
maxReps = 1

# Start the clock!
ptm <- proc.time()

for (node in minSize:maxSize){
  for (lambda in lambdas){
    
    results = c()
    
    for (rep in 1:maxReps){
      Ind = CVInd(n=nrow(mydata1),10)
      K = length(Ind)
      y = mydata1$total_cost
      yhat = y
      
      for (k in 1:K) {
        out = nnet(total_cost~.,mydata1[-Ind[[k]],], 
                   linout=T, skip=F, size=node, decay=lambda, maxit=1000, trace=F)
        yhat[Ind[[k]]] = as.numeric(predict(out,mydata1[Ind[[k]],]))
      }
      
      e=y-yhat
      R2 = 1 - var(e)/var(y)
      results = c(results,R2)
      
    }
    R2_avg = mean(results)
    sdreps = rbind(sdreps,c(node,lambda,R2_avg)) 
  }
  
}

# Stop the clock
proc.time() - ptm

sdreps[which.max(sdreps[,"R2_avg"]),]

best = sdreps[which.max(sdreps[,"R2_avg"]),]
best # size =2, lambda = 0.08

write.table(sdreps,"Hw2_problem2.csv", sep=",")

# fit model 

fitNNET= nnet(total_cost~.,mydata1, 
              linout=T, skip=F, size=best["size"], decay=best["lambda"], maxit=1000, trace=F)

# output
summary(fitNNET)

# residual plots
y = mydata1$total_cost
yhat = y
yhat = as.numeric(predict(fitNNET,mydata1))
e=y-yhat
plot(e~yhat,xlab="Fitted Values",ylab="Residuals")


sdreps; apply(sdreps,2,mean)


#### Problem 3 ##############################################

#do not have to standardize or transform predictors to fit trees
library(tree)
control = tree.control(nobs=nrow(mydata), mincut = 5, minsize = 10, mindev = 0.002)
#default is mindev = 0.01, which only gives a 10-node tree
mydata.tr = tree(total_cost ~ .,mydata,control=control)
mydata.tr
summary(mydata.tr)
plot(mydata.tr,type="u"); text(mydata.tr,digits=2)  #type="p" plots proportional branch lengths
######now prune tree and plot deviance vs. complexity parameter
mydata.tr1 = prune.tree(mydata.tr)
plot(mydata.tr1)
######now plot CV deviance vs complexity parameter

minList = c()
for (i in 1:100){
  cvtree = cv.tree(mydata.tr, , prune.tree,K=10)
  #plot(cvtree) # K = 10, 10 fold
  minCurrent = min(cvtree$size[min(cvtree$dev)==cvtree$dev])
  #which.min(cvtree$dev)
  
  
  minList = c(minList,minCurrent)
}

methods(class = tree)

# cross validation with best size

prune.tree(tree_cv, best = bestSize)
tree_


hist(minList)
table(minList)

######now find the final tree with the best value of complexity parameter
mydata.tr1 = prune.tree(mydata.tr, best = 8) #can replace replace argument "k=2800" by "best=13"
mydata.tr1
plot(mydata.tr1,type="u");text(mydata.tr1,digits=3)




#### Problem 4 ##############################################

######Read data, convert response to binary, and standardize predictors#####
FGL = read.table("fgl.txt",sep="\t")

z = (FGL$type == "WinF") | (FGL$type == "WinNF")
y = as.character(FGL$type)
y[z] = "Win"; y[!z] = "Other"
FGL = data.frame(FGL,"type_bin"=as.factor(y))  #add a binary factor response column
y[y == "Win"] = 1;y[y == "Other"] = 0;
FGL = data.frame(FGL,"type01"=as.numeric(y))  #also add a binary numeric response column
FGL1 = FGL
k = ncol(FGL)-3;

FGL1[1:k] = sapply(FGL1[1:k], function(x) (x-mean(x))/sd(x)) #standardize predictors
FGL
#############Fit a neural network classification model to the FGL1 data######
library(nnet)
fgl.nn1 = nnet(type_bin~., FGL1[,c(1:9,11)], linout=F, skip=F, size=10, decay=.05, maxit=1000, trace=F)
phat = as.numeric(predict(fgl.nn1))
y = FGL1[[12]] 
yhat = as.numeric(phat >= 0.5)  #classify as 1 if predicted probability >= 0.5
sum(y != yhat)/length(y)  #misclassification rate
summary(fgl.nn1)
plot(phat,jitter(y,0.05))

## Using CV to Compare Models for the FGL Data

Ind = CVInd(n=nrow(FGL1),10); K<-length(Ind)
y = FGL1[[12]]; yhat = y
for (k in 1:K) {
  out = nnet(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],linout=F,skip=F,size=10,decay=.3, maxit=1000,trace=F)
  phat = as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)])) 
  yhat[Ind[[k]]] = as.numeric(phat >= 0.5)
}
CVrate1 = sum(y != yhat)/length(y)  #CV misclassification rate
#now compare to a different neural net model
yhat = y
for (k in 1:K) {
  out = nnet(type_bin~.,FGL1[-Ind[[k]],c(1:9,11)],linout=F,skip=F,size=10,decay=0, maxit=1000,trace=F)
  phat = as.numeric(predict(out,FGL1[Ind[[k]],c(1:9,11)])) 
  yhat[Ind[[k]]] = as.numeric(phat >= 0.5)
}
CVrate2 = sum(y != yhat)/length(y)  #CV misclassification rate
c(CVrate1,CVrate2)

#############Same, but use the original 6-category response######
library(nnet)
fgl.nn1 = nnet(type~.,FGL1[,c(1:10)],linout=F,skip=F,size=14,decay=.05,maxit=1000,trace=F)
##output the class probabilities
phat = predict(fgl.nn1,type="raw")
phat[1:20,]
apply(phat,1,sum)  #you can see that the 6 predicted class probabilities sum to 1.0
##output the class with the largest class probability
yhat = predict(fgl.nn1,type="class")
yhat
y = FGL1$type 
sum(y != yhat)/length(y)  #training misclassification rate

Ind = CVInd(n=nrow(FGL1),10); K<-length(Ind)

for (k in 1:K){
  out = nnet(type~.,FGL1[-Ind[[k]],c(1:10)],linout=F,skip=F,size=16,decay=.05,maxit=1000,trace=F)  
  yhat[Ind[[k]]]  = predict(out,type="class", newdata=FGL1[Ind[[k]],c(1:10)] )
  y = FGL1$type 
}

sum(y != yhat)/length(y)  #training misclassification rate

#### Tree

## original 6-category response

minList = c()
for (i in 1:100){
  cvtree = cv.tree(fgl.tr, , prune.tree,K=10)
  #plot(cvtree) # K = 10, 10 fold
  minCurrent = min(cvtree$size[min(cvtree$dev)==cvtree$dev])
  #which.min(cvtree$dev)
  
  
  minList = c(minList,minCurrent)
}

hist(minList)

results = c()

for (i in 1:10){
  Ind = CVInd(n=nrow(FGL),10); K<-length(Ind)
  for (k in 1:K){
    control = tree.control(nobs=nrow(FGL), mincut = 5, minsize = 10, mindev = 0.005)
    fgl.tr = tree(type~.,FGL[-Ind[[k]],c(1:10)],control=control)
    fgl.tr1 = prune.tree(fgl.tr, best=5) 
    yhat[Ind[[k]]]  = predict(fgl.tr1,type="class", newdata=FGL[Ind[[k]],c(1:10)] )
    y = FGL$type 
  }
  
  m = sum(y != yhat)/length(y)  #training misclassification rate
  results = c(results,m)
  
}
mean(results)


#### Multinomial
results = c()

y = FGL$type 
yhat = y 

for (i in 1:20){
  Ind = CVInd(n=nrow(FGL),10); K<-length(Ind)
  for (k in 1:K){
    fit = multinom(type~.,data=FGL[-Ind[[k]],c(1:10)],trace=FALSE)
    yhat[Ind[[k]]]  = predict(fit,type="class", newdata=FGL[Ind[[k]],c(1:10)] )
    
  }
  
  m = sum(y != yhat)/length(y)  #training misclassification rate
  results = c(results,m)
  
}
mean(results)