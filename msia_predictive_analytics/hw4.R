
# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_420_Predictive_Analytics"
datafolder = "Data"
setwd(file.path(main,course, datafolder))

filename = "HW4_data.csv"
mydata = read.csv(filename,header = T)

#1

#(a)Use an appropriate moving average filter to smooth out the seasonality in the data.
#centered MA
y=ts(mydata[[1]], frequency=1)  
m=12;k=24;n=length(y)  #m = MA window length, k = prediction horizon
plot(y,type="b",xlim=c(0,n+k))
MApass=filter(y, filter=rep(1/m,m), method = "convolution", sides = 2)
yhat=c(NA,MApass,rep(MApass[n],k-1))  #fits and forecasts
lines(yhat,col="red")

#b
y<-ts(mydata[[1]], frequency=1)
alpha=0.9;k=24;n=length(y)  #m = MA window length, k = prediction horizon
plot(y,type="b",xlim=c(0,n+k))
EWMA<-alpha*filter(y, filter=1-alpha, method = "recursive", sides = 1, init=y[1]/alpha)
yhat=c(NA,EWMA,rep(EWMA[n],k-1))
lines(yhat,col="red")

#repeat for alpha = 1, 0.2, 0.05
# loop over minize MAPE, etc
# EWMAHW4<-i*filter(y, filter=1-i, method = "recursive", sides = 1, init=y[1]/i) 

#(b) Calculate and plot EWMA forecasts for the next two years (1961 and 1962). What is the optimal EWMA parameter ??? Discuss the nature of the k-step-ahead
#forecasts for this optimal ?? value.
alpha=0.05;k=24;n=length(y)  #m = MA window length, k = prediction horizon
plot(y,type="b",xlim=c(0,n+k))
EWMApass=HoltWinters(y, seasonal = "additive", beta = FALSE, gamma = FALSE) 
EWMApassPred=predict(EWMApass, n.ahead=k, prediction.interval = T, level = 0.95)
plot(EWMApass,EWMApassPred,type="b")
EWMApass


#(c) Calculate and plot the Holt method forecasts for the next two years (1961 and
#1962). What optimal Holt parameters ?? and ??? Discuss the nature of the k-step-
#ahead forecasts and how they differ from the EWMA forecasts in part (b).

##let algorithm find optimal alpha and beta
k=24;n=length(y)  #k = prediction horizon
Holt=HoltWinters(y, gamma = FALSE) 
HoltPred=predict(Holt, n.ahead=k, prediction.interval = T, level = 0.95)
plot(Holt,HoltPred,type="b",ylim=c(1,750))
Holt

##repeat, but specifying alpha and larger beta
Holt=HoltWinters(y, seasonal = "additive", alpha=.4, beta=.3,gamma = FALSE, l.start=17, b.start=.03) 
HoltPred=predict(Holt, n.ahead=k, prediction.interval = T, level = 0.95)
plot(Holt,HoltPred,type="b",ylim=c(1,750))

##repeat, but specifying alpha and smaller beta
Holt=HoltWinters(y, seasonal = "additive", alpha=.4, beta=.05,gamma = FALSE, l.start=17, b.start=.03) 
HoltPred=predict(Holt, n.ahead=k, prediction.interval = T, level = 0.95)
plot(Holt,HoltPred,type="b",ylim=c(1,750))


#(d) Calculate and plot the Holt-Winters forecasts for the next two years (1961 and 1962) for an additive model. What optimal Holt-Winters parameters ??, ??, and ???
#Interpret the seasonality coefficients.
y=ts(mydata[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period. Could instead specify frequency = 12
k=24;n=length(y)  #k = prediction horizon
HW=HoltWinters(y, seasonal = "additive") 
HWPred=predict(HW, n.ahead=k, prediction.interval = T, level = 0.95)
plot(HW,HWPred,type="b",ylim=c(100,750))
HW

#(e)Repeat part (d) but for a Holt-Winters multiplicative model?
y=ts(mydata[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period. Could instead specify frequency = 12
k=24;n=length(y)  #k = prediction horizon
HW=HoltWinters(y, seasonal = "multiplicative") 
HWPred=predict(HW, n.ahead=k, prediction.interval = T, level = 0.95)
plot(HW,HWPred,type="b",ylim=c(100,750))
HW

#2
#(a) Fit an additive decomposition model to the data. Interpret the trend and 
#seasonality indices. Construct a plot of the original time series and the 
#fitted values, both on the same plot. Discuss the extent to which the variability
#in the data are accounted for by the trend and seasonality.
y=ts(mydata[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period
k=24;n=length(y)  #k = prediction horizon
Dec=decompose(y, type = "additive")
plot(Dec,type="b")
Dec
##
y_hat=Dec$trend+Dec$seasonal 
plot(y,type="b")
lines(y_hat,col="red")
lines(y,col="blue")

#(b) Repeat for a multiplicative decomposition model. Which model ??? additive or multiplicative 
#??? do you think better represents the data?
y=ts(HW4_data[[1]], deltat=1/12)  #sampling interval corresponds to 1/12 the seasonality period
k=24;n=length(y)  #k = prediction horizon
Dec=decompose(y, type = "multiplicative")
plot(Dec,type="b")
Dec
##
y_hat=Dec$trend+Dec$seasonal 
plot(y,type="b")
lines(y_hat,col="red")
lines(y,col="blue")