# Setup ####

# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015_NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/10_06"
setwd(file.path(main,course, datafolder))

height <- read.table("height.txt", header=T)

# Mean  ####
M = height$Male;
F = height$Female;
n = length(F);
mean(M);# equivalent to mean(height[,1])or mean(height[, "Male"])
mean(F);

# Variance  ####

var(M); # sample variance
var(F); # sample variance
(n-1)/n*var(M); #population variance if we assume data is from population
(n-1)/n*var(F); # population variance if we assume data is from population
sd(F); # standard deviation

cov(M,F);

# Histogram  ####

hist(M);
hist(F);
hist(F, breaks=4); # Try
hist(F, breaks=seq(1.40,1.75,by=0.03)) # Try

# Confidence Interval ####

# known population variance
sigma = 0.0036; Mbar = mean(M);
E = qnorm(0.975)*sigma/sqrt(n); # margin of error
CI_M = Mbar + c(-E,E);

# unknown population variance
S = sd(M);
E = qt(0.975, df=n-1)*S/sqrt(n); # margin of error
CI_M = Mbar + c(-E,E);
     
# Hypothesis Testing ####

# Two tailed test with known variance
z = (Mbar - 1.729)/(sigma/sqrt(n));
z.half.alpha= qnorm(1-0.05/2);
c(-z.half.alpha, z.half.alpha);# check if z is inside the interval

# Lower-tailed test with known variance
z = (Mbar - 1.729)/(sigma/sqrt(n));
z.alpha= -qnorm(1-0.05);

# Two tailed test with unknown variance

t= (Mbar - 1.729)/(sd(M)/sqrt(n));
t.half.alpha= qt(1-0.05/2, df=n-1);
c(-t.half.alpha, t.half.alpha); # check if t is inside the interval

# ANOVA
Tensile = read.table("tensile.txt", header=T)

# Turn the data into a single vector
resp= c(t(as.matrix(Tensile)));

#Setup values
treats = c("HC5","HC10","HC15","HC20");

k = 4;
n = 6;

# Create a vector of treatment factors that corresponds to each element of resp

tm = gl(k,1,n*k,factor(treats))

# gl(): generate levels
# (# levels, # replications, length, labels)

myANOVA= aov(resp ~ tm);
summary(myANOVA);
anova(Tensile)
qf(1-0.01,df1=3,df2=20)

#Samples from Normal distribution

x.norm= rnorm(n=200,m=10,sd=2); # Generate 200 random samples from N(10,2)
hist(x.norm,breaks= 10,main="Histogram of observed data");
plot(density(x.norm),main="Density estimate of data");

# Comparisons
h = hist(x.norm,breaks=10);
x.hist= c(min(h$breaks),h$breaks); y.hist= c(0,h$density,0);
x.fit= seq(min(x.norm),max(x.norm),length=40);
y.fit= dnorm(x.fit,mean=mean(x.norm),sd=sd(x.norm));
plot(x.hist,y.hist,type="s",ylim=c(0,max(y.hist,y.fit)), main="Normal pdfand histogram")
lines(x.fit,y.fit, col="red");
plot(density(x.norm),main="Density estimate of data")
lines(x.fit,y.fit, col="red")

z.norm= (x.norm-mean(x.norm))/sd(x.norm);
qqnorm(z.norm);
abline(0,1, col ="red");

# alternative
g = d$mydata
m<-mean(g)
std<-sqrt(var(g))
hist(g, density=20, breaks=20, prob=TRUE, 
     xlab="x-variable", ylim=c(0, 2), 
     main="normal curve over histogram")
curve(dnorm(x, mean=10, sd=2), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

# alternative

hist(x.norm, density=10, breaks=20, prob=TRUE, 
     xlab="x-variable", 
     main="normal curve over histogram")
curve(dnorm(x, mean=10, sd=2), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

