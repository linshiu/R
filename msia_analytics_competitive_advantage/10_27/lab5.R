

# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015_NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/10_27"
setwd(file.path(main,course, datafolder))
filename = 'whitewine.txt'

wine <-read.delim(filename);
y = wine[,1];
x = wine[,2:length(wine[1,])];


# Stepwise ####

library(MASS)
reg= lm(y~., data=x)
reg.step= stepAIC(object=reg, direction="both")

# Direction = forward/backward

summary(reg.step)

formula(reg.step); # print the formula of the model
AIC(reg.step); # print AIC value of the model
summary(reg.step)$r.squared; # print r^2value of the model
summary(reg.step)$adj.r.squared; # print adjusted r^2value
e = resid(reg.step); # define residuals
SSE = sum(e^2); # calculate Sum of Squared errors
SAE = sum(abs(e)); # calculate Sum of Absolute errors

# Variable Selection Using Package ####
library(leaps)

# find the best subset

reg.exh= regsubsets(x,y, nbest=1, nvmax=length(y), method="exhaustive");
summary(reg.exh)
plot(reg.exh)
plot(reg.exh, scale="adjr2")
plot(leaps, scale="bic")

summary(reg.exh)$which
summary(reg.exh)$cp
summary(reg.exh)$adjr2
cbind(summary(reg.exh)$which, summary(reg.exh)$cp, summary(reg.exh)$adjr2)

# optimizing various criteria

leaps(x,y,nbest=1,method="Cp")
leaps(x,y,nbest=1,method="adjr2")

# Piecewise Regression ####
filename = "height2.txt"

height2 <-read.delim(filename);

mht= height2[,1];
fht= height2[,2];

reg.seg= lm(fht~ (mht<1.73)*mht)
summary(reg.seg)

# Piecewise Regression Using Package ####
# install.packages("segmented")
library(segmented)
reg.ht = lm(fht~ mht)
reg.seg1 = segmented(reg.ht, seg.Z= ~mht, psi=1.73)
summary(reg.seg1)

plot(height2)
plot(reg.seg1, add=T)

plot(height2)
abline(coef(reg.ht))

# piecewise with multiple points

reg.ht = lm(fht~ mht)
reg.seg1 = segmented(reg.ht, seg.Z= ~mht, psi=1.73)
reg.seg2 = segmented(reg.ht, seg.Z= ~mht, psi=c(1.65,1.73))
summary(reg.seg1)
summary(reg.seg2)
