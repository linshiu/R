#########################
# STAT 425
# Homework 6
# Date: 11/18/11
# Author: Luis Lin
#########################

###Install Packages - needed depending on computer

install.packages("lmtest", lib="E:/R")
library("zoo",lib.loc="E:/R")
library("lmtest",lib.loc="E:/R")

install.packages("nlme", lib="E:/R")
library("nlme",lib.loc="E:/R")

install.packages("leaps",lib="E:/R")
library("leaps",lib.loc="E:/R")

install.packages("faraway", lib="E:/R")
library("faraway",lib.loc="E:/R")

install.packages("MASS", lib="E:/R")
library("MASS",lib.loc="E:/R") 

getwd()
setwd("E:/R")

library(faraway)
library(MASS)

###########################################################
###PROBLEM 1

# Data :
# A data frame with 54 observations on 3 variables. 
#[,1]  breaks  numeric  The number of breaks 
#[,2]  wool  factor  The type of wool (A or B) 
#[,3]  tension  factor  The level of tension (L, M, H)  


### Part a: Use the Box-Cox method to determine an appropriate transformation on the re-sponse.

attach(warpbreaks)
names(warpbreaks)
g=lm(breaks ~ wool*tension)
breaks.trans=boxcox(g, lambda=seq(-2, 2, length=400))
round(breaks.trans$x[breaks.trans$y == max(breaks.trans$y)],3)
tmp=breaks.trans$x[breaks.trans$y > max(breaks.trans$y) - qchisq(0.95, 1)/2];
CI=range(tmp) # 95% CI.
round(CI,3)
1>CI[1] & 1<CI[2] # Check contains 1 
0>CI[1] & 0<CI[2] # Check contains 0

# Since 1 is not contaited in the CI, transformation needed
# Since 0 is contained in the CI, choose natural log transformation

### Part b: Determine which factors (including interactions) are signicant.

newbreaks = log(breaks)
g=lm(newbreaks ~ wool*tension)
anova(g)

# Significant factors: p-value < 0.05
# The Main effect tension and interaction wool-tension are significant.
# The main effect wool is not significant.

### Part c: Now form a six-level factor from all combinations of the wool and tension factors.
### Which combinations|there are totally 15 pairs are signicantly different?

tky = TukeyHSD(aov(g)); tky
names(tky)
comb.names = row.names(tky$`wool:tension`);
sig=tky$`wool:tension`[,4]<0.05
comb.names[sig]

# Signficant different pairs: Look at p-value <0.05 or CI that doesn't include zero
# Combinations of wool and tension "A:M-A:L" , "A:H-A:L",  "B:H-A:L" are signficantly different

###########################################################
###PROBLEM 2

library(lattice)
names(barley)
?barley

### Part a: Provide a graphical display of the data

dotplot(variety ~ yield | site, data = barley, groups = year,  key = simpleKey(levels(barley$year), space = "right"),
xlab = "Barley Yield (bushels/acre) ", aspect=0.5, layout = c(2,3), ylab=NULL)

newbarley=barley
newbarley$year[newbarley$site=="Morris"]=ifelse(newbarley$year[newbarley$site=="Morris"]==1931, 1932, 1931)

dotplot(variety ~ yield | site, data = newbarley, groups = year, key = simpleKey(levels(newbarley$year), space = "right"),  
xlab = "Barley Yield (bushels/acre) ", aspect=0.5, layout = c(2,3), ylab=NULL, font=0.5)

### Part b: Perform a three-way ANOVA with yield as the response. Include all the two-way
### interactions, but no three-way interactions since we have only one observation in
### each three-way combination. Provide the ANOVA table, and comment on your result.

g=lm(yield~variety+year+site+variety:year+variety:site+year:site, data=newbarley)
#g=lm(yield~(variety+year+site)^2, data=newbarley) # alternative command

anova(g)
round(summary(g)$r.sq,3)

# Significant factors: p-value < 0.05
# Significant factors: main effects "variety", "year", "site", and
# intercations "variety:site", "year:site"

### Part c: Check the diagnostics | you will find that two points, the 23rd and the 83rd
### samples, stick out. They are the two with the highest residuals (in absolute value).
### Check these two points. Which site or sites are they from? Which year or years are they from?

plot(g)

# Samples 23 and 83 have the highest residuals (in absolute value)
# No evidence of non-constant variance 
# QQ shows normality of residuals, excluding samples 23 and 83

newbarley[c(23, 83),]

# Sample 23: Variety= velvet, Site= Grand Rapids, Year=1931
# Sample 83: Variety= vevvet, Site= Grand Rapids, Year=1932

### Part d: We suspect these two cases, the 23rd and the 83rd samples, were also switched.
### Can you find evidence from the the graphical display produced in (a)? Switch the
### two cases, repeat the analysis, and comment on your results.

# Look at second plot of part a. 
# Yields from 1931 are higher than that of 1932 for all variety across all sites,
# with the exception of variety No. 475 in University Farm, variety velvet in Grand Rapids. 
# For velvet variety, yields from 1931 are higher than that of 1932 across all sites 
# with the exception of Grand Rapids.Thus, this is evidence that the samples
# from 1931 and 1932 were possibly switched.

newbarley[23,3]=1932; newbarley[83,3]=1931;

g=lm(yield~variety+year+site+variety:year+variety:site+year:site, data=newbarley)
anova(g)
round(summary(g)$r.sq,3)

# Significant factors: p-value < 0.05
# Significant factors: main effects "variety", "year", "site", and
# intercations "variety:site", "year:site"
# The significant factors stay the same
# The p-value of variety:site seems to have a large decrease 
# (from 0.041 to 0.0059, meaning now significant at a 0.01 level)
# R-square increased from 0.944 to 0.956.

###########################################################
###PROBLEM 3

### The peanut data come from a factional factorial experiment to
### investigate factors that affect an industrial process using carbon dioxide to extract oil
### from peanuts. 
### Fit an ANOVA model including all the two-way interactions. 
### Recommend a couple of important factors, on which further experiments will be conducted. 
### You will find that you cannot carry any hypothesis test, since the residual is zero; explain how
### you reach your decision.

### Fit ANOVA
?peanut
g=lm(solubility ~ (press+temp+moist+flow+size)^2., data=peanut)
summary(g)

# Can't make F-tests because there are as many parameters as cases.
# Coding: Low=0, High=1

# If no significant effects and errors are normally distributed, 
# then the estimated effects would then just be linear combinations of the errors and hence normal.
# Look at normal quantile plot of the main effects
# Outliers represent significant effects

halfnorm (coef (g) [-1] ,labs=names(coef(g) [-1] ) )

# From halfnorm, press1:flow1 and press1:temp1 are extremes (ouliers), representing significant effects

### Recommend a couple of important factors, on which further experiments will be conducted. 

# Since interactions of press-flow and press-temp were found to be signficiant
# conduct another experiment focusing on the factors: press, flow and temp to determine their effects and significance

###########################################################
###PROBLEM 4

### Data: The eggprod comes from a randomized block experiment to
### determine factors affecting egg production.

### Data:
### The composite data frame has 12 rows and 3 columns. Six pullets were placed into each of 12 pens. 
### Four blocks were formed from groups of 3 pens based on location. 
### Three treatments were applied. The number of eggs produced was recorded 

data(eggprod)
attach(eggprod)
eggprod
?eggprod

### Part a:  What is the blocking variable?

# A blocking variable is a variable that groups similar observations
# In this case we have 3 treatments, 12 pens available.
# Divide the pens in 4 blocks of 3 pens each wehre the pens
# in each block are grouped by location
# Assign treatment for each block 
# Blocking variable has 4 levels
# Thus, pens grouped into 3 based on location
# The blocking variable is the location of pens
# Each level of the blocking variable represents a location

# Part b: Is there a difference in the treatments?

g=lm(eggs~treat + block)
anova(g)

# Significant effect: p-value <0.05
# Treatment effect significant: there is a difference in treatments
# (null hypothesis that there are no differences among the three treatment means is rejected at 0.005 level)
# Blocking effect not significant 

# Note this is a sequential testing of the models:
# y ˜ 1
# y ˜ treat
# y ˜ treat + block

# Orthogonal so same result if change order
# g=lm(eggs~block+treat)
# anova(g)

### Part c: What efficiency was gained by the blocked design?

# Efficiency : Relative advantage of RCBD over CRD (sigmaCRD/sigmaRCBD)^2


gcrd = lm (eggs ~ treat)
sigmaCRD = summary(gcrd)$sig; sigmaCRD 
sigmaRCBD = summary(g)$sig; sigmaRCBD

efficiency = (sigmaCRD/sigmaRCBD)^2; efficiency

# The relative efficiency of RCBD over CRDis 1.336
# The interpretation is that a CRD would require 33.6% more observations to 
# obtain the same level of precision as a RCBD.


###########################################################
###PROBLEM 5

alfalfa
?alfalfa

### Data: The alfalfa data arise from a Latin square design where the treatment
### factor is inoculum and the blocking factors are shade and irrigation.

### Part a: Test the signicance of the treatment effects.

g=lm(yield ~ ., data=alfalfa)
anova(g)

# Significant effect: p-value <0.05
# Treatment effect (Inoculum) is significant
# Blocking effect shade significant 

### Part b: Determine which levels of the treatment factor are signicantly different.


tky = TukeyHSD(aov(yield ~ ., data=alfalfa), 'inoculum' ) ; tky
names(tky)
comb.names = row.names(tky$inoculum);
sig=tky$inoculum[,4]<0.05
comb.names[sig]

# Signficant different pairs: Look at p-value <0.05 or CI that doesn't include zero
# Treatment paris "E-A" , "E-B" , "E-C" and "E-D" are signficantly different

### Part c: Compare the effciency (i.e., compare sigmahat^2) of the Latin square, the completely ran-
### domized design, and the blocked designs (with shade only, or with irrigation only).


# efficiency

myanova=anova(g)
gr=lm(yield ~ inoculum, data=alfalfa)
(summary(gr)$sig / summary(g)$sig)^2

# Relative efficiency of Latin Squares over CRD is 2.295

gr=lm(yield ~ inoculum + irrigation, data=alfalfa)
(summary(gr)$sig / summary(g)$sig)^2

# Relative efficiency of Latin Squares over block design with irrigation only is 2.531

gr=lm(yield ~ inoculum + shade, data=alfalfa)
(summary(gr)$sig / summary(g)$sig)^2

# Relative efficiency of Latin Squares over block design with  shade only is 1.088






#alternative
#sum(myanova[c(1,2,4),2])/sum(myanova[c(1,2,4),1])/myanova[4,3] # alternative calculation
#sum(myanova[c(1,4),2])/sum(myanova[c(1,4),1])/myanova[4,3] # alternative calculation
#sum(myanova[c(2,4),2])/sum(myanova[c(2,4),1])/myanova[4,3] # alternative calculation