# Setup ####

# My PC
main = "/Users/Steven/Documents/Academics/3_Graduate School/2014-2015_NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/10_13"
setwd(file.path(main,course, datafolder))

height <- read.table("heightmissing.txt", header=T)

# Missing Data

x = c(1,2,NA,4,5,NA);
is.na(x);

summary(height);

# mean doesn't work if missing
mean(height$Male)

# disregard missing
mean(height$Male, na.rm = T)

# remove missing obs
height.omit= na.omit(height)

# Imputing Missing Data: Random sampling ####

random.imp<-function (a){
  missing <-is.na(a) ## T/F matrix
  n.missing<-sum(missing) ## number of observations with missing values
  a.obs<-a[!missing]
  imputed <-a
  imputed[missing] <-sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

# this is actually wrong because is taking a random sample
# of both female and male heights to impute both female and male
# should be male sample to impute male, and female to female
height.rndimp= random.imp(height)


# Imputing Missing Data: Most Common ####

# Find the mode


x = c(1,1,NA,3,4,4,5,5,5,5,6,NA) ## what is the mode?
Mode <-function(x) {
  ux<-unique(x) ## list the unique values (no duplicate)
  ux[which.max(tabulate(match(x, ux)))]
}
mode.x= Mode(x);

# also might need to replace last line with the following because of ties
# tab <- tabulate(match(x, ux)); ux[tab == max(tab)]

mcv.imp<-function (a, modevalue){
  missing <-is.na(a)
  n.missing<-sum(missing)
  a.obs<-a[!missing]
  imputed <-a
  imputed[missing] <-modevalue
  return (imputed)
}

x.mcv= mcv.imp(x,mode.x)

# Imputing Missing Data: Average Value ####

avg.imp<-function (a, avg){
  missing <-is.na(a)
  n.missing<-sum(missing)
  a.obs<-a[!missing]
  imputed <-a
  imputed[missing] <-avg
  return (imputed)
}

mavg= mean(na.omit(height$Male));
favg= mean(na.omit(height$Female));
mheight.avgimp= avg.imp(height$Male,mavg);
fheight.avgimp= avg.imp(height$Female,favg);

# kNN ####
impute_kNN = function(x1,x2,n){
  
 
}

# p =

sapply(x2[x1_missing],function(x) sort((abs(na.omit(x2)-x))[1:5]))

# for missing value in column (x1), find corresponding value in other column (x2)
# if corresponding value in (x2) missing, then remove
# 1) find distance from given value (x2) to all values in (x2)
# 2) sort, find smallest n 
# 3) average of in this subset in corresponding (x1) and impute to missing value
# 

x1_missing = is.na(x1)
x2_missing = is.na(x2)

imputed_x1 = x1
imputed_x2 = x2

abs(x2[x1_missing]-x2)


imputed_x1[x1_missing] = 
  
missing = is



x1 = height$Male
x2 = height$Female

# outliers ####

male.mean= mean(height.omit$Male)
female.mean= mean(height.omit$Female)
male.std= sd(height.omit$Male)
female.std= sd(height.omit$Female)

male.lb = male.mean-male.std# To see the effect clearly, use +/-S
male.ub= male.mean+ male.std# To see the effect clearly, use +/-S
female.lb = female.mean-female.std# To see the effect clearly, use +/-S
female.ub= female.mean+ female.std# To see the effect clearly, use +/-S

subset(height.omit, Male<male.ub& Male>male.lb & Female<female.ub& Female>female.lb)

library(outliers)

# Simple outlier detection: observation farthest from the sample average
outlier(height.omit); ## returns the observation

height.out= rm.outlier(height.omit);
height.out= rm.outlier(height.omit, median=T); # use median instead of mean
