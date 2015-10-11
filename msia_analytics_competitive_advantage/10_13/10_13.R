x = c(1,2,NA,4,5,NA)
is.na(x)

# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_400_Analytics for Competitive Advantage"
datafolder = "Lab/10_13"
setwd(file.path(main,course, datafolder))


height = read.table("heightmissing.txt", header=T)
summary(height)

mean(height$Male)

mean(height$Male, na.rm = T)

height.omit= na.omit(height)

#### Use Random ####

random.imp <- function (a){
  missing <- is.na(a)           ## T/F matrix
  n.missing <- sum(missing)     ## number of observations with missing values
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

height.rndimp= random.imp(height)

#### Use Mode ####

x = c(1,1,NA,3,4,4,5,5,5,5,6,NA) ## what is the mode?
Mode <-function(x) {
  ) ## list the unique values (no duplicate)
  ux[which.max(tabulate(match(x, ux)))]
}
mode.x= Mode(x);

mcv.imp<-function (a, modevalue){
  missing <-is.na(a)
  n.missing<-sum(missing)
  a.obs<-a[!missing]
  imputed <-a
  imputed[missing] <-modevalue
  return (imputed)
}

x.mcv= mcv.imp(x,mode.x);


#### Average ####

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

#### k-nn ####

#### Outliers ####

male.mean= mean(height.omit$Male)
female.mean= mean(height.omit$Female)
male.std= sd(height.omit$Male)
female.std= sd(height.omit$Female)
male.lb = male.mean-male.std# To see the effect clearly, use +/- 3S
male.ub= male.mean+ male.std# To see the effect clearly, use  +/- 3S
female.lb = female.mean-female.std# To see the effect clearly,  +/- 3S
female.ub= female.mean+ female.std# To see the effect clearly,  +/- 3S

subset(height.omit, Male<male.ub& Male>male.lb & Female<female.ub& Female>female.lb)

install.packages("outliers")
library(outliers)
outlier(height.omit);

height.out= rm.outlier(height.omit);
height.out= rm.outlier(height.omit, median=T); # use median instead of mean

