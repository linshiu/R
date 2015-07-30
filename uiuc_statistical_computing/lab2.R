# Lab 2
# Luis S Lin

library(MASS)
set.seed(1234)

#### Problem 1

# input = number of observations
#         variables

multi.norm1 = function(n,k){
	mean = rep(0,k)
	cov = diag(1,k)
	return(mvrnorm(n,mean,cov))
}

# 100 observations, 5 variables
multi.norm1(100,5)

#### Problem 2

# input = number of observations
#         variables

multi.norm2 = function(n,k){
	mean = rep(1,k)
	cov = diag(1,k) # fill diagonals with 1
	# fill any other entry not in diagonal
	for(i in 1:k){
		for(j in 1:k){
			if(i!=j){
				cov[i,j]=0.3^abs(i-j)
			}
		}
	}	
	return(mvrnorm(n,mean,cov))
}

# 100 observations, 5 variables
multi.norm2(100,5)

#### Problem 3

# 200 observations with values 0 or 1 each with prob=0.5

bin=rbinom(n=200,size=1, prob=0.5)
bin

#### Problem 4

# two classes:
#	 let bin = 0 be class = 0
#	 and bin = 1 be class = 1

# number of observations from each class
nclass1 = sum(bin)
nclass0 = length(bin)-nclass1

# total nclass0 + nclass1 observations
# first nclass0 observations are generated using multi.norm1
# remaining nclass1 observations are generated using multi.norm2
x = rbind(multi.norm1(nclass0,5),multi.norm2(nclass1,5))
x

#### Problem 5

# class label 
# first nclass0 observations are class 0
# remaining nclass1 observations are class 1
y = c(rep(0,nclass0),rep(1,nclass1))

my.lda=lda(x,y)
my.lda

#### Problem 6

# cannot get more than one LD coefficient so use code provided in class
# following code is adapted from the one posted in compass

#get same result by diy without using the lda fcn in R
gmean=by(x,y,mean)

#make list as matrix
gmean=matrix(unlist(gmean),ncol=2)
gmean

#compare to R
names(my.lda)
my.lda$means
gmean==t(my.lda$means)

#compute sigma b
gvar=var(t(gmean))

#compute sigma w, by taking avg of the four estimates, 
#by law of large number to get a smaller variance
varw=by(x,y,var)
varw1=(varw[[1]]+varw[[2]])/2

#M is asymmtric but it should symmetric
#compute in another way

temp=svd(varw1)
names(temp)
vareta=temp$u%*%diag(1/sqrt(temp$d))%*%t(temp$v)
M=vareta%*%gvar%*%vareta
ldasvd=svd(M)

# eigenvector
ldasvd$u  

# coefficient
my.lda$scaling
LDAscale=vareta%*%ldasvd$u

ldaproj=as.matrix(x)%*%LDAscale[,1:2]

# plot data projected on the first two LDA directions
# need to add + 1 because class value 0
plot(ldaproj,col=as.numeric(y+1))

#### Problem 7

# ratio in svd d 
ratio = ldasvd$d
ratio
barplot(ratio,xlab='# LDA directions',ylab='between/within')

