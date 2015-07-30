####################
# Class: STAT 428  #
# Date: 03/14/13   #
# Homework: 3      #
# Luis Steven Lin  #
####################

# Terminology
# loadings or rotations = principal component
# 1st PC = direction of maximum spread
# scores or 'x' (data%*%rotation)= projecttion of data points
# along the PCs (rotated data) = predict(my.pca)
# LDA
# loadings/coeff of the discriminant functions = 'scaling'
# discriminant functions = predict(my.lda)$x

#### Problem 1
# Part a
# Using the Gram-Schmidt algorithm 
# to find the QR decomposition of M.

# Gram-Schmidt function

gram.Schmidt = function (A){
	m = nrow(A)
	n = ncol(A)
	Q = matrix(0,m,n)
	R = matrix(0,n,n)

	# loop through columns
	for (j in 1:n){
		Q[,j] = A[,j]
		
		# loop through rows until j-1 row th
		for (i in (1:j)[-j]){
			# Fill i,j element of R
			R[i,j] = t(Q[,i])%*%A[,j]
			# Update the calculation of j element of Q
			Q[,j] = Q[,j] - R[i,j]*Q[,i]
	
		}
		# Fill in the j,j element of R
		R[j,j] = sqrt( sum(Q[,j]^2) )
		# Fill in the j element of Q
		Q[,j] = Q[,j]/R[j,j]	
	}
return(list("Q"=Q,"R"=R))
}

# Input Data

M = matrix(c(rep(1,5),
		-1.0,-0.5,0.0,0.5,1.0,
             1.0,0.25,0.0,0.25,1.0), nrow=5, ncol=3, byrow=F)

b = c(1.0,0.5,0.0,0.5,2.0)

# Find the QR decomposition

gram.Schmidt(M)

# Part b
# Find x of the linear system Mx = b

# Back substituion function

solve.Backsubs = function (A,b){
	# Ax = b -> QRx = b -> Rx = Q´b 
	
	# Find the QR decomposition
	GS=gram.Schmidt(A)
	Q = GS$Q
	R = GS$R
	m = ncol(A)
	Qb = t(Q)%*%b
	x = rep(0,m)
	
	# Loop starting from last row of R
	# and work from the bottom up
	for(i in m:1){
		x[i] = (Qb[i]- R[i,]%*%x)/R[i,i]
	}
	
return(x)
	
}

# Use back substituion algorithm

solve.Backsubs(M,b)

# Double check using solve function and 
# least square formula:

GS=gram.Schmidt(M)
solve(GS$R, t(GS$Q)%*%b)	

solve(t(M)%*%M)%*%t(M)%*%b

#### Problem 3

getwd()
eNose=read.csv('DB_220_IDLH_2mins.txt', header=F, row.names=1, sep='\t')

# Examine data
dim(eNose)
rownames(eNose)
colnames(eNose)
eNose[1:14,1:5]

## Part a and b
# output the first 3 principal components
# use the 147 by 108 matrix as input matrix
# output the “cumulative” screeplot to show 
# the importance of first 25 PC directions
	
find.pca = function(data){
	# Use built-in R function for PCA
	my.pca = prcomp(data)
	
	# Get the cumulative propotions of PC
	cumProp = summary(my.pca)$imp["Cumulative Proportion",]

	# Output the cumulative scree plot of first, 25 PC's
	plot(1:25,cumProp[1:25],type="o", pch=16,
	xlab="Principal Components",
	ylab="Proportion of variance explained",
	main="Cumulative Screeplot of the first 25 PCs")

	# Output the 
	return(my.pca$rotation[,1:3])

}

find.pca(eNose)

## Part c
# conduct the linear discriminant analysis on the original 
# data and obtain the mis-classification error

library(MASS)

# 147 chemical analytes can be separated into 
# 21 groups with each group has 7 analytes

# get group labels 
y = rownames(eNose)

# replace "-" with "_" to have the same pattern
y = gsub("-","_",y,fixed=TRUE)

# Keep the group name only (string before "_")
y = sapply(strsplit(y,"_",fixed = TRUE),"[[",1)

# String as levels
y = as.factor(y)

# Run Linear Discriminant Analysis
my.lda=lda(eNose,y)
summary(my.lda)
my.lda$prior
my.lda$counts
my.lda$means[c(1:5),1:5]
my.lda$scaling[c(1:5),1:5]
my.lda$lev
my.lda$svd
my.lda$N
my.lda$call

# Use LDA to predict group
my.pred=predict(my.lda)

# Tabulate actual vs prediction
table(y,my.pred$class)

# Compute misclassification error
sum(my.pred$class!=y)/length(y)

## Part d
# Reduce the dimensionality of the original data using the
# first 20 PC directions and conduct the LDA on the reduced data set

# Get rotated data (scores) of the first 20 PC s
my.pca = prcomp(eNose)
scores = my.pca$x[,1:20]

# Note that my.pca$x, predict(my.pca, eNose) and predict(my.pca)
# are all equivalent. 

# Run Linear Discriminant Analysis
my.lda=lda(scores,y)
summary(my.lda)
my.lda$prior
my.lda$counts
my.lda$means[c(1:5),1:5]
my.lda$scaling[c(1:5),1:5]
my.lda$lev
my.lda$svd
my.lda$N
my.lda$call

# Use LDA to predict group
my.pred=predict(my.lda)

# Tabulate actual vs prediction
table(y,my.pred$class)

# Compute misclassification error
sum(my.pred$class!=y)/length(y)

## Part e
# Write an R code to separate the data into training set and testing set. 
# Use the training set to construct the model and testing set to test 
# your model. Output the prediction error of the testing set

# Randomly split data set into training and test sets (80/20)
# optional: can create data frame data.frame(cbind(eNose,y))

# number of testing points
nTest = as.integer(0.2*nrow(eNose))

# indices of testing points 
set.seed(1)
indicesTest = sample(1:nrow(eNose), size= nTest, replace=FALSE)

# training and testing data sets
eNoseTest = eNose[indicesTest,]
yTest = y[indicesTest]
eNoseTrain = eNose[-indicesTest,]
yTrain = y[-indicesTest]

# Construct model with training set
# Run Linear Discriminant Analysis
my.lda=lda(eNoseTrain,yTrain)
summary(my.lda)
my.lda$prior
my.lda$counts
my.lda$means[c(1:5),1:5]
my.lda$scaling[c(1:5),1:5]
my.lda$lev
my.lda$svd
my.lda$N
my.lda$call

# Use LDA to predict group of testing data
my.pred=predict(my.lda,eNoseTest)

# Tabulate actual vs prediction
table(yTest,my.pred$class)

# Compute misclassification error
sum(my.pred$class!=yTest)/length(yTest)

## Part f (BONUS)
# Write an R code to compare if the first two PCA directions
# and first two LDA directions generate the same space span. 
# (Hint: find a reasonable measurement to evaluate the 
# similarity of two directions)

# get principal components and linear discriminant 
# from the original data
my.pca = prcomp(eNose)
PC1 = my.pca$rotation[,'PC1']
PC2 = my.pca$rotation[,'PC2']
my.lda=lda(eNose,y)
LD1 = my.lda$scaling[,'LD1']
LD2 = my.lda$scaling[,'LD2']

# Cosine similarity is a measure of 
# similarity between two vectors
# output =  1: same direction
# output = -1: opposite direction
# output =  0: independence

similarity = function(a,b){
	dot = a%*%b
	mag.a = sqrt(sum(a^2))
	mag.b = sqrt(sum(b^2))
	s = dot/(mag.a*mag.b)
	return(s)
}

similarity(PC1,LD1)
similarity(PC1,LD2)
similarity(PC2,LD1)
similarity(PC2,LD2)
