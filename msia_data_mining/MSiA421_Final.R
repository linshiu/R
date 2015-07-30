#### MSiA 421 : Data Mining Final 
#### Steven Lin

#### Quesion 1 ######################################################################

# My PC
main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

course = "MSIA_421_Data_Mining"
datafolder = "Final"
setwd(file.path(main,course, datafolder))

filename = "gene.csv"
mydata = read.csv(filename,header = T)

#### Part a ####

sick = mydata$sick
mydata = mydata[names(mydata)!="sick"]

# principal component analysis
fit_pca = prcomp(mydata,scale=T)

names(fit_pca)
names(summary(fit_pca))

# Proportion of variance explained by first 2 PCS = 11.5%
summary(fit_pca)$imp[,1:2]

#### Part b ####

col_sick = sick
col_sick[col_sick==0] = "red"
col_sick[col_sick==1] = "blue"

# col=ifelse(cn>=3,"red","black")

plot(fit_pca$x[,"PC1"],fit_pca$x[,"PC2"],
     xlab="PC1",
     ylab="PC2",
     col =col_sick)

#legend(0,10, c("First Half","Second Half"), # puts text in the legend in the appropriate place
       #lty=c(1,1), # gives the legend appropriate symbols (lines)
       #lwd=c(2.5,2.5), # gives the legend lines the correct color and width
       #col=c("darkorange","darkgreen"))

# It reavaels to clusters, one with low values of PC1 corresponding to sick = 0
# and high values of PC1 corresponding to sick = 1


#### Part c ####

dist_matrix = dist(mydata,method = "euclidean")
class(dist_matrix)

fit_hclust1 = hclust(dist_matrix, method = "complete")
plot(fit_hclust1,main="Compete Linkage",labels= sick )

# Yes it separates separates into two groups


#### Part d ####

dist_matrix = as.dist(1-cor(t(mydata)))

fit_hclust2 = hclust(dist_matrix, method = "complete")
plot(fit_hclust2,main="Compete Linkage",labels= sick )

# No it does not separate into two groups

#### Part e ####

set.seed(12345) # not necessary, but assures that you get same starting values
fit_kmeans = kmeans(mydata, 2, nstart=100)
summary(fit_kmeans)
plot(fit_kmeans)
crosstab = table(fit_kmeans$cluster,sick)
rownames(crosstab) = c("cluster1","cluster2")
colnames(crosstab) = c("not sick","sick")
crosstab

# Yes clusters separates sick and not sick people

# use his code

#### Part f ####

## i)
#install.packages("mclust")
library(mclust)
x = fit_pca$x[,1:2]
fit_mclust = Mclust(x,G=2)
plot(fit_mclust)

# EEI : equal shape, equal volume

## ii)

# equal shape, equal volume
# simpler, 0 misclassification

## iii)
fit_mclust$parameter$pro
fit_mclust$parameter$mean
fit_mclust$parameter$variance$sigma

### iv)

# Yes

### v)

# If you would like to separate healthy vs not healthy, you only need PC1
# PC2 is only needed if variation in PC2 needs to be used


#### Quesion 2 ######################################################################

filename = "pcurve.csv"
pcurve = read.csv(filename,header = T)

## Part a

pairs(pcurve)

# V1, V2, V4, V5 look like they have a linear relationship among each other
# V3 seems to have a non-linear relationship with V1,V2,V4,V5
# V6 does not seem to have any relationship with any other variable

## Part b

# install.packages("princurve")
library(princurve)
pcurve_scale = as.matrix(scale(pcurve))
fit_pcurve = principal.curve(pcurve_scale)
summary(fit_pcurve)
fit_pcurve$dist

# SSE = 233.8937

# Part c
names(fit_pcurve)
fit_pcurve$lambda[2]-fit_pcurve$lambda[1]

# distance along the curve (arc length) between observations 1 and 2 = 0.3250539

# Part d

fit_pca2 = prcomp(pcurve,scale=T)
fit_pca2

# Proportion of variance explained by first 2 PCS = 83.75%
summary(fit_pca2)$imp[,1:2] 


plot(fit_pca2$x[,"PC1"],fit_pca2$x[,"PC2"],
     xlab="PC1",
     ylab="PC2")

# Part e
dim(fit_pcurve$s)            # fitted curves for 6 dimesions
dim(fit_pca2$rotation[,1:2]) # vectors of PC1 and PC2 from PCA

# project curves into subpace of the PCA
proj = fit_pcurve$s %*% fit_pca2$rotation[,1:2] # projection onto PC1 and PC2 from PCA

# order by PC1
proj = proj[order(proj[,"PC1"]),]
lines(proj,col="blue",lwd=3)

#### Quesion 3 ######################################################################




names(fit)
methods(class=prcomp)

biplot(fit)
fit$rotation
fit$sdev
fit$x
round(var(fit$x),6)

my.pca$x, predict(my.pca, eNose) and predict(my.pca)


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

names(summary(fit))

