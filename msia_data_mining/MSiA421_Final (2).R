#### MSiA 421 : Data Mining Final 
#### Steven Lin

#### Quesion 1 ######################################################################


# sscc
setwd("/sscc/home/l/lsl575/")

# My PC
#main = "C:/Users/Steven/Documents/Academics/3_Graduate School/2014-2015 ~ NU/"

# Aginity
#main = "\\\\nas1/labuser169"

#course = "MSIA_421_Data_Mining"
#datafolder = "Final"
#setwd(file.path(main,course, datafolder))

filename = "gene.csv"
mydata = read.csv(filename,header = T)
gene = mydata

#### Part a ####

sick = gene$sick
gene = gene[names(gene)!="sick"]

# principal component analysis
fit_pca = prcomp(gene,scale=F)

names(fit_pca)
names(summary(fit_pca))

# Proportion of variance explained by first 2 PCS = 11.5%
summary(fit_pca)$imp[,1:2]

#### Part b ####

#col_sick = ifelse(sick==0, "red","blue")
#col_sick = sick
#col_sick[col_sick==0] = "red"
#col_sick[col_sick==1] = "blue"

# col=ifelse(cn>=3,"red","black")

#plot(fit_pca$x[,"PC1"],fit_pca$x[,"PC2"],
     #xlab="PC1",
     #ylab="PC2",
     #col =col_sick)

#legend(0,0,c("group A", "group B"), pch = c(1,2))

#legend(0,10, c("First Half","Second Half"), # puts text in the legend in the appropriate place
       #lty=c(1,1), # gives the legend appropriate symbols (lines)
       #lwd=c(2.5,2.5), # gives the legend lines the correct color and width
       #col=c("darkorange","darkgreen"))

PCS = cbind(PC1= fit_pca$x[,"PC1"],PC2=fit_pca$x[,"PC2"],sick)
PCS = data.frame(PCS)

library(ggplot2)


#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#scale_colour_manual(values=cbPalette)p = ggplot(PCS, aes(PC1, PC2))

p = ggplot(PCS, aes(PC1, PC2))
p + geom_point(aes(colour = factor(sick)),size=3) + 
  scale_colour_manual(values=c("blue", "red"), name="Sick")


# It reavaels to clusters, one with low values of PC1 corresponding to sick = 0
# and high values of PC1 corresponding to sick = 1


#### Part c ####

dist_matrix = dist(gene,method = "euclidean")
class(dist_matrix)

fit_hclust1 = hclust(dist_matrix, method = "complete")
plot(fit_hclust1,main="Compete Linkage (Euclidean metric)",labels= sick )

# Yes it separates separates into two groups

#### Part d ####

dist_matrix = as.dist(1-cor(t(gene)))

fit_hclust2 = hclust(dist_matrix, method = "complete")
plot(fit_hclust2,main="Compete Linkage (Correlation metric)",labels= sick )

# No it does not separate into two groups

#### Part e ####

summary.kmeans = function(fit)
{
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  n = sum(fit$size)
  sse = sum(fit$withinss)
  xbar = t(fit$centers)%*%fit$size/n
  ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2)
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
  ))
  cat("SSE = ", sse, "; SSB = ", ssb, "\n")
  cat("R-Squared = ", ssb/(ssb+sse), "\n")
  cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
  invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}
plot.kmeans = function(fit,boxplot=F)
{
  require(lattice)
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){
                  panel.dotplot(...)
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1),
                xlab="Cluster Mean"
  ))
  invisible(plotdat)
}

set.seed(12345) # not necessary, but assures that you get same starting values

fit_kmeans = kmeans(gene, 2, nstart=100)
crosstab = table(fit_kmeans$cluster,sick)
rownames(crosstab) = c("cluster1","cluster2")
colnames(crosstab) = c("not sick","sick")
crosstab

summary(fit_kmeans)
plot(fit_kmeans)

# Yes clusters separates sick and not sick people

# use his code

#### Part f ####

## i)
#install.packages("mclust")
set.seed(12345)
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

summary(fit_mclust)
fit_mclust$classification
plot(fit_mclust)

# Yes

### v)

fit_mclust2 = Mclust(x[,1],G=2)
plot(fit_mclust2)
summary(fit_mclust2)
fit_mclust2$classification

# If you would like to separate healthy vs not healthy, you only need PC1
# PC2 is only needed if variation in PC2 needs to be used


#### Quesion 2 ######################################################################

filename = "pcurve.csv"
pcurve = read.csv(filename,header = T)

## Part a

pairs(pcurve)

# colored pair wise scatter plot
#dta <- pcurve # get data 
#dta.r <- abs(cor(dta)) # get correlations
#dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
#dta.o <- order.single(dta.r) 
#cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
    #   main="Variables Ordered and Colored by Correlation" )


# V1, V2, V4, V5 look like they have a linear relationship among each other
# V3 seems to have a non-linear relationship with V1,V2,V4,V5
# V6 does not seem to have any relationship with any other variable

## Part b

# install.packages("princurve")
library(princurve)
#pcurve_scale = as.matrix(scale(pcurve))
pcurve_scale = as.matrix(pcurve)
fit_pcurve = principal.curve(pcurve_scale)
fit_pcurve$dist

# Part c
names(fit_pcurve)

fit_pcurve$lambda[2]-fit_pcurve$lambda[1]

# distance along the curve (arc length) between observations 1 and 2 = 0.3250539

# Part d
#fit_pca2 = prcomp(pcurve,scale=T)

fit_pca2 = prcomp(pcurve,scale=F,center=F)
fit_pca2

# Proportion of variance explained by first 2 PCS = 83.75%
summary(fit_pca2)$imp[,1:2] 

# order index by score of principal curce
plot(fit_pca2$x[,"PC1"],fit_pca2$x[,"PC2"],
     xlab="PC1",
     ylab="PC2")

# Part e
dim(fit_pcurve$s)            # fitted curves for 6 dimesions
dim(fit_pca2$rotation[,1:2]) # vectors of PC1 and PC2 from PCA

# project curves (points <PC1,PC2> ) into subpace of the PCA (vectors of PC1 and PC2 of PCA)
proj = fit_pcurve$s %*% fit_pca2$rotation[,1:2] # projection onto PC1 and PC2 from PCA

# order by scores (tag has index of small to largest of scores) when plotting line
lines(proj[fit_pcurve$tag,],col="blue",lwd=3)

# from : PC scores of PCA, to: projected points of principal curve into PCA subspace
segments(fit_pca2$x[,"PC1"],
         fit_pca2$x[,"PC2"],
         proj[,"PC1"],
         proj[,"PC2"])

#### scaled ####

library(princurve)
pcurve_scale = as.matrix(scale(pcurve))
#pcurve_scale = as.matrix(pcurve)
fit_pcurve = principal.curve(pcurve_scale)
fit_pcurve$dist

# Part c
names(fit_pcurve)

fit_pcurve$lambda[2]-fit_pcurve$lambda[1]

# distance along the curve (arc length) between observations 1 and 2 = 0.3250539

# Part d
#fit_pca2 = prcomp(pcurve,scale=T)

fit_pca2 = prcomp(pcurve, scale=T, center=T)
fit_pca2

# Proportion of variance explained by first 2 PCS = 83.75%
summary(fit_pca2)$imp[,1:2] 

# order index by score of principal curce
plot(fit_pca2$x[,"PC1"],fit_pca2$x[,"PC2"],
     xlab="PC1",
     ylab="PC2")

# Part e
dim(fit_pcurve$s)            # fitted curves for 6 dimesions
dim(fit_pca2$rotation[,1:2]) # vectors of PC1 and PC2 from PCA

# project curves (points <PC1,PC2> ) into subpace of the PCA (vectors of PC1 and PC2 of PCA)
proj = fit_pcurve$s %*% fit_pca2$rotation[,1:2] # projection onto PC1 and PC2 from PCA

# order by scores (tag has index of small to largest of scores) when plotting line
lines(proj[fit_pcurve$tag,],col="blue",lwd=3)

# from : PC scores of PCA, to: projected points of principal curve into PCA subspace
segments(fit_pca2$x[,"PC1"],
         fit_pca2$x[,"PC2"],
         proj[,"PC1"],
         proj[,"PC2"])


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

