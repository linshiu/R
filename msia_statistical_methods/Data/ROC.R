
plotROC2 = function(f1,f2,mydata){
  
  fit1 = glm(f1, family=binomial(link="logit"), data=mydata)
  fit2 = glm(f2, family=binomial(link="logit"), data=mydata) 
  response1 = model.frame(f1,mydata)[,1]
  response2 = model.frame(f2,mydata)[,1]
  
  roc1=roc(response1,fit1$fitted,percent=TRUE)
  roc2=roc(response2,fit2$fitted,percent=TRUE)
  
  auc(roc1)
  auc(roc2)
  testobj = roc.test(roc1, roc2)
  testobj 
  
  legend1 = paste("fit1: auc=",format(as.numeric(auc(roc1),digits=2), digits=2, nsmall=2),"%")
  legend2 = paste("fit2: auc=",format(as.numeric(auc(roc2),digits=2), digits=2, nsmall=2),"%")
  
  plot(roc1,col="#1c61b6")
  plot(roc2,col="#008600",add=T) # T = don't erase previous grap
  
  legend("bottomright", legend=c(legend1, legend2), 
         col=c("#1c61b6", "#008600"), lwd=2, inset=0.05,cex=0.75)
  
  text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
  
}


