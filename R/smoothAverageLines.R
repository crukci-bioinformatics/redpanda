lines.smooth <- function(x, y,...){
  
  toremove <- which(is.na(x) | is.na(y) | is.infinite(x) | is.infinite(y))
  
  if(length(toremove)>0) x <- x[-toremove];y <- y[-toremove]
  
  lo <- loess(y~x)
  
  xs <- seq(min(x),max(x), (max(x) - min(x))/1000)
  
  lines(xs, predict(lo,xs),col="red",lwd=2)
  
}

smoothAverageLines <- function(data, Group, xCol =1, average="mean",spread="sd",plotAllObs=TRUE,...){
  
  Group <- as.factor(Group)
  
  xs <- data[,xCol]
  
  data <- data[,-xCol]
    
  levs <- levels(Group)
  
  avLine <- varLine <- NULL
  
  plot(xs, type="n", xlim=range(xs), ylim=range(data,na.rm=TRUE),...)
  
  
  for(i in 1:length(levs)){
    
    subMat <- data[,which(Group == levs[i])]
  
    if(average == "mean") aveVals <-  apply(subMat, 1, mean,na.rm=TRUE)
    if(average == "median") aveVals <-  apply(subMat, 1, median,na.rm=TRUE)
    
    lines(xs,aveVals,...)
    points(xs,aveVals,pch=16,...)
    
    if(plotAllObs){
      
      x.rep <- jitter(rep(xs, ncol(subMat)))
      y.all <- unlist(subMat)
      points(x.rep, y.all,col="grey",pch=16)
    }
    
    vars <- apply(subMat, 1, sd,na.rm=TRUE)
    nobs <- apply(subMat, 1, function(x) sum(!is.na(x)))
    
    if(spread =="s.e") vars <- vars/sqrt(nobs)
    if(spread == "95CI") vars <- 1.96 *(vars/sqrt(nobs))
    
    
    arrows(xs, aveVals-vars, xs, aveVals+vars, code=3, angle=90,col=col[i])
    
  }
  
}