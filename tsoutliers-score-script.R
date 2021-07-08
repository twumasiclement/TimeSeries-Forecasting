#Method 2 of Stage 2
tsoutliers_score <- function(x,plot=FALSE,data_name,xaxt_cat)
{
  x <- as.ts(x)
  if(frequency(x)>1)
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  
  limits <- resid.q + 1.5*iqr*c(-1,1)
  
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0)) #pmin is parallel minimum
  if(plot)
  {
    plot(x,ylab=paste(data_name), xaxt=xaxt_cat,lwd=1,col="grey80")
    x2 <- ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    tsp(x2) <- tsp(x)
    points(x2,pch=19,col="red")
    return(invisible(score))
  }
  else
    return(score)
}