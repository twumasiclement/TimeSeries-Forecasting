plot.ELM_forecast<- function(x,main_title,...){
  # Plot function for NNs
  method <- x$method
  if (any(method=="elm.fast")){
    method <- "ELM"
  }
  reps <- dim(x$all.mean)[2]
  ts.plot(x$x,x$mean,
          col=c("black","blue"),lwd=c(1,2),
          main=paste(main_title))
  # If h==1 then use markers
  if (length(x$mean)==1){
    points(rep(time(x$mean),reps+1),c(x$mean),
           pch=c(rep(1,reps),20),col=c("blue"))
  }
  
  
}


# Function to reverse time
reverse_ts <- function(y)
{
  ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}
# Function to reverse a forecast
reverse_forecast_ELM<- function(object)
{
  h <- length(object[["mean"]])
  f <- frequency(object[["mean"]])
  object[["x"]] <- reverse_ts(object[["x"]])
  object[["mean"]] <- ts(rev(object[["mean"]]),
                         end=tsp(object[["x"]])[1L]-1/f, frequency=f)
  object[["lower"]] <- object[["lower"]][h:1L,]
  object[["upper"]] <- object[["upper"]][h:1L,]
  return(object)
}



plot.ELM_reverse <- function(x,main_title,...){
  # Plot function for NNs
  each_mean=reverse_series=list()
  method <- x$method
  if (any(method=="elm.fast")){
    method <- "ELM"
  }
  reps <- dim(x$all.mean)[2]
  # print(x$all.mean)
  
  f=12  #frequency
  
  for(i in 1:dim(x$all.mean)[2])  {
    time.allmean<-time(x$all.mean[,i])
    each_mean[[i]]<- ts(x$all.mean[,i], start=time.allmean[1],
                        end=time.allmean[length(time.allmean)],frequency=12)
    reverse_series[[i]]<-reverse_ts(each_mean[[i]])
    time_end=time(x$x)
    time_end_reverse<-  time_end[1]-(1/f)#end time of the testing data
    reverse_series[[i]]<- ts(reverse_series[[i]],end=time_end_reverse,frequency=f) 
    #print(tsp(reverse_series[[i]])[1L]-1/f)
    # print(reverse_series[[i]])
  }
  
  
  mat <- do.call("ts.union",reverse_series)
  x$all.mean<- mat
  
  
  ts.plot(x$x,x$mean,
          col=c("black","blue"),lwd=c(1,2),
          main=paste(main_title))
  # If h==1 then use markers
  if (length(x$mean)==1){
    points(rep(time(x$mean),reps+1),c(x$mean),
           pch=c(rep(1,reps),20),col=c("blue"))
  }
  
  
}



