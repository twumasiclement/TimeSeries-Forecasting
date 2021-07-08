#Function to choose lag value which results in minimum MAPE
#sigma=A positive real value. The smoothing parameter in GRNN regression. 
#If sigma=NULL (the default) the parameter is chosen using an optimization tool.

#lags=An integer vector in increasing order expressing the lags used as autoregressive variables
#Forecast
GRNN_lag_forecast_best<- function(train_series,test_series,horizon=18,lag_values){
  lag_save <- rep(NA,length=length(lag_values))
  for(i in lag_values){
    #The smoothing parameter of GRNN (sigma) is chosen automatically via optimisation 
    GRNN_pred<- grnn_forecasting(train_series,h =horizon, lags = i,msas = "MIMO",scale = F,sigma=NULL)      
    #accuracy measure (extract MAPE)
    lag_save[i]<- c(as.vector(accuracy(GRNN_pred$prediction,test_series)[,c(2,3,5)][3]))
    
  }
  
  #choose the lag value that gives minimum MSE
  lag_selected<- which.min(lag_save)
  
  return(list(lag_selected=lag_selected,MAPE=lag_save))
}


#Backcast
reverse_ts <- function(y)
{
  #Tsp Attribute Of Time-Series-Like Objects
  #Reverse the time series data
  ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}

reverse_forecast_GRNN <- function(object)
{
  h <- length(object[["prediction"]])
  f <- frequency(object[["prediction"]])
  object[["model"]]$ts <- reverse_ts(object[["model"]]$ts)
  object[["prediction"]] <- ts(rev(object[["prediction"]]),
                               end=tsp(object[["model"]]$ts)[1L]-1/f, frequency=f)
  return(object)
}




#Function to choose lag value which results in minimum MAPE
#sigma=A positive real value. The smoothing parameter in GRNN regression. 
#If sigma=NULL (the default) the parameter is chosen using an optimization tool.

#lags=An integer vector in increasing order expressing the lags used as autoregressive variables
#Forecast
GRNN_lag_backcast_best<- function(train_series,test_series,horizon=18,lag_values){
  lag_save <- rep(NA,length=length(lag_values))
  for(i in lag_values){
    #The smoothing parameter of GRNN (sigma) is chosen automatically via optimisation 
    GRNN_pred<- grnn_forecasting(train_series,h =horizon, lags = i,msas = "MIMO",scale = F,sigma=NULL)
    GRNN_reverse_forecast<-reverse_forecast_GRNN(GRNN_pred) 
    #accuracy measure (extract MAPE)
    lag_save[i]<- c(as.vector(accuracy(GRNN_reverse_forecast$prediction,test_series)[,c(2,3,5)][3]))
    
  }
  
  #choose the lag value that gives minimum MSE
  lag_selected<- which.min(lag_save)
  
  return(list(lag_selected=lag_selected,MAPE=lag_save))
}

