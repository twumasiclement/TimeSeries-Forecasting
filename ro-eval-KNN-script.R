# A function for rolling-origin forecast & backcast evaluations for the KNN model
ro_eval_KNN_forecast<- function(series,forecast_length){
  ## A function for rolling-origin forecast evaluations##
  nf<- length(series)# final period of series
  n0<- nf-forecast_length# length of first training series
  data_times<-as.vector(time(series))# seriestimes
  f<- data_times[2]-data_times[1]# frequency of series (or 1/12 for a monthly data)
  yearmonths<-yearmon(data_times)
  lead_time_index<- n0:(nf-2)# lead time or forecast origin indices
  lead_times_errors<- data.frame(matrix(NA,nrow=length(lead_time_index),ncol=3))# lead times by accuracy measure dim
  rownames(lead_times_errors)<-  c(yearmonths[ lead_time_index])
  colnames(lead_times_errors)<- c("RMSE","MAE","MAPE")
  KNN_Model<-NULL; KNN_Model_forecasts<-NULL
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    train_series<- window(series, end=data_times[i])#training data
    #testing series
    test_series<- window(series, start=(data_times[i]+f))# testing data  
    #rolling forecast horizons h 
    horizon<- length(test_series)
    
    i_index<-seq_along(lead_time_index)[(i+1)-n0] #extracting lead time index positions
    
    #function to choose k and the lag value which results in minimum RMSE
    #or better forecasts
    Out_knn<-KNN_k_lag_forecast_best(train_series=train_series,test_series=test_series,
                                     horizon= horizon,lag_values=2,k_values=15)
    
    #Fitting the best KNN model based on good choice of K and lag value
    #with different forecast origins
    KNN_Model[[i_index]]<-knn_forecasting(train_series, h = horizon, lags =Out_knn$lag_selected,
                                          k = Out_knn$k_selected,msas = "MIMO",cf = "mean") 
    #forecasts
    KNN_Model_forecasts[[i_index]]<- KNN_Model[[i_index]]$prediction
    
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<- accuracy(KNN_Model_forecasts[[i_index]],test_series)[,c(2,3,5)]    
    
  }
  return(list(KNN_errors=lead_times_errors,KNN_Model=KNN_Model,KNN_forecasts= KNN_Model_forecasts))
}



##For backcast

reverse_ts <- function(y)
{
  #Tsp Attribute Of Time-Series-Like Objects
  #Reverse the time series data
  ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}

reverse_forecast_KNN <- function(object)
{
  h <- length(object[["prediction"]])
  f <- frequency(object[["prediction"]])
  object[["model"]]$ts <- reverse_ts(object[["model"]]$ts)
  object[["prediction"]] <- ts(rev(object[["prediction"]]),
                               end=tsp(object[["model"]]$ts)[1L]-1/f, frequency=f)
  return(object)
}


# A function for rolling-origin backcast evaluations for the automatic ARIMA model
ro_eval_KNN_backcast<- function(series,backcast_length){
  ## A function for rolling-origin forecast evaluations##
  nf<- length(series)# final period of series
  n0<- backcast_length# initial end lead time index 
  data_times<-as.vector(time(series))# seriestimes
  f<- data_times[2]-data_times[1]# frequency of series (or 1/12 for a monthly data)
  yearmonths<-yearmon(data_times)
  lead_time_index<-  n0:2 # n0:2 lead time or backcast origin indices
  lead_times_errors<- data.frame(matrix(NA,nrow=length(lead_time_index),ncol=3))# lead times by accuracy measure dim
  rownames(lead_times_errors)<-  c(yearmonths[ lead_time_index])
  colnames(lead_times_errors)<- c("RMSE","MAE","MAPE")
  KNN_Model<-NULL; KNN_Model_forecasts<-NULL;Reverse_forecast<-NULL
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    train_series<- window(series, start=data_times[i]+f)#training data
    
    #testing series
    test_series<- window(series, end=(data_times[i]))# testing data 
    #rolling forecast horizons h 
    horizon<- length(test_series)      
    
    i_index<-seq_along(lead_time_index)[(n0+1)-i] #extracting lead time index positions
    
    #function to choose k and the lag value which results in minimum RMSE
    #or better forecasts
    Out_knn<-KNN_k_lag_backcast_best(train_series=train_series,test_series=test_series,
                                     horizon= horizon,lag_values=2,k_values=15)
    
    #Fitting the best KNN model based on good choice of K and lag value
    #with different forecast origins
    KNN_Model[[i_index]]<-knn_forecasting(train_series, h = horizon, lags =Out_knn$lag_selected,
                                          k = Out_knn$k_selected,msas = "MIMO",cf = "mean") 
    #Reverse forecasts
    
    Reverse_forecast[[i_index]]<-reverse_forecast_KNN(KNN_Model[[i_index]])
    
    #Backcasts
    KNN_Model_forecasts[[i_index]]<- Reverse_forecast[[i_index]]$prediction
    
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<- accuracy(KNN_Model_forecasts[[i_index]],test_series)[,c(2,3,5)]    
    
  }
  return(list(KNN_errors=lead_times_errors,KNN_Model=Reverse_forecast,KNN_backcasts= KNN_Model_forecasts))
}


