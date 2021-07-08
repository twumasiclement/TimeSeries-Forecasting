# A function for rolling-origin evaluations for the automatic ARIMA model
ro_eval_ARIMA_forecast<- function(series,forecast_length){
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
  AutoArimaModel<-NULL; AutoArimaModel_forecasts<-NULL
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    train_series<- window(series, end=data_times[i])#training data
    #testing series
    test_series<- window(series, start=(data_times[i]+f))# testing data  
    #rolling forecast horizons h 
    horizon<- length(test_series)
    
    i_index<-seq_along(lead_time_index)[(i+1)-n0] #xextracting lead time index positions
    #fitting the ARIMA model with different forecast origins
    AutoArimaModel[[i_index]]<-auto.arima(train_series, seasonal = FALSE)
    #forecasting 
    AutoArimaModel_forecasts[[i_index]]<- forecast::forecast(AutoArimaModel[[i_index]],PI=TRUE,h=horizon,
                                                             level = c(95),interval = "c")
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<- accuracy(AutoArimaModel_forecasts[[i_index]],test_series)[,c(2,3,5)][2,]    
    
  }
  return(list(ARIMA_errors=lead_times_errors,ARIMA_model= AutoArimaModel, ARIMA_forecasts= AutoArimaModel_forecasts))
}

#exporting external script
source("reverse-ARIMA-forecast-script.r")


# A function for rolling-origin evaluations for the automatic ARIMA model
ro_eval_ARIMA_backcast<- function(series,backcast_length){
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
  AutoArimaModel<-NULL; AutoArimaModel_forecasts<-NULL;Reverse_forecast<-NULL
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    train_series<- window(series, start=data_times[i]+f)#training data
    
    #testing series
    test_series<- window(series, end=(data_times[i]))# testing data 
    #rolling forecast horizons h 
    horizon<- length(test_series)      
    
    i_index<-seq_along(lead_time_index)[(n0+1)-i] #extracting lead time index positions
    #fitting the ARIMA model with different forecast origins
    AutoArimaModel[[i_index]]<-auto.arima(train_series, seasonal = FALSE)
    #forecasting 
    AutoArimaModel_forecasts[[i_index]]<- forecast::forecast(AutoArimaModel[[i_index]],PI=TRUE,h=horizon,
                                                             level = c(95),interval = "c")
    Reverse_forecast[[i_index]]<-reverse_forecast_ARIMA(AutoArimaModel_forecasts[[i_index]])
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<- accuracy(Reverse_forecast[[i_index]],test_series)[,c(2,3,5)][2,]    
    
  }
  return(list(ARIMA_errors=lead_times_errors,ARIMA_model= AutoArimaModel, ARIMA_backcasts=Reverse_forecast))
}

