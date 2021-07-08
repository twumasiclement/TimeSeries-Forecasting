
ro_eval_ELM_forecast<- function(series,forecast_length,seed.number=1){
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
  ELM_Model<-NULL; ELM_Model_forecasts<-NULL
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    train_series<- window(series, end=data_times[i])#training data
    #testing series
    test_series<- window(series, start=(data_times[i]+f))# testing data  
    #rolling forecast horizons h 
    horizon<- length(test_series)
    
    i_index<-seq_along(lead_time_index)[(i+1)-n0] #extracting lead time index positions
    
    
    #Fitting the automated ELM model with different forecast origins
    set.seed(seed.number)#setting a seed number of reproducibility
    # sel.lag=TRUE automatically select lags
    #difforder=NULL automatically determines the differencing order
    #hd=Number of hidden nodes (Use NULL to automatically specify).
    
    ELM_Model[[i_index]]<- elm(train_series,sel.lag=TRUE,
                               hd=NULL,retrain =TRUE,difforder=NULL)
    
    #forecasts
    ELM_Model_forecasts[[i_index]]<-forecast::forecast(ELM_Model[[i_index]], PI=TRUE,
                                 h=horizon,level = c(95),interval = "c")
    
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<- accuracy(ELM_Model_forecasts[[i_index]],test_series)[,c(2,3,5)][2,]    
    
  }
  return(list(ELM_errors=lead_times_errors,ELM_Model=ELM_Model,ELM_forecasts= ELM_Model_forecasts))
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


# A function for rolling-origin backcast evaluations for ELM model
ro_eval_ELM_backcast<- function(series,backcast_length,seed.number){
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
  ELM_Model<-NULL; ELM_Model_forecasts<-NULL;Reverse_forecast<-NULL
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    train_series<- window(series, start=data_times[i]+f)#training data
    
    #testing series
    test_series<- window(series, end=(data_times[i]))# testing data 
    #rolling forecast horizons h 
    horizon<- length(test_series)      
    
    i_index<-seq_along(lead_time_index)[(n0+1)-i] #extracting lead time index positions
    
    #Fitting the automated ELM model with different forecast origins
    set.seed(seed.number)#setting a seed number of reproducibility
    # sel.lag=TRUE automatically select lags
    #difforder=NULL automatically determines the differencing order
    #hd=Number of hidden nodes (Use NULL to automatically specify).
    ELM_Model[[i_index]]<- elm(train_series,sel.lag=TRUE,
                               hd=NULL,retrain =TRUE,difforder=NULL) 
    
    #forecasts
    ELM_Model_forecasts[[i_index]]<-  forecast::forecast(ELM_Model[[i_index]], PI=TRUE,
                                                h=horizon,level = c(95),interval = "c")
    
    #Reverse forecasts
    Reverse_forecast[[i_index]]<-reverse_forecast_ELM(ELM_Model_forecasts[[i_index]])  
    
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<- accuracy(Reverse_forecast[[i_index]],test_series)[,c(2,3,5)][2,]     
  }
  return(list(ELM_errors=lead_times_errors,ELM_Model=ELM_Model,ELM_backcasts= Reverse_forecast))
}