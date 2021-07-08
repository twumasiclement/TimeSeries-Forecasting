#Function to choose k and the lag value which results in minimum MAPE
#Forecast
KNN_k_lag_forecast_best<- function(train_series,test_series,horizon=18,lag_values,k_values){
  lag_save<- NULL; for(i in 1:lag_values)  lag_save[[i]]<- rep(NA,length=k_values)
  for(i in 1:lag_values){
    for(j in 1:k_values){
      KNN_pred<- knn_forecasting(train_series, h =horizon, 
                                 lags =i,k = j,msas = "MIMO",cf = "mean")       
      #accuracy measure (extract MAPE)
      lag_save[[i]][j]<- c(as.vector(accuracy(KNN_pred$prediction,test_series)[,c(2,3,5)][3]))
    }
  }
  MAPE<-do.call("rbind",lag_save)
  #choose the lag and k value that gives minimum MSE
  lag_k<-c(which(MAPE == min(MAPE), arr.ind = TRUE))
  lag_selected<-lag_k[1]
  k_selected<-lag_k[2]
  
  return(list(lag_selected=lag_selected, k_selected= k_selected,MAPE=MAPE))
}



#Function to choose k and the lag value which results in minimum MAPE
#Backcast
KNN_k_lag_backcast_best<- function(train_series,test_series,horizon=18,lag_values,k_values){
  lag_save<- NULL; for(i in 1:lag_values)  lag_save[[i]]<- rep(NA,length=k_values)
  for(i in 1:lag_values){
    for(j in 1:k_values){      
      KNN_pred<- knn_forecasting(train_series, h =horizon, 
                                 lags =i,k = j,msas = "MIMO",cf = "mean") 
      
      #Reverse forecast
      KNN_reverse_forecast<- reverse_forecast_KNN(KNN_pred)
      
      
      #accuracy measure (extract MAPE)
      lag_save[[i]][j]<- c(as.vector(accuracy(KNN_reverse_forecast$prediction,test_series)[,c(2,3,5)][3]))
    }
  }
  MAPE<-do.call("rbind",lag_save)
  #choose the lag and k value that gives minimum MSE
  lag_k<-c(which(MAPE == min(MAPE), arr.ind = TRUE))
  lag_selected<-lag_k[1]
  k_selected<-lag_k[2]
  
  return(list(lag_selected=lag_selected, k_selected= k_selected,MAPE=MAPE))
}