#Function to choose k and the lag value which results in minimum MAPE
#Forecast
#Function to choose non-seasonal lag (p) and the average number of networks
#which results in a minimum MAPE

#Forecast
NNAR_tuning_forecast<- function(train_series,test_series,
          horizon=18,p_values,num_net_values,seed.number){
  #p= Number of non-seasonal lags used as inputs
  #P= Number of seasonal lags used as inputs (set P=0 for non-seasonal series)
  #repeats= average number of networks #lambda= boxcox transformation
  #If lambda="auto", then a transformation is automatically selected using BoxCox.lambda.
  p_save<- NULL; for(i in p_values)  p_save[[i]]<- rep(NA,length=length(num_net_values))
  for(i in p_values){
    for(j in seq_along(num_net_values)){
      set.seed(seed.number)#setting a seed number of reproducibility
      NNAR_model<- nnetar(train_series, p=i, P = 0,lambda="auto",repeats=num_net_values[j])
      NNAR_forecast<- forecast::forecast(NNAR_model, PI=TRUE,h=horizon,level = c(95),interval = "c")
      #accuracy measure (extract MAPE)
      p_save[[i]][j]<- c(as.vector(accuracy(NNAR_forecast,test_series)[,c(2,3,5)][2,3]))
    }
  }
  MAPE<-do.call("rbind",p_save)
  colnames(MAPE)<-paste("nnet=",num_net_values)
  rownames(MAPE)=paste("p=",p_values)
  #choose the lag and k value that gives minimum MSE
  p_nnet<-c(which(MAPE == min(MAPE), arr.ind = TRUE))
  p_selected<-p_values[p_nnet[1]]
  nnet_selected<- num_net_values[p_nnet[2]]
  return(list(p_selected=p_selected, nnet_selected= nnet_selected,MAPE=MAPE))
}




#backcast
# Function to reverse time
reverse_ts <- function(y)
{
  ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}
# Function to reverse a forecast
reverse_forecast_NNAR<- function(object)
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


NNAR_tuning_backcast<- function(train_series,test_series,
          horizon=18,p_values,num_net_values,seed.number){
  #p= Number of non-seasonal lags used as inputs
  #P= Number of seasonal lags used as inputs (set P=0 for non-seasonal series)
  #repeats= average number of networks #lambda= boxcox transformation
  #If lambda="auto", then a transformation is automatically selected using BoxCox.lambda.
  p_save<- NULL; for(i in p_values)  p_save[[i]]<- rep(NA,length=length(num_net_values))
  for(i in p_values){
    for(j in seq_along(num_net_values)){
      set.seed(seed.number)#setting a seed number of reproducibility
      NNAR_model<- nnetar(train_series, p=i, P = 0,lambda="auto",repeats=num_net_values[j])
      NNAR_forecast<- forecast::forecast(NNAR_model, PI=TRUE,h=horizon,level = c(95),interval = "c")
      Reverese_forecast<-reverse_forecast_NNAR(NNAR_forecast)#reverse forecasts
      #accuracy measure (extract MAPE)
      p_save[[i]][j]<- c(as.vector(accuracy(Reverese_forecast,test_series)[,c(2,3,5)][2,3]))
    }
  }
  MAPE<-do.call("rbind",p_save)
  colnames(MAPE)<-paste("nnet=",num_net_values)
  rownames(MAPE)=paste("p=",p_values)
  #choose the lag and k value that gives minimum MSE
  p_nnet<-c(which(MAPE == min(MAPE), arr.ind = TRUE))
  p_selected<-p_values[p_nnet[1]]
  nnet_selected<- num_net_values[p_nnet[2]]
  return(list(p_selected=p_selected, nnet_selected= nnet_selected,MAPE=MAPE))
}


