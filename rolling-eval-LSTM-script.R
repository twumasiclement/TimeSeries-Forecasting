
#Fitting LSTM model (forecast model)
ro_eval_LSTM_forecast<- function(series,forecast_length,lag=1,units = 1,Epochs=60,
                                 batch_size = 1){
  ## A function for rolling-origin forecast evaluations##
  #differencing the series/transform data to stationary
  diffed = diff(series, differences = lag)
  
  #LSTM expects the data to be in a supervised learning mode. 
  #That is, having a target variable Y and predictor X
  supervised<- lag_transform(diffed, lag)
  
  ## A function for rolling-origin forecast evaluations##
  nf<- length(series)# final period of series
  n0<- nf-forecast_length# length of first training series
  
  data_times<-as.vector(time(series))# seriestimes
  f<- data_times[2]-data_times[1]# frequency of series (or 1/12 for a monthly data)
  yearmonths<-yearmon(data_times)
  lead_time_index<- n0:(nf-2)# lead time or forecast origin indices
  lead_times_errors<- data.frame(matrix(NA,nrow=length(lead_time_index),ncol=3))# lead times by accuracy measure dim
  rownames(lead_times_errors)<-  c(yearmonths[lead_time_index])
  colnames(lead_times_errors)<- c("RMSE","MAE","MAPE")
  LSTM_Model_forecasts<-NULL;LSTM_forecast_list<- NULL
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    ## split into train and test sets
    
    train_series<- window(series, end=data_times[i])#training data
    #testing series
    test_series<- window(series, start=(data_times[i]+f))# testing data 
    #rolling forecast horizons h 
    horizon<- length(test_series)
    #split the first N of the series as training set and the remaining (N-h) as test set
    N = nf-lag
    n = N-horizon
    train = supervised[1:n, ]
    test  = supervised[(n+1):N,  ]
    
    #Normalise data
    Scaled = scale_data(train, test, c(-1, 1))
    y_train = Scaled$scaled_train[, 2]
    x_train = Scaled$scaled_train[, 1]
    y_test = Scaled$scaled_test[, 2]
    x_test = Scaled$scaled_test[, 1]
    
    #LSTM Modeling
    dim(x_train) <- c(length(x_train), 1, 1)
    
    # specify required arguments
    X_shape2 = dim(x_train)[2]
    X_shape3 = dim(x_train)[3]
    batch_size = batch_size# must be a common factor of both the train and test samples
    units = units# can adjust this, in model tuninig phase
    
    i_index<-seq_along(lead_time_index)[(i+1)-n0] #xextracting lead time index positions
    #First stage of modelling
    lstm_model <- keras_model_sequential() 
    lstm_model%>%
      layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
      layer_dense(units = units)
     
   
    #transform the data into an array (x_train)
    x_scaled_train=as.matrix(x_train)
    x_train_data <- t(sapply(1:(length(x_scaled_train)),
      function(x) x_scaled_train[x:(x + lag - 1), 1]))
    # now we transform it into 3D form
    x_train_arr <- array(data = as.numeric(unlist(x_train_data)),
      dim = c(length(x_train_data),lag,1))
    #transform the data into an array (x_test)
    x_scaled_test=as.matrix(x_test)
    x_test_data <- t(sapply(1:(length(x_scaled_test)),
      function(x) x_scaled_test[x:(x + lag - 1), 1]))
    # now we transform it into 3D form
    x_test_arr <- array(data = as.numeric(unlist(x_test_data)),
      dim = c(length(x_test_data),lag,1)) 
    
    #transform the data into an array (y_train)
    y_scaled_train=as.matrix(y_train)
    y_train_data <- t(sapply(1:(length(y_scaled_train)),
      function(x) y_scaled_train[x:(x + lag - 1), 1] ))
    # now we transform it into 3D form
    y_train_arr <- array(data = as.numeric(unlist(y_train_data)),
      dim = c(length(y_train_data),lag,1))
    #transform the data into an array (y_test)
    y_scaled_test=as.matrix(y_test)
    y_test_data <- t(sapply(1:(length(y_scaled_test)),
      function(x) y_scaled_test[x:(x + lag - 1), 1]))
    # now we transform it into 3D form
    y_test_arr <- array(data = as.numeric(unlist(y_test_data)),
      dim = c(length(y_test_data),lag,1 ))
    
    #Compile the model (stage 2)
    lstm_model %>% compile(loss = 'mape',# 'mean_absolute_percentage_error'
      optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),
      #optimizer = 'adam',
      metrics = c('accuracy')
    )
    #Stage 3
    for(j in 1:Epochs ){
      lstm_model %>% fit(x_train_arr, y_train_arr, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
      lstm_model %>% reset_states()
    }
    
    
    #Make predictions from the fitted LSTM
    L_test = length(x_test_arr);scaler = Scaled$scaler;predictions = numeric(L_test)
    #prediction based on x test
    for(j in 1:L_test){
      X = x_test_arr[j];dim(X) = c(1,1,1)
      yhat =  lstm_model%>% predict(X, batch_size=batch_size)
      # invert scaling
      yhat = invert_scaling(yhat, scaler,  c(-1, 1))
      # invert differencing
      yhat  = yhat + series[(n+j)]
      # store
      predictions[j] <- yhat
    }
    
    #predictions on x train
    L_train = length(x_train_arr);scaler = Scaled$scaler;fitted = numeric(L_train)
    for(k in 1:L_train){
      X = x_train_arr[k];dim(X) = c(1,1,1)
      yhat = lstm_model %>% predict(X, batch_size=batch_size)
      # invert scaling
      yhat = invert_scaling(yhat, scaler,  c(-1, 1))
      # invert differencing
      yhat  = yhat + series[k]
      # store
      fitted[k] <- yhat
    }
    
    #Convert predictions into a forecast object
    #lstm_forecast <-  predictions
    LSTM_Model_forecasts[[i_index]]<- predictions
    times_forecast<- time(test_series)
  
    LSTM_Model_forecasts[[i_index]] <- timetk::tk_ts(LSTM_Model_forecasts[[i_index]],
                                   start = times_forecast[1],
                                   end =times_forecast[horizon],
                                   frequency = 12)
   
    #Additionally we need to transform the series 
    #into a time series object
    times_input<- time(train_series)
    input_ts <- timetk::tk_ts(as.vector(train_series), 
                              start=times_input[1],
                              end = times_input[length(times_input)], 
                              frequency = 12)
    
  
    
   #Finally we can define the forecast object:
    LSTM_forecast_list[[i_index]] <- list(
      model = NULL,
      method = "LSTM",
      mean = LSTM_Model_forecasts[[i_index]],
      x = input_ts,
      fitted = fitted,
      residuals = as.numeric(input_ts) - as.numeric(fitted)
    )
    
    class(LSTM_forecast_list[[i_index]]) <- "forecast"
    
    
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<-accuracy(LSTM_forecast_list[[i_index]],
                                      test_series)[,c(2,3,5)][2,]    
    
                     }
  
  return(list(LSTM_errors=lead_times_errors, 
              LSTM_forecasts=LSTM_forecast_list))
  
}



# Function to reverse time
reverse_ts <- function(y)
{
  ts(rev(y), start=tsp(y)[1L], frequency=frequency(y))
}

# Function to reverse a forecast
reverse_forecast_LSTM<- function(object)
{
  h <- length(object[["mean"]])
  f <- frequency(object[["mean"]])
  #print(object[["mean"]])
  
  object[["x"]] <- reverse_ts(object[["x"]])
  object[["mean"]] <- ts(rev(object[["mean"]]),
                         end=tsp(object[["x"]])[1L]-1/f, frequency=f)
  return(object)
}



ro_eval_LSTM_backcast<- function(series,backcast_length,lag=1,units = 1,Epochs=60,
                                 batch_size = 1){
  ## A function for rolling-origin forecast evaluations##
  #differencing the series/transform data to stationary
  diffed = diff(series, differences = lag)
  
  #LSTM expects the data to be in a supervised learning mode. 
  #That is, having a target variable Y and predictor X
  supervised<- lag_transform(diffed, lag)
  
  ## A function for rolling-origin forecast evaluations##
  nf<- length(series)# final period of series
  n0<- backcast_length# length of first training series
  
  data_times<-as.vector(time(series))# seriestimes
  f<- data_times[2]-data_times[1]# frequency of series (or 1/12 for a monthly data)
  yearmonths<-yearmon(data_times)
  lead_time_index<- n0:2 # n0:2 lead time or backcast origin indices
  lead_times_errors<- data.frame(matrix(NA,nrow=length(lead_time_index),ncol=3))# lead times by accuracy measure dim
  rownames(lead_times_errors)<-  c(yearmonths[lead_time_index])
  colnames(lead_times_errors)<- c("RMSE","MAE","MAPE")
  LSTM_Model_forecasts<-NULL; LSTM_forecast_list<-NULL;Reverse_forecast_list<-NULL
 
  for(i in lead_time_index){# different forecast origins
    # training data with different forecast origins
    ## split into train and test sets
    train_series<- window(series, start=data_times[i]+f)#training data
    
    #testing series
    test_series<- window(series, end=(data_times[i]))# testing data  
    #rolling forecast horizons h 
    horizon<- length(test_series)
    #split the first N of the series as training set and the remaining (N-h) as test set
    N = nf-lag
    n = horizon
    
    train = supervised[(n+1):N, ]
    test  = supervised[1:n,  ]
    
    #Normalise data
    Scaled = scale_data(train, test, c(-1, 1))
    y_train = Scaled$scaled_train[, 2]
    x_train = Scaled$scaled_train[, 1]
    y_test = Scaled$scaled_test[, 2]
    x_test = Scaled$scaled_test[, 1]
    
    #LSTM Modeling
    dim(x_train) <- c(length(x_train), 1, 1)
    
    # specify required arguments
    X_shape2 = dim(x_train)[2]
    X_shape3 = dim(x_train)[3]
    batch_size = batch_size# must be a common factor of both the train and test samples
    units = units# can adjust this, in model tuninig phase
    
    i_index<-seq_along(lead_time_index)[(n0+1)-i] #extracting lead time index positions
    #First stage of modelling
    lstm_model <- keras_model_sequential() 
    lstm_model%>%
      layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
      layer_dense(units = units)
    
    
    #transform the data into an array (x_train)
    x_scaled_train=as.matrix(x_train)
    x_train_data <- t(sapply(1:(length(x_scaled_train)),
                             function(x) x_scaled_train[x:(x + lag - 1), 1]))
    # now we transform it into 3D form
    x_train_arr <- array(data = as.numeric(unlist(x_train_data)),
                         dim = c(length(x_train_data),lag,1))
    #transform the data into an array (x_test)
    x_scaled_test=as.matrix(x_test)
    x_test_data <- t(sapply(1:(length(x_scaled_test)),
                            function(x) x_scaled_test[x:(x + lag - 1), 1]))
    # now we transform it into 3D form
    x_test_arr <- array(data = as.numeric(unlist(x_test_data)),
                        dim = c(length(x_test_data),lag,1)) 
    
    #transform the data into an array (y_train)
    y_scaled_train=as.matrix(y_train)
    y_train_data <- t(sapply(1:(length(y_scaled_train)),
                             function(x) y_scaled_train[x:(x + lag - 1), 1] ))
    # now we transform it into 3D form
    y_train_arr <- array(data = as.numeric(unlist(y_train_data)),
                         dim = c(length(y_train_data),lag,1))
    #transform the data into an array (y_test)
    y_scaled_test=as.matrix(y_test)
    y_test_data <- t(sapply(1:(length(y_scaled_test)),
                            function(x) y_scaled_test[x:(x + lag - 1), 1]))
    # now we transform it into 3D form
    y_test_arr <- array(data = as.numeric(unlist(y_test_data)),
                        dim = c(length(y_test_data),lag,1 ))
    
    #Compile the model (stage 2)
    lstm_model %>% compile(loss = 'mape',# 'mean_absolute_percentage_error'
                           optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),
                           #optimizer = 'adam',
                           metrics = c('accuracy')
    )
    #Stage 3
    for(j in 1:Epochs ){
      lstm_model %>% fit(x_train_arr, y_train_arr, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
      lstm_model %>% reset_states()
    }
    
    
    #Make predictions from the fitted LSTM
    L_test = length(x_test_arr);scaler = Scaled$scaler;predictions = numeric(L_test)
    #prediction based on x test
    for(j in 1:L_test){
      X = x_test_arr[j];dim(X) = c(1,1,1)
      yhat =  lstm_model%>% predict(X, batch_size=batch_size)
      # invert scaling
      yhat = invert_scaling(yhat, scaler,  c(-1, 1))
      # invert differencing
      yhat  = yhat + series[j]
      # store
      predictions[j] <- yhat
    }
    
    #predictions on x train
    L_train = length(x_train_arr);scaler = Scaled$scaler;fitted = numeric(L_train)
    for(k in 1:L_train){
      X = x_train_arr[k];dim(X) = c(1,1,1)
      yhat = lstm_model %>% predict(X, batch_size=batch_size)
      # invert scaling
      yhat = invert_scaling(yhat, scaler,  c(-1, 1))
      # invert differencing
      yhat  = yhat + series[n+k]
      # store
      fitted[k] <- yhat
    }
    
   
    
 
    #Convert predictions into a forecast object
    #lstm_forecast <-  predictions
    LSTM_Model_forecasts[[i_index]]<- rev(predictions)
    times_forecast<- time(test_series)
    
    LSTM_Model_forecasts[[i_index]] <- timetk::tk_ts(LSTM_Model_forecasts[[i_index]],
                                                     start = times_forecast[1],
                                                     end =times_forecast[horizon],
                                                     frequency = 12)
    
    
    
    #Additionally we need to transform the series 
    #into a time series object
    times_input<- time(train_series)
    input_ts <- timetk::tk_ts(as.vector(train_series), 
                              start= times_input[1],
                              end = times_input[length(times_input)], 
                              frequency = 12)
    
    #Finally we can define the forecast object:
    LSTM_forecast_list[[i_index]] <- list(
      model = NULL,
      method = "LSTM",
      mean = LSTM_Model_forecasts[[i_index]],
      x = input_ts,
      fitted = fitted,
      residuals = as.numeric(input_ts) - as.numeric(fitted)
    )
    
    class(LSTM_forecast_list[[i_index]]) <- "forecast"
    
    Reverse_forecast_list[[i_index]]<- reverse_forecast_LSTM(LSTM_forecast_list[[i_index]])
    
    #Computing the RMSE, MAE and MAPE
    lead_times_errors[i_index, ]<-accuracy(Reverse_forecast_list[[i_index]],
                                           test_series)[,c(2,3,5)][2,]    
    
  }
  
  return(list(LSTM_errors=lead_times_errors, 
              LSTM_backcasts= Reverse_forecast_list))
  
}

