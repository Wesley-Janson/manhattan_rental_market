### DEFINE MODELS HERE:

rent_SARIMA_model = function(training_data){
  return(Arima(
    training_data,order=c(0,1,0),seasonal=c(2,0,0),
    include.drift=FALSE,lambda=rent_lambda,method='CSS-ML'))}

inventory_SARIMA_model = function(training_data){
  return(Arima(
    training_data,order=c(0,1,0),seasonal=c(0,1,1),method='CSS-ML'))}

rent_ets_model = function(training_data){
  return(ets(training_data,model="MAN"))}

inventory_ets_model = function(training_data){
  return(ets(training_data,model="MAA"))}

### cross-validation helper functions

add_NAs_to_list <- function(lst) {
  n <- length(lst)
  if (n >= 12) {
    return(lst)
  }
  return(c(lst, rep(NA, 12 - n)))
}

predict_and_forecast = function(model,series,i,last_training_point,n_forecast,window_type){
  if (window_type=="expanding"){
    training_window <- ts(series[1:last_training_point],frequency=12)
  } else if (window_type=="sliding"){
    training_window <- ts(series[i:last_training_point],frequency=12)}
  last_test_point <- ifelse(
    last_training_point+n_forecast<=length(series),
    last_training_point+n_forecast,
    length(series))
  true_future <- series[(last_training_point+1):(last_training_point+n_forecast)]
  model_trained <- model(training_window)
  model_aicc <- model_trained$aicc
  forecast_error <- forecast(model_trained,h=length(true_future))$mean-true_future 
  return(list(model_aicc,add_NAs_to_list(forecast_error)))
}

# cross-validation function
cross.validate = function(model,series,n_window,n_forecast){
  
  # initialize
  iterations <- length(series)-n_window
  model_aicc_expanding <- vector(mode='list', length=iterations)
  error_expanding <- vector(mode='list', length=iterations)
  model_aicc_sliding <- vector(mode='list', length=iterations)
  error_sliding <- vector(mode='list', length=iterations)
  
  # expanding
  for (i in 1:iterations){
    iteration_output <- predict_and_forecast(model,series,i,i+n_window-1,n_forecast,"expanding")
    model_aicc_expanding[i] <- iteration_output[1]
    error_expanding[[i]] <- iteration_output[2]
  }
  
  # sliding
  for (i in 1:iterations){
    iteration_output <- predict_and_forecast(model,series,i,i+n_window-1,n_forecast,"sliding")
    model_aicc_sliding[i] <- iteration_output[1]
    error_sliding[[i]] <- iteration_output[2]
  } 
  
  return(list(model_aicc_expanding,error_expanding,model_aicc_sliding,error_sliding))
}

# plotting
generate.plots = function(output_arima, output_ets){
  arima_expanding_errors <- data.frame(matrix(unlist(output_arima[[2]]), ncol = 12, byrow = TRUE))
  arima_sliding_errors <- data.frame(matrix(unlist(output_arima[[4]]), ncol = 12, byrow = TRUE))
  ets_expanding_errors <- data.frame(matrix(unlist(output_ets[[2]]), ncol = 12, byrow = TRUE))
  ets_sliding_errors <- data.frame(matrix(unlist(output_ets[[4]]), ncol = 12, byrow = TRUE))
  
  # Mean Absolute Forecast Error (MAE) vs forecast horizon
  plot(1:12,colMeans(abs(arima_expanding_errors),na.rm=TRUE),'l',
       main='expanding arima MAE',xlab='horizon',ylab='MAE')
  plot(1:12,colMeans(abs(arima_sliding_errors),na.rm=TRUE),'l',
       main='sliding arima MAE',,xlab='horizon',ylab='MAE')
  
  # Root-square Forecast Error (RMSE) vs forecast horizon
  plot(1:12,sqrt(colMeans((arima_expanding_errors)**2,na.rm=TRUE)),'l',
       main='expanding arima RSME',xlab='horizon',ylab='RMSE')
  plot(1:12,sqrt(colMeans((arima_sliding_errors)**2,na.rm=TRUE)),'l',
       main='sliding arima RSME',,xlab='horizon',ylab='RMSE')
  
  # Mean Absolute Forecast Error (MAE) vs iteration
  plot(1:41,rowMeans(abs(arima_expanding_errors),na.rm=TRUE),'l',
       main='expanding arima MAE',xlab='iteration',ylab='MAE')
  plot(1:41,rowMeans(abs(arima_sliding_errors),na.rm=TRUE),'l',
       main='sliding arima MAE',xlab='iteration',ylab='MAE')
  
  # Root-square Forecast Error (RMSE) vs iteration
  plot(1:41,sqrt(rowMeans((arima_expanding_errors)**2,na.rm=TRUE)),'l',
       main='expanding arima RSME',xlab='iteration',ylab='RMSE')
  plot(1:41,sqrt(rowMeans((arima_sliding_errors)**2,na.rm=TRUE)),'l',
       main='sliding arima RSME',xlab='iteration',ylab='RMSE')
  
  # AICc vs iteration
  plot(1:41,output_arima[[1]],'l',main='expanding arima aicc',xlab='iteration',ylab='AICc')
  plot(1:41,output_arima[[3]],'l',main='sliding arima aicc',,xlab='iteration',ylab='AICc')
  
  # Mean Absolute Forecast Error (MAE) vs forecast horizon
  plot(1:12,colMeans(abs(ets_expanding_errors),na.rm=TRUE),'l',
       main='expanding ets MAE',,xlab='horizon',ylab='MAE')
  plot(1:12,colMeans(abs(ets_sliding_errors),na.rm=TRUE),'l',
       main='sliding ets MAE',xlab='horizon',ylab='MAE')
  
  # Root-square Forecast Error (RMSE) vs forecast horizon
  plot(1:12,sqrt(colMeans((ets_expanding_errors)**2,na.rm=TRUE)),'l',
       main='expanding ets RMSE',xlab='horizon',ylab='RMSE')
  plot(1:12,sqrt(colMeans((ets_sliding_errors)**2,na.rm=TRUE)),'l',
       main='sliding ets RMSE',xlab='horizon',ylab='RMSE')
  
  # Mean Absolute Forecast Error (MAE) vs iteration
  plot(1:41,rowMeans(abs(ets_expanding_errors),na.rm=TRUE),'l',
       main='expanding ets MAE',xlab='iteration',ylab='MAE')
  plot(1:41,rowMeans(abs(ets_sliding_errors),na.rm=TRUE),'l',
       main='sliding ets MAE',xlab='iteration',ylab='MAE')
  
  # Root-square Forecast Error (RMSE) vs iteration
  plot(1:41,sqrt(rowMeans((ets_expanding_errors)**2,na.rm=TRUE)),'l',
       main='expanding ets RMSE',xlab='iteration',ylab='RMSE')
  plot(1:41,sqrt(rowMeans((ets_sliding_errors)**2,na.rm=TRUE)),'l',
       main='sliding ets RMSE',xlab='iteation',ylab='RMSE')
  
  # AICc vs iteration
  plot(1:41,output_ets[[1]],'l',main='expanding ets aicc',xlab='iteration',ylab='AICc')
  plot(1:41,output_ets[[3]],'l',main='sliding ets aicc',xlab='iteration',ylab='AICc')
}