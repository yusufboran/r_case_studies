
forecasting.packages <- function(dataset,datasetname,frequency,count) {
  
  dataset <- data.frame(time =  dataset$time, value =  dataset$value)
  
  days <- count
  time <- dataset$time
  euro <- dataset$value
  
  a = dataset$value
  N = length(a)
  set.seed(123)
  n = seq(1:N)
  
  step = 6
  a = c(a, replicate(step, tail(a, 1)))
  x = NULL
  y = NULL
  for(i in 1:N)
  {
    s = i-1+step
    x = rbind(x,a[i:s])
    y = rbind(y,a[s+1])
  }
  train = array(x, dim=c(N, step,1))
  
  
  time_train <- head(time,-days)
  x_train    <- head(train,-days) 
  y_train    <- head(euro,-days)
  x_test     <- tail(train,days) 
  y_test     <- tail(euro,days) 
  
  dataset.df <- data.frame( dataset = datasetname,frequency =frequency, type=datasetname, time = dataset$time, value=dataset$value )
  finaly.df <- dataset.df
  
  start_time <- Sys.time()
  model = keras_model_sequential() %>%   
    layer_lstm(units=128, input_shape=c(step, 1), activation="relu") %>%  
    layer_dense(units=64, activation = "relu") %>%  
    layer_dense(units=32) %>%  
    layer_dense(units=16) %>%  
    
    layer_dense(units=1, activation = "linear")
  
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = list("mean_absolute_error")
  )
  
  model %>% summary()
  model %>% fit(x_train,y_train, epochs=50 ,batch_size=128, shuffle = FALSE) 
  # model eğitiliyor, epochs tüm verilerin model tarafından kaç kez geçirileceğini, batch_size = her adımda kullanılacak veri örneklerinin sayısını
  f_lstm  <- model %>% predict(x_test)
  end_time <- Sys.time()
  
  lstm_prediction <- data.frame(
    dataset = datasetname,
    frequency=frequency,
    type= "LSTM",
    time =tail(time,days),
    value =f_lstm)
  
  finaly.df <- rbind(finaly.df, lstm_prediction)
  
  postResample_metric <- postResample(y_test,f_lstm)
  correlation <-cor(y_test,f_lstm)
  mape <- mean(abs((y_test - f_lstm) / y_test)) * 100
  
  
  performance_table <- data.frame(
   datasetname = datasetname,
   frequency=frequency,
   model="LSTM",
   cor = correlation,
   num1 = as.numeric(  postResample_metric[2]),
   num2 = as.numeric(postResample_metric[1]),
   num3 = as.numeric (postResample_metric[3]),
   num4 = mape,
   num5 =  as.numeric(end_time - start_time))
  
   
  # prophet ----------------------------------------------------------------- 
  
  if(count == 4 || count == 2 ){
    
    ds <- data.3m[  data.3m$dataset == "Euro" &  data.3m$frequency == "3M" , ]$time
    y_train    <- data.3m[  data.3m$dataset == "Euro" &  data.3m$frequency == "3M" , ]$value
    
    split_ds <- strsplit(ds, "Q")  
    processed_ds <- lapply(split_ds, function(x) {
      year <- as.numeric(x[1])
      quarter <- as.numeric(x[2])
      number <- 3 * quarter - 2
      paste0(year, "-", number,"-1")
    })
    processed_ds <- unlist(processed_ds)
    time_train <-  as.Date(processed_ds)
   
  }
  
  start_time <- Sys.time()
  df <- data.frame(ds =time_train , y = y_train ) 
  m <- prophet(df,seasonality.mode = 'multiplicative',weekly.seasonality=FALSE,daily.seasonality=FALSE)
  future <- make_future_dataframe(m, periods = days,  freq= (60*60*24) *365/count)
  f_prophet <- predict(m, future)
  end_time <- Sys.time()

prophet_prediction <- data.frame(
  dataset = datasetname,
  frequency=frequency,
  type= "Prophet",
  time =tail(time,days),
  value =tail(f_prophet$yhat,days))
finaly.df <- rbind(finaly.df, prophet_prediction)

mape <- mean(abs((y_test - tail(f_prophet$yhat,days)) / y_test)) * 100
postResample_metric <- postResample(y_test, tail(f_prophet$yhat,days))
correlation <- cor(y_test, tail(f_prophet$yhat,days))

performance_table <- rbind(performance_table,
                           list(datasetname,frequency,"Prophet",correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),mape, as.numeric(end_time - start_time) ))



# arima -------------------------------------------------------------------
start_time <- Sys.time()
ts_data <- ts(y_train)
arima_model <- auto.arima(ts_data)
f_arima <- forecast(arima_model, h = days)
plot(f_arima)
end_time <- Sys.time()
arima_prediction <- data.frame(
  dataset = datasetname,
  frequency=frequency,
  type= "ARIMA",
  time =tail(time,days),
  value =f_arima$mean)

finaly.df <- rbind(finaly.df, arima_prediction)

postResample_metric <- postResample(y_test, tail(f_arima$mean,days))

x <-  tail(f_arima$mean,days)
if(sd(x) == 0){
  x[1] <- x[1] * .99
}
correlation <- cor(y_test,x)
mape <- mean(abs((y_test - x) / y_test)) * 100

performance_table <- rbind(performance_table,
                           list(datasetname,frequency,"Arima",correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),mape, as.numeric(end_time - start_time) ))



# logarithmic regression --------------------------------------------------
start_time <- Sys.time()
y <- ts(y_train)
fit <- tslm(y ~ poly(trend,degree = 2) )
f_regression <- forecast(fit, h=days)
plot(f_regression)
end_time <- Sys.time()
lregression_prediction <- data.frame(
  dataset = datasetname,
  frequency=frequency,
  type= "LR",
  time =tail(time,days),
  
  value =f_regression$mean
)

finaly.df <- rbind(finaly.df, lregression_prediction)

postResample_metric <- postResample(f_regression$mean,y_test)
correlation <- cor(f_regression$mean,y_test)
mape <- mean(abs((y_test - x) / f_regression$mean)) * 100

performance_table <- rbind(performance_table,
                           list(datasetname,frequency,"LR",correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),mape, as.numeric(end_time - start_time) ))


return(list(performance_table = performance_table, finaly.df =finaly.df))

}

completion.time <- function(dataset) {
  
  euro.df <- data.frame(
    time = dataset$time,
    value = dataset$value
  )
  time_araligi <- seq(from = min(euro.df$time), to = max(euro.df$time), by = "day")
  euro.df <- data.frame(time = time_araligi) %>%
    left_join(euro.df, by = "time")
  
  return(euro.df)
}

completion.values <- function(arr) {
  
  
  for (i in 1:6) {
    if (is.na(arr[i])) {
      
      if (!is.na(arr[i+1])) {
        arr[i] <- arr[i+1]
      }
      else{
        arr[i] <- arr[i-1]
      }
      
    }
  }
  for (i in 6:length( arr)) {
    if (is.na(arr[i])) {
      ort <- arr[i-1] + arr[i-2] + arr[i-3] + arr[i-4] + arr[i-5]
      ort <-  ort / 5
      arr[i] <- arr[i-1]
    }
  }
  
  return(arr)
}

completion.deficiencies <- function(dataset,name,freq) {
  
  
  for (i in 1:6) {
    if (is.na(dataset$value[i])) {
      
      if (!is.na(dataset$value[i+1])) {
        dataset$value[i] <- dataset$value[i+1]
      }
      else{
        dataset$value[i] <- dataset$value[i-1]
      }
      
    }
  }
  for (i in 6:length( dataset$value)) {
    if (is.na(dataset$value[i])) {
      ort <- dataset$value[i-1] + dataset$value[i-2] + dataset$value[i-3] + dataset$value[i-4] + dataset$value[i-5]
      ort <-  ort / 5
      dataset$value[i] <- dataset$value[i-1]
    }
  }
  names(dataset) <- c("time", "value")
  dataset <- data.frame(
    dataset = name,
    frequency=freq,
    time = dataset$time,
    value = dataset$value
  )
  return(dataset)
}

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
