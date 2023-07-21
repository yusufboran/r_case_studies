
forecasting.packages <- function(dataset,name,count) {
  
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
  
  dataset.df <- data.frame( dataset = name, type=strsplit(name, " ")[[1]][1], time = dataset$time, value=dataset$value )
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
  
  lstm_prediction <- data_frame(
    dataset = name,
    type= "LSTM",
    time =tail(time,days),
    value =f_lstm)
  
  finaly.df <- rbind(finaly.df, lstm_prediction)
  
  postResample_metric <- postResample(y_test,f_lstm)
  correlation <-cor(y_test,f_lstm)
  performance_table <- data_frame(
    dataset = name,
    type= "LSTM",
    Metric = c("Correlation", "R^2","RMSE","MAE","Time"),
    value =c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time ))
  
   
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
  future <- make_future_dataframe(m, periods = days,  freq= (60*60*24*30* 1.013889) *12/count)
  f_prophet <- predict(m, future)
  end_time <- Sys.time()

prophet_prediction <- data_frame(
  dataset = name,
  type= "Prophet",
  time =tail(time,days),
  value =tail(f_prophet$yhat,days))
finaly.df <- rbind(finaly.df, prophet_prediction)

postResample_metric <- postResample(y_test, tail(f_prophet$yhat,days))
correlation <- cor(y_test, tail(f_prophet$yhat,days))
per_prophet<- data_frame(
  dataset = name,
  type= "Prophet",
  Metric = c("Correlation", "R-squared","Root Mean Squared Error","Mean Absolute Error","Time"),
  value = c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time ))
performance_table <- rbind(performance_table, per_prophet)

# arima -------------------------------------------------------------------
start_time <- Sys.time()
ts_data <- ts(y_train)
arima_model <- auto.arima(ts_data)
f_arima <- forecast(arima_model, h = days)
plot(f_arima)
end_time <- Sys.time()
arima_prediction <- data_frame(
  dataset = name,
  type= "ARIMA",
  time =tail(time,days),
  value =f_arima$mean)

finaly.df <- rbind(finaly.df, arima_prediction)

postResample_metric <- postResample(y_test, tail(f_arima$mean,days))
correlation <- cor(y_test, tail(f_arima$mean,days))
per_arima<- data_frame(
  dataset = name,
  type= "ARIMA",
  Metric = c("Correlation", "R^2","RMSE","MAE","Time"),
  value = c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time ))
performance_table <- rbind(performance_table, per_arima)


# logarithmic regression --------------------------------------------------
start_time <- Sys.time()
y <- ts(y_train)
fit <- tslm(y ~ poly(trend,degree = 2) )
f_regression <- forecast(fit, h=days)
plot(f_regression)
end_time <- Sys.time()
lregression_prediction <- data_frame(
  dataset = name,
  type= "LR",
  time =tail(time,days),
  
  value =f_regression$mean
)

finaly.df <- rbind(finaly.df, lregression_prediction)

postResample_metric <- postResample(f_regression$mean,y_test)
correlation <- cor(f_regression$mean,y_test)
per_arima<- data_frame(
  dataset = name,
  type= "LR",
  Metric = c("Correlation", "R-squared","Root Mean Squared Error","Mean Absolute Error","Time"),
  value = c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time ))
performance_table <- rbind(performance_table, per_arima)


return(list(performance_table = performance_table, finaly.df =finaly.df))


}

graphic <- function(finaly.df) {
  
  ggplot(data = finaly.df, aes(x = time, y = value, colour = type)) +
    geom_line(size=2) +colors +
    theme(text = element_text(size = 16))
  
graphic.detail <- ggplot(data = finaly.df[  finaly.df$time >= "2022-1-1" , ], 
         aes(x = time, y = value, colour = type)) +
    geom_line(size=2) +
    colors +
    labs(
      x = "Zaman", 
      y = "USD", 
      colour = "Göstergeler"
    )+
    theme(text = element_text(size = 16),legend.position = "none")
  return(graphic.detail)
}
  
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
