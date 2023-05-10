library(prophet)
library(readxl)
library (EVDS)
library(forecast)
library(keras)
library(xts)
library(dplyr)
library(ggplot2)

#preparing training data
pre_tran <- function(arr,step) {
  
  a = arr
  N = length(a)
  n = seq(1:N)
  
  a = c(a, replicate(step, tail(a, 1)))
  x = NULL
  y = NULL
  for(i in 1:N)
  {
    s = i-1+step
    x = rbind(x,a[i:s])
    y = rbind(y,a[s+1])
  }
  X = array(x, dim=c(N, step,1))
  return(X)
}

set_evds_key("bSjcFT2O0A")

data <- get_series (
  series = c("TP.DK.EUR.A"),
  start_date= "01-01-2020",
  end_date= format(Sys.time(), "%d-%m-%Y")
)


data <-  data$items
data$UNIXTIME <-  NULL
data$Tarih <-  as.POSIXct(data$Tarih, format="%d-%m-%Y")
data <- data[!is.na(data$TP_DK_EUR_A), ]
data$TP_DK_EUR_A <- as.numeric (data$TP_DK_EUR_A)

data_ts <- ts(data$TP_DK_EUR_A , start = c(2020, 1, 1), frequency = 365)
plot(data_ts)


days <- 30
time <- data$Tarih
euro <- data$TP_DK_EUR_A

a = data$TP_DK_EUR_A
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


train      <- pre_tran(euro,6)
time_train <- head(time,-days)
x_train    <- head(train,-days) 
y_train    <- head(euro,-days)
x_test     <- tail(train,days) 
y_test     <- tail(euro,days) 


# prophet -----------------------------------------------------------------
df <- data.frame(ds =time_train , y = y_train ) 
m <- prophet(df)
future <- make_future_dataframe(m, periods = days)
f_prophet <- predict(m, future)
c_prophet <-  cor(y_test, tail(f_prophet$yhat,days))
plot(m, f_prophet)
# arima -------------------------------------------------------------------

ts_data <- ts(y_train)
arima_model <- auto.arima(ts_data)
f_arima <- forecast(arima_model, h = days)
c_arima <-  cor(y_test, tail(f_arima$mean,days))
plot(f_arima)
# logarithmic regression --------------------------------------------------
y <- ts(y_train)
fit <- tslm(y ~ poly(trend,degree = 7) )
f_regression <- forecast(fit, h=days)
c_regression <- cor(f_regression$mean,y_test)
plot(f_regression)

# lstm --------------------------------------------------------------------

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
model %>% fit(x_train,y_train, epochs=50, batch_size=128, shuffle = FALSE) 
# model eğitiliyor, epochs tüm verilerin model tarafından kaç kez geçirileceğini, batch_size = her adımda kullanılacak veri örneklerinin sayısını

f_lstm  <- model %>% predict(x_test)
c_lstm  <- cor(y_test,f_lstm)
x_axes = seq(1:length(euro))
plot(x_axes, euro, type="l", col="red", lwd=2)
lines(tail(x_axes,days), f_lstm, col="blue",lwd=2)
legend("topleft", legend=c("original", "predicted"),
       col=c("red", "blue"), lty=1,cex=0.8)


# grnn --------------------------------------------------------------------

f_grnn <-  grnn_forecasting(y_train, h = 30,transform = "additive", sigma = 50)
c_grnn <- cor(f_grnn$prediction,y_test)
plot(f_grnn,type="l")

# final chart -------------------------------------------------------------

prophet_pre <- data.frame(
  time = tail(time,days), 
  orijinal =y_test,
  prophet = tail(f_prophet$yhat,days),  
  arima = f_arima$mean,
  regression = f_regression$mean,
  lstm = f_lstm,
  grnn = f_grnn$prediction,
  )


p1 <- ggplot(prophet_pre, aes(x = time)) +
  geom_line(aes(y = prophet, color = "Orijinal"),size=1) +
  geom_line(aes(y = orijinal, color = "Prophet"),size=1) +
  geom_line(aes(y = arima, color = "arima"),size=1) +
  geom_line(aes(y = regression, color = "regression"),size=1) +
  geom_line(aes(y = lstm, color = "lstm"),size=1) +
  geom_line(aes(y = f_grnn$prediction, color = "grnn"),size=1) +
  
  scale_color_manual(
    values = c(
      "Prophet" = "#9dd866", 
      "Orijinal" = "#262626",
      "arima" = "#f6c85f",
      "regression" = "#6f4e7c",
      "lstm" = "#ca472f",
      "grnn" = "#0b84a5"
      
      # #ffa056 #8dddd0
      # https://www.heavy.ai/blog/12-color-palettes-for-telling-better-stories-with-your-data
      )) +
  labs(title = "Original and predicted values", x = "Zaman", y = "TL / Euro",  colour = "Algoritma",)
p1 
