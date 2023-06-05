library(prophet)
library(readxl)
library(EVDS)
library(forecast)
library(keras)
library(xts)
library(dplyr)
library(ggplot2)
library(caret)
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
# [#e60049", "#0bb4ff", "#50e991", "#e6d800", "#9b19f5", "#ffa300", "#dc0ab4", "#b3d4ff", "#00bfa0"]

colors <-  scale_color_manual(
  values = c(
    "Euro/TL" = "#271263",
    "Prophet" = "#0bb4ff", 
    "ARIMA" = "#50e991",
    "LR" = "#e6d800",
    "LSTM" = "#e60049"
  )
) 
grafic_text <-  labs(
  x = "Zaman", 
  y = "Euro/TL", 
  colour = "Tahmin",
  title = "01.01.2020 - 01.06.2023 arası Euro/TL"
)

set_evds_key("bSjcFT2O0A")

data <- get_series (
  series = c("TP.DK.EUR.A"),
  start_date= "01-01-2020",
  end_date= format(Sys.time(), "%d-%m-%Y")
)

summary(data$TP_DK_EUR_A)
tail(data)
data <-  data$items
data$Tarih <-  as.POSIXct(data$Tarih, format="%d-%m-%Y")
data$TP_DK_EUR_A <- as.numeric (data$TP_DK_EUR_A)

raw.df <- data.frame( time = data$Tarih, value=data$TP_DK_EUR_A )

ggplot(data = raw.df, aes(x = time, y = value)) +
  geom_line() +
  xlab("Zaman") +
  ylab("Euro/Tl") 


for (i in 1:6) {
  if (is.na(data$TP_DK_EUR_A[i])) {
    
    if (!is.na(data$TP_DK_EUR_A[i+1])) {
      data$TP_DK_EUR_A[i] <- data$TP_DK_EUR_A[i+1]
    }
    else{
      data$TP_DK_EUR_A[i] <- data$TP_DK_EUR_A[i-1]
    }
    
  }
}
for (i in 6:length( data$TP_DK_EUR_A)) {
  if (is.na(data$TP_DK_EUR_A[i])) {
    ort <- data$TP_DK_EUR_A[i-1] + data$TP_DK_EUR_A[i-2] + data$TP_DK_EUR_A[i-3] + data$TP_DK_EUR_A[i-4] + data$TP_DK_EUR_A[i-5]
    ort <-  ort / 5
    data$TP_DK_EUR_A[i] <- data$TP_DK_EUR_A[i-1]
  }
}

data.df <- data.frame( time = data$Tarih, value=data$TP_DK_EUR_A )

days <- 180
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


euro.df <- data.frame( type="Euro/TL", time = data$Tarih, value=data$TP_DK_EUR_A )
finaly.df <- euro.df

performance_table <- data.frame(
  Metric = c("Correlation", "R-squared","Root Mean Squared Error","Mean Absolute Error","Time"
  )
)

# lstm --------------------------------------------------------------------
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
model %>% fit(x_train,y_train, epochs=200, batch_size=128, shuffle = FALSE) 
# model eğitiliyor, epochs tüm verilerin model tarafından kaç kez geçirileceğini, batch_size = her adımda kullanılacak veri örneklerinin sayısını
f_lstm  <- model %>% predict(x_test)
end_time <- Sys.time()

lstm_prediction <- data_frame(
  type= "LSTM",
  time =tail(time,days),
  value =f_lstm)

finaly.df <- rbind(finaly.df, lstm_prediction)

ggplot(data = finaly.df[finaly.df$type %in% c("LSTM", "Euro/TL"), ], 
       aes(x = time, y = value, colour = type)) +
  geom_line(size=2) +colors +grafic_text+
  theme(text = element_text(size = 16))

postResample_metric <- postResample(y_test,f_lstm)
correlation <-cor(y_test,f_lstm)
performance_table$lstm <- c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time )

# prophet -----------------------------------------------------------------
start_time <- Sys.time()
df <- data.frame(ds =time_train , y = y_train ) 
m <- prophet(df,daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods = days)
f_prophet <- predict(m, future)
plot(m, f_prophet)
end_time <- Sys.time()

prophet_prediction <- data_frame(
  type= "Prophet",
  time =tail(f_prophet$ds,days),
  value =tail(f_prophet$yhat,days))
finaly.df <- rbind(finaly.df, prophet_prediction)

ggplot(data = finaly.df[finaly.df$type %in% c("Prophet", "Euro/TL"), ], 
       aes(x = time, y = value, colour = type)) +
  geom_line(size=2) +colors +grafic_text+
  theme(text = element_text(size = 16))


postResample_metric <- postResample(y_test, tail(f_prophet$yhat,days))
correlation <- cor(y_test, tail(f_prophet$yhat,days))
performance_table$prophet <- c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time )

# arima -------------------------------------------------------------------
start_time <- Sys.time()
ts_data <- ts(y_train)
arima_model <- auto.arima(ts_data)
f_arima <- forecast(arima_model, h = days)
plot(f_arima)
end_time <- Sys.time()
arima_prediction <- data_frame(
  type= "ARIMA",
  time =tail(time,days),
  value =f_arima$mean)

finaly.df <- rbind(finaly.df, arima_prediction)
ggplot(data = finaly.df[finaly.df$type %in% c("ARIMA", "Euro/TL"), ], 
       aes(x = time, y = value, colour = type)) +
  geom_line(size=2) +colors +grafic_text+
  theme(text = element_text(size = 16))


postResample_metric <- postResample(y_test, tail(f_arima$mean,days))
correlation <- cor(y_test, tail(f_arima$mean,days))
performance_table$arima <-  c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time )
# logarithmic regression --------------------------------------------------
start_time <- Sys.time()
y <- ts(y_train)
fit <- tslm(y ~ poly(trend,degree = 2) )
f_regression <- forecast(fit, h=days)
plot(f_regression)
end_time <- Sys.time()
lregression_prediction <- data_frame(
  type= "LR",
  time =tail(time,days),
  value =f_regression$mean
)

finaly.df <- rbind(finaly.df, lregression_prediction)
ggplot(data = finaly.df[finaly.df$type %in% c("LR", "Euro/TL"), ], 
       aes(x = time, y = value, colour = type)) +
  geom_line(size=2) +colors +grafic_text+
  theme(text = element_text(size = 16))

postResample_metric <- postResample(f_regression$mean,y_test)
correlation <- cor(f_regression$mean,y_test)
performance_table$l_regresyon <-c(correlation, as.numeric(  postResample_metric[2]), as.numeric(postResample_metric[1]),   as.numeric (postResample_metric[3]),end_time - start_time )
end_time - start_time


# finaly ------------------------------------------------------------------

ggplot(data = finaly.df, aes(x = time, y = value, colour = type)) +
  geom_line(size=2) +colors +grafic_text+
  theme(text = element_text(size = 16))


ggplot(data = finaly.df[  finaly.df$time >= "2023-01-01" & finaly.df$time <= "2023-06-01", ], 
       aes(x = time, y = value, colour = type)) +
  geom_line(size=2) +
  colors +
  labs(
    x = "Zaman", 
    y = "Euro/TL", 
    colour = "Tahmin",
    title = "01.01.2023 - 01.06.2023 arası Euro/TL"
  ) +
  theme(text = element_text(size = 16))

ggplot(data = finaly.df[ !finaly.df$type %in% c("LR") & finaly.df$time >= "2023-01-01" & finaly.df$time <= "2023-06-01", ], 
       aes(x = time, y = value, colour = type)) +
  geom_line(size=2) +
  colors +
  grafic_text +
  theme(text = element_text(size = 16))



t(format(performance_table, digits = 3))
