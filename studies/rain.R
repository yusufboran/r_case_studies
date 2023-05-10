library(readr)
library(lubridate)
library(xts)
library(dplyr)
library(zoo)
library(prophet)
library(forecast)
library(keras)
library(TSstudio)
library(seastests)
library(zoo)

#delete out of range
del_out_range <- function(arr,min,max) {
  
  for(i in 2:length(arr)){
    if(arr[i] < min | arr[i] > max){
      arr[i] <- arr[i-1]
    }
  }
  return(arr)
}

normalization <- function(arr) {
  arr <- (arr - min(arr)) / (max(arr) - min(arr))
  return(arr)
}

rain <- read_csv("C:/Users/yusuf/Desktop/burdurPERSIANN_20000301_20190301.csv")
rain <-   head(rain, -1)
time <- as.Date( ymd(substr(rain$Time, 12, 19)))
rain <- rain$`Rain(mm)`
tail(time,12)
#verini içinde olan anomali verileri silinmesi
rain <-  del_out_range(rain,0,200)
summary(rain)

#haftalık ve aylık ortalamalar olarak liste
rain_df <- data.frame(time,rain) 
rain_w_avg <- aggregate(rain ~ format(time, "%Y-%U"), data = rain_df, FUN = mean)
rain_m_avg <- aggregate(rain ~ format(time, "%Y-%m"), data = rain_df, FUN = mean)
names(rain_w_avg) <- c("time", "value")
names(rain_m_avg) <- c("time", "value")

rain_ts<- ts(rain, frequency = 365, start = c(2000, 3, 1))

rain_ts <- ts(rain_df$rain, start = c(2000,3,1),frequency = 365)
rain_ts_w <- ts(rain_w_avg$value, start =  c(2000, 9) , frequency = 52)
rain_ts_m <- ts(rain_m_avg$value, start =  c(2000, 3, 1) , frequency = 12)

head(rain_ts,12)
plot(head(rain_ts))
#plot(rain_ts_w,col="red")
lines(rain_ts_m,col="green")

tail(rain_ts_w,12)
tail(rain_ts_m,12)

ggseasonplot(rain_ts_m)
ggmonthplot(rain_ts_m)
isSeasonal(rain_ts_m)
values <- rain_m_avg$value
# logarithmic regression --------------------------------------------------

days <- 12
#hareketli ortalama
#m_avg_roll <- rollmean(monthly_averages$avg_value, k=3, fill = NA)
#m_avg_roll_c <-  head(tail(m_avg_roll, -1), -1) 
# ilk ve son elemanını silme
m_avg_roll_c <-rain_m_avg$value

num <- length(m_avg_roll_c)-days
ts_12<- ts(head(m_avg_roll_c,num), frequency = 12, start = c(2000, 3))

fit <- tslm(ts_12 ~ poly(trend ,  degree=3) + season  )
ls_for <- forecast(fit, h=days)
data_lm<- ts(m_avg_roll_c, frequency = 12, start = c(2000, 3))

reg_cor <- cor(ls_for$mean, tail(m_avg_roll_c,days))

plot(data_lm)
lines(ls_for$mean,col="red")
legend("topleft", legend=c("original",  paste0("regression korelasyon:", round(reg_cor, digits = 3))),
       col=c("black", "red"), lty = 1:1, cex=0.8)

# arima -------------------------------------------------------------------

ts_data <- ts(head(values,-12))
arima_model <- auto.arima(rain_ts_m,  seasonal = TRUE, ic = c("aicc", "aic", "bic"),)
forecast_auto<- forecast(arima_model, h = 12)
arima_for_ts<- ts(forecast_auto$mean, frequency = 12, start = c(2018, 6))
arima_cor <- cor(forecast_auto$mean,tail(values,12) )

plot(rain_ts_m, main="Arima ile Edilen Değerler", xlab="Zaman", ylab="Değerler", type="l")
lines(arima_for_ts, col="red", type = "l", )
legend("topleft", legend=c("original",  paste0("Arima korelasyon:", round(arima_cor, digits = 3))),
       col=c("black", "red"), lty = 1:1, cex=0.8)


# prophet -----------------------------------------------------------------

date <- as.Date(paste0(rain_m_avg$time, "-01"), format = "%Y-%m-%d")
df <- data.frame(ds = date, y = rain_m_avg$value)
m <- prophet(df, seasonality.mode = 'additive')
future <- make_future_dataframe(m,freq = 'month', periods = 12)
fcst <- predict(m, future)
#plot(m, fcst)
prophet_for_ts<- ts(tail(fcst$yhat,12), frequency = 12, start = c(2018, 6))
prophet_cor <-  cor(prophet_for_ts,tail(values,12) )

plot(rain_ts_m, main="prophet ile tahmin edilen değerler", xlab="Zaman", ylab="Değerler", type="l")
lines( prophet_for_ts, col="red", type = "l", )
legend("topleft", legend=c("original",  paste0("Prophet korelasyon:", round(prophet_cor, digits = 3))),
       col=c("black", "red"), lty = 1:1, cex=0.8)


# lstm --------------------------------------------------------------------
a = values
N = length(a)
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
X = array(x, dim=c(N, step,1))

train_num<- (N - 12)
x_train <-head(X,train_num) 
y_train <-head(y,train_num)
x_test  <- tail(X,12) 
y_test <- tail(y,12) 

model = keras_model_sequential() %>%   
  layer_lstm(units=64, input_shape=c(step, 1)) %>%  
  layer_dense(units=32) %>% 
  layer_dense(units=8) %>%  
  layer_dense(units=1, activation = "linear")
model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)


model %>% summary()
model %>% fit(x_train,y_train, epochs=500, batch_size=64, shuffle = FALSE) 
# model eğitiliyor, epochs tüm verilerin model tarafından kaç kez geçirileceğini, batch_size = her adımda kullanılacak veri örneklerinin sayısını
y_pred = model %>% predict(x_test)
scores = model %>% evaluate(x_train,y_train, verbose = 0)
x_axes = seq(1:length(values))

lstm_for_ts<- ts(y_pred, frequency = 12, start = c(2018, 4))

lstm_cor<-  cor(y_test,y_pred)
plot(rain_ts_m, type="l",  lwd=2)
lines(lstm_for_ts, col="red",lwd=2)
legend("topleft", legend=c("original",  paste0("lstm korelasyon:", round(lstm_cor, digits = 3))),
       col=c("black", "red"), lty = 1:1, cex=0.8)


# finaly grafic -----------------------------------------------------------



plot(rain_ts_m, main="Tahmin Edilen Değerler", xlab="Zaman", ylab="Değerler", type="l")
lines(arima_for_ts, col="red", type = "l" ) 
lines(lstm_for_ts, col="blue",lwd=2)
lines( prophet_for_ts, col="green", type = "l" )
lines(ls_for$mean,col="purple")
legend("topleft", bty="n", 
       legend=c(
         "original", 
         paste0("Arima korelasyon:", round(arima_cor, digits = 2)),
         paste0("LSTM korelasyon:", round(lstm_cor, digits = 2)),
         paste0("Prophet korelasyon:", round(prophet_cor, digits = 2)),
         paste0("Regresyon korelasyon:", round(reg_cor, digits = 2))
         
       ),
       col=c("black", "red","blue","green","purple"), lty = 1:1, cex=0.7)

