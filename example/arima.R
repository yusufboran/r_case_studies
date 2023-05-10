library(forecast)

data("AirPassengers")
class(AirPassengers)

sum(is.na(AirPassengers))
summary(AirPassengers)
plot(AirPassengers)


tsdata <- ts(AirPassengers, frequency = 12) 
ddata <- decompose(tsdata, "multiplicative")
plot(ddata)

plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))


boxplot(AirPassengers~cycle(AirPassengers, xlab="Date", ylab = "Passenger Numbers (1000's)", main = "Monthly air passengers boxplot from 1949-1960"))

mymodel <- auto.arima(AirPassengers)
mymodel

plot.ts(mymodel$residuals)

myforecast <- forecast(mymodel, level=c(95), h=10*12)
plot(myforecast)

Box.test(mymodel$resid, lag=5, type="Ljung-Box")
Box.test(mymodel$resid, lag=10, type="Ljung-Box")
Box.test(mymodel$resid, lag=15, type="Ljung-Box")