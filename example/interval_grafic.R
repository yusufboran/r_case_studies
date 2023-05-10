library(xts)
library(dygraphs)

trend <- sin(seq(1,41))+runif(41)
data <- data.frame(
  time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), 
  trend=trend, 
  max=trend+abs(rnorm(41)), 
  min=trend-abs(rnorm(41, sd=1))
)

# switch to xts format
data <- xts(x = data[,-1], order.by = data$time)

# Plot
dygraph(data) %>%
  dySeries(c("min", "trend", "max"))
