library(EVDS)

set_evds_key("bSjcFT2O0A")

data <- get_series (
  series = c("TP.DK.USD.A"),
  start_date= "04-01-1999",
  end_date= "01-07-2023"
)

data <-  data$items
data$Tarih <-  as.POSIXct(data$Tarih, format="%d-%m-%Y")
data$TP_DK_USD_A <- as.numeric (data$TP_DK_USD_A)
data[data$Tarih < "2005-1-1", ]$TP_DK_USD_A <- data[data$Tarih < "2005-1-1", ]$TP_DK_USD_A /1000000
data <-  data[c(1,2)]

names(data) <- c("time","value")

head(data)
for (i in 1:6) {
  if (is.na(data$value[i])) {
    
    if (!is.na(data$value[i+1])) {
      data$value[i] <- data$value[i+1]
    }
    else{
      data$value[i] <- data$value[i-1]
    }
    
  }
}
for (i in 6:length( data$value)) {
  if (is.na(data$value[i])) {
    ort <- data$value[i-1] + data$value[i-2] + data$value[i-3] + data$value[i-4] + data$value[i-5]
    ort <-  ort / 5
    data$value[i] <- data$value[i-1]
  }
}
data <- head(data,-1)
