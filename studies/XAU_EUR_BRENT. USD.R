library(readr)
library(readxl)
library(dplyr)
library(lubridate)

# import dataset  ---------------------------------------------------------
#%>% 

month <- 12*22
#https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=120.EXR.D.USD.EUR.SP00.A
euro.m <- read_csv("D:/yl/dataset/US dollarEuro.csv")[, c(7,8)]
euro.m$OBS_VALUE <- 1/euro.m$OBS_VALUE # USD/EUR to EUR/USD
euro.m <- aggregate(OBS_VALUE ~ format(TIME_PERIOD, "%Y-%m", trim = TRUE), data = euro.m, FUN = mean)
euro.m <- tail(euro.m,month)
names(euro.m) <- c("time", "value")
euro.m$time <- as.Date(paste(euro.m$time, "01", sep = "-"))
#Europe Brent Spot Price FOB (Dollars per Barrel)
brent.m <-  read_csv("D:/yl/dataset/Europe_Brent_Spot_Price_FOB.csv", skip = 4)
brent.m <- head(brent.m,month)
names(brent.m) <- c("time", "value")
brent.m <- brent.m[nrow(brent.m):1, ]
brent.m$time <-as.Date( format(dmy( paste("1", brent.m$time, sep = " ")), "%Y-%m-%d"))
#https://www.gold.org/goldhub/data/gold-prices
gold.m <- read_excel("D:/yl/dataset/goldPrices.xlsx",sheet = "Monthly_Avg", skip = 5)[, c(1,2)]
gold.m <- tail(gold.m,month)
names(gold.m) <- c("time", "value")
gold.m$time <- format(gold.m$time,"%Y-%m-%d")
gold.m$time <- euro.m$time
# export dataset summary --------------------------------------------------
library(openxlsx) 
dataset.sum <- data.frame("NA" = c("Min","1. Qu.", "Median","Mean","3. Qu.", "Max"),
                          Brent=c(summary(brent.m$value)),
                          Gold=c(summary(gold.m$value)),
                          Euro=c(summary(euro.m$value))
)
dataset.sum <-  (format(dataset.sum, digits = 3))

dataset.cor <-  (format(cor(cbind(euro.m$value,brent.m$value,gold.m$value)), digits = 3))

xlsx.sum <- list('dataset_summary' = dataset.sum, 'dataset_correlation' = dataset.cor)
write.xlsx(xlsx.sum, file = 'D:/makale/makale.xlsx')


# dataset correlation -----------------------------------------------------

library(GGally)
dataset.cor <- data.frame( euro = euro.m$value, brent =brent.m$value, gold = gold.m$value) 
ggcorr(dataset.cor, method = c("everything", "pearson"),palette = "RdBu", label = TRUE) 
rm(dataset.sum,dataset.cor)
# dataset grafic summary --------------------------------------------------

library(ggplot2)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

dataset.n <- data.frame(dataset ="Brent",frequency="M", time = brent.m$time, value=normalize( brent.m$value))
dataset.n <- rbind(dataset.n, data.frame(dataset ="Gold",frequency="M", time = gold.m$time, value=normalize( gold.m$value) ))
dataset.n <- rbind(dataset.n,  data.frame(dataset ="Euro", frequency="M",time = euro.m$time, value=normalize( euro.m$value) ))

ggplot(dataset.n, aes(x = time, y = value, color = dataset)) +
  geom_line(size = 3) +
  labs(
    x = "Zaman",
    y = "USD",
    colour = "Tahmin"
  ) +
  facet_wrap(~dataset, dir = "v") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 24))

# prediction ------------------------------------------------------------------
library(prophet)
library(forecast)
library(keras)
library(xts)
library(caret)

# month prediction --------------------------------------------------------


EuroMprediction.data <- forecasting.packages(dataset.n[  dataset.n$dataset == "Euro" &  dataset.n$frequency == "M" , ],"Euro 1M",12)
euro.m.pt <-   (EuroMprediction.data$performance_table) #performans table
euro.m.fr <-  (EuroMprediction.data$finaly.df) #forecasting result

GoldMprediction.data <- forecasting.packages(dataset.n[  dataset.n$dataset == "Gold" &  dataset.n$frequency == "M", ],"Gold 1M",12)
gold.m.pt <-   (GoldMprediction.data$performance_table) #performans table
gold.m.fr <-  (GoldMprediction.data$finaly.df) #forecasting result

BrentMprediction.data <- forecasting.packages(dataset.n[  dataset.n$dataset == "Brent" &  dataset.n$frequency == "M", ],"Brent 1M",12)
brent.m.pt <-   (BrentMprediction.data$performance_table) #performans table
brent.m.fr <-  (BrentMprediction.data$finaly.df) #forecasting result

g<-  rbind(brent.m.fr, gold.m.fr,euro.m.fr)
performance.g <- rbind(euro.m.pt,gold.m.pt,brent.m.pt)


# quarterly prediction ----------------------------------------------------


library(zoo)
library(dplyr)
euro <- dataset.n[  dataset.n$dataset == "Euro" &  dataset.n$frequency == "M" , ]
euro <- euro %>% 
  group_by(time = format(as.yearqtr(time, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean)

gold <- dataset.n[  dataset.n$dataset == "Gold" &  dataset.n$frequency == "M" , ]
gold <- gold %>% 
  group_by(time = format(as.yearqtr(time, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean)

brent <- dataset.n[  dataset.n$dataset == "Brent" &  dataset.n$frequency == "M" , ]
brent <- brent %>% 
  group_by(time = format(as.yearqtr(time, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean)


data.3m <- data.frame(dataset ="Euro",frequency="3M", time = euro$time, value=euro$value)
data.3m <- rbind(data.3m, data.frame(dataset ="Gold",frequency="3M", time = gold$time, value=gold$value ))
data.3m <- rbind(data.3m,  data.frame(dataset ="Brent", frequency="3M",time = brent$time, value= brent$value ))


Euro3Mprediction.data <- forecasting.packages(data.3m[  data.3m$dataset == "Euro" &  data.3m$frequency == "3M" , ],"Euro 3M",4)
euro.m.pt <-   (Euro3Mprediction.data$performance_table) #performans table
euro.m.fr <-  (Euro3Mprediction.data$finaly.df) #forecasting result

Gold3Mprediction.data <- forecasting.packages(data.3m[  data.3m$dataset == "Gold" &  data.3m$frequency == "3M", ],"Gold 3M",4)
gold.m.pt <-   (Gold3Mprediction.data$performance_table) #performans table
gold.m.fr <-  (Gold3Mprediction.data$finaly.df) #forecasting result

Brent3Mprediction.data <- forecasting.packages(data.3m[  data.3m$dataset == "Brent" &  data.3m$frequency == "3M", ],"Brent 3M",4)
brent.m.pt <-   (Brent3Mprediction.data$performance_table) #performans table
brent.m.fr <-  (Brent3Mprediction.data$finaly.df) #forecasting result

data.3m.p<-  rbind(brent.m.fr, gold.m.fr,euro.m.fr)
performance.g <- rbind(performance.g,euro.m.pt,gold.m.pt,brent.m.pt)

split_ds <- strsplit(data.3m.p$time, "Q")  
processed_ds <- lapply(split_ds, function(x) {
  year <- as.numeric(x[1])
  quarter <- as.numeric(x[2])
  number <- 3 * quarter - 2
  paste0(year, "-", number,"-1")
})
processed_ds <- unlist(processed_ds)
data.3m.p$time <-  as.Date(processed_ds)
g<-  rbind(g,data.3m.p)


# 6 month prediction ------------------------------------------------------
ma6 <-  0
for (i in 1:(length(dataset.n$dataset)/6)) {
  ma6[i] <- (mean(dataset.n$value[i:i+6]))
}

data6m<- data.frame(
  dataset =data.3m$dataset[seq_along(data.3m$dataset) %% 2 == 1],
  frequency="6M", 
  time = data.3m$time[seq_along(data.3m$time) %% 2 == 1], 
  value= ma6)

Euro6Mprediction.data <- forecasting.packages(data6m[  data6m$dataset == "Euro" &  data6m$frequency == "6M" , ],"Euro 6M",2)
euro.m.pt <-   (Euro6Mprediction.data$performance_table) #performans table
euro.m.fr <-  (Euro6Mprediction.data$finaly.df) #forecasting result

Gold6Mprediction.data <- forecasting.packages(data6m[  data6m$dataset == "Gold" &  data6m$frequency == "6M", ],"Gold 6M",2)
gold.m.pt <-   (Gold6Mprediction.data$performance_table) #performans table
gold.m.fr <-  (Gold6Mprediction.data$finaly.df) #forecasting result

Brent6Mprediction.data <- forecasting.packages(data6m[  data6m$dataset == "Brent" &  data6m$frequency == "6M", ],"Brent 6M",2)
brent.m.pt <-   (Brent6Mprediction.data$performance_table) #performans table
brent.m.fr <-  (Brent6Mprediction.data$finaly.df) #forecasting result

data.6m.p<-  rbind(brent.m.fr, gold.m.fr,euro.m.fr)
performance.g <- rbind(performance.g,euro.m.pt,gold.m.pt,brent.m.pt)

split_ds <- strsplit(data.6m.p$time, "Q")  
processed_ds <- lapply(split_ds, function(x) {
  year <- as.numeric(x[1])
  quarter <- as.numeric(x[2])
  number <- 3 * quarter - 2
  paste0(year, "-", number,"-1")
})
processed_ds <- unlist(processed_ds)
data.6m.p$time <-  as.Date(processed_ds)

g<-  rbind(g,data.3m.p,data.6m.p)
performance.g$value <- round( as.numeric(performance.g$value), digits = 3)

colors <-  scale_color_manual(
  values = c(
    "Brent" = "#271263",
    "Euro" = "#0bb4ff", 
    "Gold" = "#ffd700",
    "ARIMA" = "#0BFF0A",
    "LSTM" = "#FF370A",
    "LR"="#37745B",
    "Prophet"="#810826"
  )
)


 ggplot(data = g[  g$time >= "2022-1-1" , ],   aes(x = time, y = value, colour = type)) + 
  geom_line(size=2) +colors +
  facet_wrap(~dataset, ncol = 3)
 
#performance.g[performance.g$Metric == "Time",]$value <-performance.g[performance.g$Metric == "Time",]$value /100

ggplot(performance.g, aes(fill=type, y=value, x=Metric  )) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~dataset, ncol = 3)

ggsave(here::here("temp", paste0("investment-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 600,width = 20, height = 20)

