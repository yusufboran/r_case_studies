library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

start.date <- "2002-1-1"

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
# euro --------------------------------------------------------------------

#https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=120.EXR.D.USD.EUR.SP00.A
euro.d <- read_csv("D:/yl/dataset/USD-EUR.csv", 
                   col_names = FALSE, skip = 21)[, c(1,2)]
euro.d$X1 <- as.Date(euro.d$X1)
names(euro.d) <- c("time","value")
euro.d <-  euro.d[euro.d$time > start.date & euro.d$time < "2023-7-1",  ]
euro.d$value <-  as.numeric(euro.d$value) 
euro.d <-  completion.time(euro.d)
euro.c <- euro.d
euro.d <- completion.deficiencies(euro.d,"Euro","D")
euro.d$value <- normalize(euro.d$value)


euro.w <- aggregate(value ~ format(time, "%Y-%U"), data = euro.d, FUN = mean)
names(euro.w) <- c("time", "value")
euro.w$time <- as.Date(paste0(euro.w$time, "-7"), format = "%Y-%U-%u")
euro.w$time <- format(euro.w$time, "%Y-%m-%d")
euro.w <- data.frame(
  dataset = "Euro",
  frequency="W",
  time = euro.w$time,
  value = euro.w$value
)

euro.m <- aggregate(value ~ format(time, "%Y-%m", trim = TRUE), data = euro.d, FUN = mean)
names(euro.m) <- c("time", "value")
euro.m$time <- as.Date(paste(euro.m$time, "01", sep = "-"))
euro.m <- data.frame(
  dataset = "Euro",
  frequency="M",
  time = euro.m$time,
  value =  euro.m$value
)
euro <- rbind(euro.d,euro.w,euro.m)

# get brent oil dataset ------------------------------------------------

brent.d <- read_excel("D:/yl/dataset/makale/RBRTEd.xls",sheet = "Data 1", skip = 2)
names(brent.d) <- c("time", "value")
brent.d <-  brent.d[brent.d$time > start.date & brent.d$time < "2023-7-1",  ]
brent.d$time <- as.Date(format(brent.d$time,"%Y-%m-%d"))

brent.d <-  completion.time(brent.d)
brent.c <- brent.d
brent.d <- completion.deficiencies(brent.d,"Brent","D")
brent.d$value <- normalize(brent.d$value)


brent.w <- aggregate(value ~ format(time, "%Y-%U"), data = brent.d, FUN = mean)
names(brent.w) <- c("time", "value")
brent.w$time <- as.Date(paste0(brent.w$time, "-7"), format = "%Y-%U-%u")

brent.w$time <- format(brent.w$time, "%Y-%m-%d")


brent.w <- data.frame(
  dataset = "Brent",
  frequency="W",
  time = brent.w$time,
  value = brent.w$value
)
brent.m <- aggregate(value ~ format(time, "%Y-%m", trim = TRUE), data = brent.d, FUN = mean)
names(brent.m) <- c("time", "value")
brent.m$time <- as.Date(paste(brent.m$time, "01", sep = "-"))
brent.m <- data.frame(
  dataset = "Brent",
  frequency="M",
  time = brent.m$time,
  value =  brent.m$value
)

brent <- rbind(brent.d,brent.w,brent.m)

#https://www.gold.org/goldhub/data/gold-prices

gold.d <- read_excel("D:/yl/dataset/goldPrices.xlsx", sheet = "Daily", skip = 5)[, c(1,2)]
names(gold.d) <- c("time", "value")
gold.d <-  gold.d[gold.d$time > start.date & gold.d$time < "2023-7-1",  ]
gold.d <- tail(gold.d,-1)
gold.d$time <- as.Date(format(gold.d$time,"%Y-%m-%d"))
gold.d <-  completion.time(gold.d)
gold.c <- gold.d
gold.d <- completion.deficiencies(gold.d,"Gold","D")
gold.d$value <- normalize(gold.d$value)


gold.w <- aggregate(value ~ format(time, "%Y-%U"), data = gold.d, FUN = mean)
names(gold.w) <- c("time", "value")
gold.w$time <- as.Date(paste0(gold.w$time, "-7"), format = "%Y-%U-%u")
gold.w$time <- format(gold.w$time, "%Y-%m-%d")
gold.w <- data.frame(
  dataset = "Gold",
  frequency="W",
  time = gold.w$time,
  value = gold.w$value
)

gold.m <- aggregate(value ~ format(time, "%Y-%m", trim = TRUE), data = gold.d, FUN = mean)
names(gold.m) <- c("time", "value")
gold.m$time <- as.Date(paste(gold.m$time, "01", sep = "-"))
gold.m <- data.frame(
  dataset = "Gold",
  frequency="M",
  time = gold.m$time,
  value =  gold.m$value
)
gold <- rbind(gold.d,gold.w,gold.m)

dataset <- rbind(euro, brent, gold)

# tl denominated ----------------------------------------------------------

library(EVDS)

set_evds_key("bSjcFT2O0A")

data <- get_series (
  series = c("TP.DK.USD.A"),
  start_date= "02-01-2002",
  end_date= "30-06-2023"
)

data <-  data$items
data$Tarih <-  as.POSIXct(data$Tarih, format="%d-%m-%Y")
data$TP_DK_USD_A <- as.numeric (data$TP_DK_USD_A)
data[data$Tarih < "2005-1-1", ]$TP_DK_USD_A <- data[data$Tarih < "2005-1-1", ]$TP_DK_USD_A /1000000
data <-  data[c(1,2)]
names(data) <- c("time","value")

data$value <- completion.values(data$value)

brent.tl <- brent.c
brent.tl$value <- completion.values(brent.c$value) * data$value

euro.tl <- euro.c
euro.tl$value <- completion.values(euro.c$value) * data$value

gold.tl <- euro.c
gold.tl$value <- completion.values(gold.c$value) * data$value

n_eurotl <-  euro.tl
n_gold.tl <- gold.tl
n_brent.tl <- brent.tl

n_eurotl$value <- normalize(n_eurotl$value)
n_gold.tl$value <- normalize(n_gold.tl$value)
n_brent.tl$value <- normalize(n_brent.tl$value)

n_eurotl$dataset <-  "Euro" 
n_gold.tl$dataset <- "Gold"
n_brent.tl$dataset <- "Brent"

data <- rbind(n_eurotl,n_gold.tl,n_brent.tl)
data.tl <- data

n_eurotl <-  euro.c
n_gold.tl <- gold.c
n_brent.tl <- brent.c

n_eurotl$value <-  completion.values(n_eurotl$value) 
n_gold.tl$value <-  completion.values(gold.c$value) 
n_brent.tl$value <-  completion.values(n_brent.tl$value) 

n_eurotl$value <- normalize(n_eurotl$value)
n_gold.tl$value <- normalize(n_gold.tl$value)
n_brent.tl$value <- normalize(n_brent.tl$value)

n_eurotl$dataset <-  "Euro" 
n_gold.tl$dataset <- "Gold"
n_brent.tl$dataset <- "Brent"

data <- rbind(n_eurotl,n_gold.tl,n_brent.tl)
data.usd <- data

data.usd$currency = "USD"
data.tl$currency = "TL"
data <- rbind(data.usd,data.tl)
ggplot(data = data,   aes(x = time, y = value, colour = dataset)) + 
  geom_line(size=2) +colors +
  facet_wrap(~currency, ncol = 3)

ggsave("D:/makale/image/tl&usd-grafic-300.pdf", dpi = 300, width = 10, height = 6)

dataset.sum <- data.frame(
  "Brent"=c(summary(euro.c$value)),
  "Gold"=c(summary(gold.c$value)),
  "Euro"=c(summary(brent.c$value)))
dataset.sum <-  t(dataset.sum)

library(GGally)
dataset.cor <- data.frame(
  "Brent"=c(brent.tl$value),
  "Gold"=c(gold.tl$value),
  "Euro"=c(euro.tl$value))

ggcorr(dataset.cor, method = c("everything", "pearson"),label = TRUE,low = "#1a9850", mid = "white", high = "#d73027") 

dataset.cor <- data.frame(
  "Brent"=brent.c$value,
  "Euro"=euro.c$value,
  "Gold"=gold.c$value
  
)
dataset.cor <- data.frame(
  "Brent"=c((dataset[dataset$dataset == "Brent" & dataset$frequency == "D",]$value)),
  "Gold"=c((dataset[dataset$dataset == "Gold" & dataset$frequency == "D",]$value)),
  "Euro"=c((dataset[dataset$dataset == "Euro" & dataset$frequency == "D",]$value))) 

ggcorr(dataset.cor, method = c("everything", "pearson"),palette = "RdBu", label = TRUE) 


 ggplot(dataset, aes(x = time, y = value, color = as.factor(dataset))) +
  geom_line(size = 0.5) +
  facet_wrap(~frequency + dataset, dir = "v") +
  theme(legend.position = "none")

rm(gold.d,gold.w,gold.m,brent.d,brent.w,brent.m,euro.d,euro.w,euro.m,euro, brent, gold,
brent.c,euro.c,gold.c,dataset.cor,data.usd,data.tl,n_gold.tl,n_brent.tl,n_eurotl,euro.tl,gold.tl,brent.tl,start.date,data)
# prediction ------------------------------------------------------------------
library(lattice)
library(prophet)
library(forecast)
library(keras)
library(xts)
library(caret)

per.e.d <- forecasting.packages(dataset[dataset$dataset == "Euro" & dataset$frequency == "D",],"Euro","D",365)
euro.d.pt <-   (per.e.d$performance_table) #performans table
euro.d.fr <-  (per.e.d$finaly.df) #forecasting result

per.e.w <- forecasting.packages(dataset[dataset$dataset == "Euro" & dataset$frequency == "W",],"Euro","W",52)
euro.w.pt <-   (per.e.w$performance_table) #performans table
euro.w.fr <-  (per.e.w$finaly.df) #forecasting result

per.e.m <- forecasting.packages(dataset[dataset$dataset == "Euro" & dataset$frequency == "M",],"Euro","M",12)
euro.m.pt <-   (per.e.m$performance_table) #performans table
euro.m.fr <-  (per.e.m$finaly.df) #forecasting result


per.b.d <- forecasting.packages(dataset[dataset$dataset == "Brent" & dataset$frequency == "D",],"Brent","D",365)
Brent.d.pt <-   (per.b.d$performance_table) #performans table
Brent.d.fr <-  (per.b.d$finaly.df) #forecasting result

per.b.w <- forecasting.packages(dataset[dataset$dataset == "Brent" & dataset$frequency == "W",],"Brent","W",52)
Brent.w.pt <-   (per.b.w$performance_table) #performans table
Brent.w.fr <-  (per.b.w$finaly.df) #forecasting result

per.b.m <- forecasting.packages(dataset[dataset$dataset == "Brent" & dataset$frequency == "M",],"Brent","M",12)
Brent.m.pt <-   (per.b.m$performance_table) #performans table
Brent.m.fr <-  (per.b.m$finaly.df) #forecasting result


per.g.d <- forecasting.packages(dataset[dataset$dataset == "Gold" & dataset$frequency == "D",],"Gold","D",365)
gold.d.pt <-   (per.g.d$performance_table) #performans table
gold.d.fr <-  (per.g.d$finaly.df) #forecasting result

per.g.w <- forecasting.packages(dataset[dataset$dataset == "Gold" & dataset$frequency == "W",],"Gold","W",52)
gold.w.pt <-   (per.g.w$performance_table) #performans table
gold.w.fr <-  (per.g.w$finaly.df) #forecasting result

per.g.m <- forecasting.packages(dataset[dataset$dataset == "Gold" & dataset$frequency == "M",],"Gold","M",12)
gold.m.pt <-   (per.g.m$performance_table) #performans table
gold.m.fr <-  (per.g.m$finaly.df) #forecasting result


p <-  rbind(euro.d.pt,euro.w.pt,euro.m.pt,Brent.d.pt,Brent.w.pt,Brent.m.pt,gold.d.pt,gold.w.pt,gold.m.pt)
g <-  rbind(euro.d.fr,euro.w.fr,euro.m.fr,Brent.d.fr,Brent.w.fr,Brent.m.fr,gold.d.fr,gold.w.fr,gold.m.fr)

g$frequency <- factor( g$frequency, levels = c("D", "W", "M"))

ggplot(data = g,   aes(x = time, y = value, colour = type)) + 
  geom_line(size=2) +colors +
  facet_grid(dataset ~ frequency) 

ggplot(data = g[  g$time >= "2022-1-1" , ],   aes(x = time, y = value, colour = type)) + 
  geom_line(size=2) +colors +
  facet_grid(dataset ~ frequency) 

  # performance table -------------------------------------------------------

p <- purrr::modify_if(p, ~is.numeric(.), ~round(., 3))

xlsx.sum <- list('dataset_summary' = dataset.sum, 'performance_table'=p)
library(openxlsx) 
write.xlsx(xlsx.sum, file = 'D:/makale/makale.xlsx')


rm(euro.d.pt,euro.w.pt,euro.m.pt,Brent.d.pt,Brent.w.pt,Brent.m.pt,gold.d.pt,gold.w.pt,gold.m.pt,
euro.d.fr,euro.w.fr,euro.m.fr,Brent.d.fr,Brent.w.fr,Brent.m.fr,gold.d.fr,gold.w.fr,gold.m.fr,
per.e.d,per.e.w,per.e.m, per.b.d,per.b.w,per.b.m, per.g.d,per.g.w,per.g.m)s