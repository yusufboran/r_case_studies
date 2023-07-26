library(readr)
library(readxl)
library(dplyr)
library(lubridate)
# euro --------------------------------------------------------------------

#https://sdw.ecb.europa.eu/quickview.do?SERIES_KEY=120.EXR.D.USD.EUR.SP00.A
euro.d <- read_csv("D:/yl/dataset/USD-EUR.csv", 
                   col_names = FALSE, skip = 21)[, c(1,2)]
euro.d$X1 <- as.Date(euro.d$X1)
names(euro.d) <- c("time","value")
euro.d$value <-  as.numeric(euro.d$value) 
euro.d$value <- 1 / euro.d$value
euro.d <-  completion.deficiencies(euro.d,"Euro","D")
euro.c <- euro.d$value
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
brent.d <-  brent.d[brent.d$time > "1999-1-1" & brent.d$time < "2023-7-1",  ]
brent.d$time <- as.Date(format(brent.d$time,"%Y-%m-%d"))
brent.d <-  completion.deficiencies(brent.d,"Brent","D")
brent.c <- brent.d$value
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
gold.d <-  gold.d[gold.d$time > "1999-1-4" & gold.d$time < "2023-7-1",  ]
gold.d$time <- as.Date(format(gold.d$time,"%Y-%m-%d"))
gold.d <-  completion.deficiencies(gold.d,"Gold","D")
gold.c <- gold.d$value
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
rm(gold.d,gold.w,gold.m,brent.d,brent.w,brent.m,euro.d,euro.w,euro.m,euro, brent, gold)
# export dataset summary --------------------------------------------------


dataset.sum <- data.frame(
  "Brent"=c(summary(brent.c)),
  "Gold"=c(summary(euro.c)),
  "Euro"=c(summary(gold.c)) )

dataset.sum <-  (format(dataset.sum, digits = 6))

# dataset correlation -----------------------------------------------------

library(GGally)
dataset.cor <- data.frame(
  "Brent"=brent.c,
  "Euro"=euro.c,
  "Gold"=gold.c
  
)
dataset.cor <- data.frame(
  "Brent"=c((dataset[dataset$dataset == "Brent" & dataset$frequency == "D",]$value)),
  "Gold"=c((dataset[dataset$dataset == "Gold" & dataset$frequency == "D",]$value)),
  "Euro"=c((dataset[dataset$dataset == "Euro" & dataset$frequency == "D",]$value))) 

ggcorr(dataset.cor, method = c("everything", "pearson"),palette = "RdBu", label = TRUE) 
ggplot( dataset , aes(x=time, y=value, color=as.factor(dataset)  )) + 
  geom_line(size=1) +  
  facet_wrap(~frequency+dataset , dir="v")  +
  theme(legend.position="none")


rm(brent.c,euro.c,gold.c,dataset.cor)
# prediction ------------------------------------------------------------------
library(prophet)
library(forecast)
library(keras)
library(xts)
library(caret)


per.e.d <- forecasting.packages(dataset[dataset$dataset == "Euro" & dataset$frequency == "D",],"Euro 1D",365)
euro.d.pt <-   (per.e.d$performance_table) #performans table
euro.d.fr <-  (per.e.d$finaly.df) #forecasting result

per.e.w <- forecasting.packages(dataset[dataset$dataset == "Euro" & dataset$frequency == "W",],"Euro 1W",52)
euro.w.pt <-   (per.e.w$performance_table) #performans table
euro.w.fr <-  (per.e.w$finaly.df) #forecasting result

per.e.m <- forecasting.packages(dataset[dataset$dataset == "Euro" & dataset$frequency == "M",],"Euro 1M",12)
euro.m.pt <-   (per.e.m$performance_table) #performans table
euro.m.fr <-  (per.e.m$finaly.df) #forecasting result


per.b.d <- forecasting.packages(dataset[dataset$dataset == "Brent" & dataset$frequency == "D",],"Brent 1D",365)
Brent.d.pt <-   (per.b.d$performance_table) #performans table
Brent.d.fr <-  (per.b.d$finaly.df) #forecasting result

per.b.w <- forecasting.packages(dataset[dataset$dataset == "Brent" & dataset$frequency == "W",],"Brent 1W",52)
Brent.w.pt <-   (per.b.w$performance_table) #performans table
Brent.w.fr <-  (per.b.w$finaly.df) #forecasting result

per.b.m <- forecasting.packages(dataset[dataset$dataset == "Brent" & dataset$frequency == "M",],"Brent 1M",12)
Brent.m.pt <-   (per.b.m$performance_table) #performans table
Brent.m.fr <-  (per.b.m$finaly.df) #forecasting result



per.g.d <- forecasting.packages(dataset[dataset$dataset == "Gold" & dataset$frequency == "D",],"Gold 1D",365)
gold.d.pt <-   (per.g.d$performance_table) #performans table
gold.d.fr <-  (per.g.d$finaly.df) #forecasting result

per.g.w <- forecasting.packages(dataset[dataset$dataset == "Gold" & dataset$frequency == "W",],"Gold 1W",52)
gold.w.pt <-   (per.g.w$performance_table) #performans table
gold.w.fr <-  (per.g.w$finaly.df) #forecasting result

per.g.m <- forecasting.packages(dataset[dataset$dataset == "Gold" & dataset$frequency == "M",],"Gold 1M",12)
gold.m.pt <-   (per.g.m$performance_table) #performans table
gold.m.fr <-  (per.g.m$finaly.df) #forecasting result

p <-  rbind(euro.d.pt,euro.w.pt,euro.m.pt,Brent.d.pt,Brent.w.pt,Brent.m.pt,gold.d.pt,gold.w.pt,gold.m.pt)
g <-  rbind(euro.d.fr,euro.w.fr,euro.m.fr,Brent.d.fr,Brent.w.fr,Brent.m.fr,gold.d.fr,gold.w.fr,gold.m.fr)


rm(euro.d.pt,euro.w.pt,euro.m.pt,Brent.d.pt,Brent.w.pt,Brent.m.pt,gold.d.pt,gold.w.pt,gold.m.pt)
rm(euro.d.fr,euro.w.fr,euro.m.fr,Brent.d.fr,Brent.w.fr,Brent.m.fr,gold.d.fr,gold.w.fr,gold.m.fr)
rm(per.e.d,per.e.w,per.e.m, per.b.d,per.b.w,per.b.m, per.g.d,per.g.w,per.g.m)

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
p$value[is.na(p$value)] <- 0
p$value <-  round( as.numeric(p$value ), digits = 3)

ggplot(data = g[  g$time >= "2022-1-1" , ],   aes(x = time, y = value, colour = type)) + 
  geom_line(size=2) +colors +
  facet_wrap(~dataset, ncol = 3)

#performance.g[performance.g$Metric == "Time",]$value <-performance.g[performance.g$Metric == "Time",]$value /100

ggplot(p, aes(fill=type, y=value, x=Metric  )) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~dataset, ncol = 3)

ggplot(p[p$Metric != "Time",], aes(fill=type, y=value, x=Metric  )) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~dataset, ncol = 3)



# performance table -------------------------------------------------------

dizi <- c("Euro 1D","Euro 1W","Euro 1M","Brent 1D","Brent 1W","Brent 1M","Gold 1D","Gold 1W","Gold 1M")
xlsx.sum <- list('dataset_summary' = dataset.sum)

for (item in dizi) {
  dataset.sum <- data.frame(
    "deneme" =  c( "Cor", "R^2", "RMSE", "MAE", "Time"),
    LSTM = c(p[p$dataset == item,]$value[1:5]),
    Prophet =c(p[p$dataset == item,]$value[6:10]),
    c =c(p[p$dataset == item,]$value[11:15]),
    LR      =c(p[p$dataset == item,]$value[16:20])
  )
  new_names <- c(item, "LSTM", "Prophet", "ARIMA","LR")
  colnames(dataset.sum) <- new_names

  xlsx.sum[[ gsub(" ", "_", item)]] <- dataset.sum
}
xlsx.sum
library(openxlsx) 
write.xlsx(xlsx.sum, file = 'D:/makale/makale.xlsx')

