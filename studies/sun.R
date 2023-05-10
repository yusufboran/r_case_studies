library(readxl)
library(dplyr)
library(zoo)
library(fpp2)
library(ggplot2)


colors <- list(
  golden_crest = "#f89d1e",
  persian_red = "#ca3074",
  parachute_purple = "#332851",
  aqua_fiesta = "#77aeaf"
)

#import dataset
ges_dataset <- read_excel("C:/Users/yusuf/Desktop/ges_dataset.xlsx")
#char to time
ges_dataset$Zaman <- as.Date(ges_dataset$Zaman,"%Y-%m-%d")   
ges_dataset <- ges_dataset[!is.na(ges_dataset$ORTALAMA_SICAKLIK), ]

#median filter and raw data graphic
mav <- function(x,n){rollmean(x, k=n, fill = NA)} #simple moving average
mmed <- function(x,n){runmed(x,n)} #Median, data and number of days

decom <- function(x){
  
  ts_data <- ts(x, frequency = 12)
  de_data <- decompose(ts_data)
  return (de_data)
}

t <- ges_dataset$Zaman                         
x <- ges_dataset$Gunluk_uretim

plot(t, x, type = "l", ylab = "Güneş Üretimi",xlab="Zaman" , col= colors$parachute_purple)

lines(t, mmed(x,7), col=colors$persian_red, lwd=2) 
lines(t, mav(x,7), col = colors$golden_crest, lwd=2) 
legend("bottomright", legend = c("Dataset","Medyan", "Hareketli Ortalama"), col = c(colors$parachute_purple,colors$golden_crest, colors$persian_red), lty = c(1, 1), cex=0.8,
       box.lty=0)


summary(ges_dataset$Gunluk_uretim)
dataset_de <- decom(ges_dataset$Gunluk_uretim)
plot(dataset_de)

ges_dataset_median <- ges_dataset
ges_dataset_median$Gunluk_uretim <-mmed( mav(x,10),5)

summary(ges_dataset_median$Gunluk_uretim)
dataset_de <- decom(ges_dataset_median$Gunluk_uretim)
plot(dataset_de)


#2021-01-01/2021-12-31 data between

filtered_data_2021 <- ges_dataset_median %>% filter(Zaman >=as.Date("2021-01-01") & Zaman <= as.Date("2021-12-31"))
plot(x = filtered_data_2021$Zaman,y = filtered_data_2021$Gunluk_uretim,type = "l")
filtered_data_2021_de <-decom(filtered_data_2021$Gunluk_uretim) 
plot(filtered_data_2021_de)
#2022-01-01/2022-12-31 data between

filtered_data_2022 <- ges_dataset_median %>% filter(Zaman >= as.Date("2022-01-01") & Zaman <= as.Date("2022-12-31"))
plot(x = filtered_data_2022$Zaman,y = filtered_data_2022$Gunluk_uretim,type = "l")
filtered_data_2022_de <-decom(filtered_data_2022$Gunluk_uretim)  
plot(filtered_data_2022_de)


#Displaying the data of the last two years in a single graphic
ts_2021 <- ts(filtered_data_2021$Gunluk_uretim,  start=2000, frequency = 52)
ts_2022 <- ts(filtered_data_2022$Gunluk_uretim,start=2000, frequency = 52)

#correlation
correlation <-  round( cor(filtered_data_2021$Gunluk_uretim[0:285],filtered_data_2022$Gunluk_uretim[0:285]), digits = 4)


plot(ts_2021, type = "l", ylab = "Güneş Üretimi",xlab="Zaman" , col= colors$golden_crest)
lines(ts_2022, col=colors$persian_red, lwd=2) 
legend("bottom", legend = c("2021", "2022"), col = c(colors$golden_crest, colors$persian_red), lty = c(1, 1), cex=0.8,
       box.lty=0)

legend("topright", bg="transparent", legend =paste0("correlation : ", correlation) , cex=0.8,
       box.lty=0)



# REGRESYON ---------------------------------------------------------------

#start_time <- Sys.time()

x=1:356
y=filtered_data_2021$Gunluk_uretim
z=filtered_data_2021$ORTALAMA_SICAKLIK
k=filtered_data_2021$ORTALAMA_NEM
t=filtered_data_2021$GUNLUK_ORTALAMA_HIZI
# 3. dereceden  poly ile regresyon oluşturma
model_reg <- lm(y ~  poly(x ,  degree=3)+ poly(z ,  degree=3)+poly(k ,  degree=3)+poly(t ,  degree=3))
summary(model_reg)

plot(x, y, xlab = "0-365 gün",ylab = "elektrik üreteim miktarı")
y=predict(model_reg,newdata=list(x=seq(
  from=0,to=356,length.out=356)),
  interval="confidence")
matlines(x,y, lwd=2)



#end_time <- Sys.time()
#end_time - start_time

