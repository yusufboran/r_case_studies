library(readxl)
library(dplyr)
library(zoo)
library(olsrr)

mav <- function(x,n){rollmean(x, k=n, fill = NA)} #simple moving average
mmed <- function(x,n){runmed(x,n)}

raw_data <- read_excel("C:/Users/yusuf/Desktop/ges_dataset.xlsx")
raw_data$Zaman <- as.Date(raw_data$Zaman,"%Y-%m-%d")   
raw_data <- raw_data[!is.na(raw_data$ORTALAMA_SICAKLIK), ]

plot(x=raw_data$Zaman, y=raw_data$Gunluk_uretim)
summary(raw_data$Gunluk_uretim)

data <- raw_data
data$Gunluk_uretim <-mmed( mav(data$Gunluk_uretim,13),7)
data$ORTALAMA_SICAKLIK <-mmed( mav(data$ORTALAMA_SICAKLIK,13),7)
data$ORTALAMA_NEM <-mmed( mav(data$ORTALAMA_NEM,13),7)
data$GUNLUK_ORTALAMA_HIZI <-mmed( mav(data$GUNLUK_ORTALAMA_HIZI,13),7)
plot(x=data$Zaman, y=data$Gunluk_uretim)
summary(data$Gunluk_uretim)

data <- data %>% filter(GUNLUK_ORTALAMA_HIZI  !="NaN")
data <- data %>% filter(Zaman >=as.Date("2021-01-01") & Zaman <= as.Date("2021-12-31"))


model<-lm(data$Gunluk_uretim ~poly(data$Zaman ,3))
summary(model)

confint(model)

par(mfrow=c(2,2))
plot(model)


