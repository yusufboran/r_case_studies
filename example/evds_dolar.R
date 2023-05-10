library (RCurl) # EVDS'ye bağlantı için
library (XML) # XML veri kullanımı için
library (plyr) # XML veriyi data. frame'e çevirmek için

anahtar <-  "bSjcFT2O0A"

tcmb_evds <-function (veriseti, baslangic, bitis, anahtar) {
  adres <-  "https://evds2.tcmb.gov.tr/service/evds/"
  seri <-  paste("series=", veriseti, sep="")
  tarihler <-  paste ("&startDate=", baslangic, "&endDate=",bitis, sep="")
  tamamlayici <-  paste ("&type=xml&key=", anahtar, sep="")
  veriadresi <-  paste (adres, seri, tarihler, tamamlayici, sep="")
  xml_veri <-  getURL (veriadresi, .opts = list (ssl.verifypeer=FALSE))
}

xml_veriler <- tcmb_evds ("TP.DK.USD.A", "1-1-2015", "29-10-2020",anahtar)


df <- xmlToDataFrame (xml_veriler)
kayitsayisi <- nrow(df)
df <- df [2: kayitsayisi,]
df <- df [2:3]
df <- df [df$TP_DK_USD_A != "", ]
str(df)
df$Tarih <- strptime (df$Tarih, "%d-%m-%Y")
df$TP_DK_USD_A <-as.numeric(as.character.numeric_version (df$TP_DK_USD_A))
plot(y=df$TP_DK_USD_A, x=df$Tarih, type = "l", col="red", xlab = "Tarih",ylab="TL/$")