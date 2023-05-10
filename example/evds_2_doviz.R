library (EVDS)
set_evds_key("bSjcFT2O0A")


df <- get_series (
  series = c("TP.DK.USD.A.YTL", "TP.DK.EUR.A"),
  start_date= "25-02-2022",
  end_date= format(Sys.time(), "%d-%m-%Y")
)
head (df$items)

kurVeri <-  df$items
kurVeri$UNIXTIME <-  NULL
kurVeri$Tarih <-  as.POSIXct(kurVeri$Tarih, format="%d-%m-%Y")
kurVeri <- kurVeri[!is.na (kurVeri$TP_DK_USD_A_YTL), ]
kurVeri <- kurVeri[!is.na(kurVeri$TP_DK_EUR_A), ]
kurVeri$TP_DK_USD_A_YTL <- as.numeric (kurVeri$TP_DK_USD_A_YTL )
kurVeri$TP_DK_EUR_A <- as.numeric (kurVeri$TP_DK_EUR_A)

#plot ile gösterimi

plot (kurVeri$Tarih, kurVeri$TP_DK_EUR_A,
      col="red", type="l", xlab = "TARİH", ylab = "KUR")
lines (kurVeri$Tarih, kurVeri$TP_DK_USD_A_YTL, col="blue", type="l")
legend("topleft", legend=c("Dolar", "Euro"), col=c("red", "blue"), lty=1:2, cex=0.8)




