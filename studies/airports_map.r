library(tidyverse)
library(maps)
library(mapproj)
library(readxl)
library(readr)
library(dplyr)
library(viridis)

tr <- map_data("world") %>% filter(region  == "Turkey")

airports.tr <- read_csv("D:/yl/dataset/list-of-airports-in-turkey-hxl-tags-1.csv",skip = 1)
airports.tr <- filter(airports.tr, `#loc +airport +type` %in% c("large_airport", "medium_airport","small_airport"))
colnames(airports.tr)[3:6] <- c("type", "name", "latitude", "longitude")
airports.tr <- airports.tr[3:6]

passenger.aircraft <-  read_excel("D:/yl/dataset/uçaksayısı haziran.xlsx", sheet = "YOLCU",skip = 2)
passenger.aircraft <- head(passenger.aircraft[1:7],-8)
passenger.aircraft <-passenger.aircraft[passenger.aircraft$Toplam...7 != 0 ,]
 

tr.map <- tr %>% ggplot( mapping=aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray80", color = "gray80")+
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(1,5,10,20,50,100), name="Number of restaurant", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Flights to Turkey in June",
    subtitle = "number of passengers",
    caption = "Data: data.world | Creation: Yusuf BORAN "
  ) +
  scale_color_manual(
    values = c(
      "large_airport" = "#271263",
      "medium_airport" = "#ffd700",
      "small_airport"="#37745B"
    )
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
  )+
  coord_map("ortho", orientation = c(39, 40, 0))
tr.map
tr.map +geom_point(data = airports.tr, 
             aes(x = longitude, y = latitude, color=type, size= 3,group = NULL))





sonuc_dizi <- gsub(" \\(\\*\\)|-", " ", passenger.aircraft$...1) %>% strsplit(" ") 


cities  <- character(0)
for(city in sonuc_dizi){
  if ((city[1] == "Muğla" || city[1] == "İstanbul" || city[1] == "İzmir"|| city[1] == "Ankara") & !is.na(city[2] ) ) {
    
    cities <- c(cities,city[2])
  }
  else{
    cities <- c(cities,city[1])
  }
}

find_word_index <- function(word_list, target_word) {
  for (i in seq_along(word_list)) {
    if (grepl(target_word, word_list[i])) {
      return(i)
    }
  }
  return(NA) 
}
city.index <- character(0)
for(city in cities){
  result <- find_word_index(airports.tr$name, city)
  city.index <-  c(city.index, result)
}

city.index <- as.numeric(city.index)
city.index[21] <- 19


data <- data.frame()
for (i in 1: length(city.index)){

  new <- data.frame(
   
    airports.tr[ city.index[i] ,],
    "2022" = passenger.aircraft[i,]$Toplam...4 ,
    "2023" = passenger.aircraft[i,]$Toplam...7 
  )
  data <- rbind(data,new)
  
}

tr.map+  geom_point(data = data, 
             aes(x = longitude, y = latitude, color=type, size= X2023/X2022,group = type))+
  geom_point(data = data, 
             aes(x = longitude, y = latitude, color="red", size= c,group = type))
 
