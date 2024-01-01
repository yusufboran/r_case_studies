library(geojsonio)
library(sp)
library(broom)
library(ggplot2)

tr <- geojson_read("D:/yl/r_case_studies/map/tr-cities.json",  what = "sp")

plot(tr, col="grey")

ggplot() +
  geom_polygon(data = tr, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
 
  theme_void() +
  coord_map()
