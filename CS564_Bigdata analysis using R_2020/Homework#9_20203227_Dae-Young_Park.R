#1
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
require(maps)

names = c("China", "North Korea", "South Korea", "Japan")
res <- map_data("world", region = names)
region.label <- res %>% group_by(region) %>% summarise(long = mean(long), lat = mean(lat))

ggplot(res, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region, alpha=0.3))+
  geom_text(aes(label = region), data = region.label,  size = 3, hjust = 0.5)+
  coord_fixed(1.3) +
  theme(legend.position = "none")

#2
library(ggmap)
library(googleVis)
api_key <- 'insert your key'
register_google(key=api_key)

kaist_loc <- geocode("KAIST", source="google")
kaist_loc

ggmap(get_map(location=kaist_loc,zoom=15, maptype='roadmap')) + geom_point(data=kaist_loc, aes(x=lon,y=lat), size=4, alpha=0.8, col="red")
