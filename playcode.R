library(tidyverse)
library(viridis)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(gganimate)
library(transformr)
library(gifski)

# Import county-level sales price data
rm(list=ls())
load("playdata.Rda")

# Pull map data
states = map_data("state")
counties = map_data("county")

# Merge county dataframe with playdata using cleaned names
playdata = merge(playdata, counties, by.x = c("County/County Equivalent", "State Name"), 
                      by.y = c("subregion", "region"))
playdata = playdata[order(playdata$"date", playdata$"group", playdata$"order"),]

# Plot graph of January sales prices--works just fine ...
playdata_one = subset(playdata, playdata$date == 202001)
graph1 = ggplot() + 
  coord_fixed(1.3) +
  labs(title = "Median sales prices", subtitle = "January 2020") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +  
  geom_polygon(data = playdata_one, aes(x = long, y = lat, fill = saleprice, group = group)) + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color="black", fill = NA) +
  scale_fill_viridis(na.value="transparent")

# Plot animated graph of January to March--fails ...
graph2 = ggplot() + 
  coord_fixed(1.3) +
  labs(title = "Median sales prices", subtitle = "{unique(msa_map_final$date[msa_map_final$date == {frame_time}])}") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +  
  geom_polygon(data = playdata, aes(x = long, y = lat, fill = saleprice, group = group)) + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color="black", fill = NA) +
  scale_fill_viridis(na.value="transparent") + 
  transition_states(date)

anim = animate(graph2)

anim