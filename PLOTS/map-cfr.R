# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library (tidyverse)

source("PLOTS/map-base.R")
base_map
rm(wall, wall_fort, water, water_fort, start, hosp, pipes)

cfr_map <- base_map + geom_polygon(data = mapdf,
                                        aes(x = long, y = lat, group = id,
                                            fill = cfr*100),
                                        color = "grey")+
  scale_fill_gradientn(name = "CFR",
                       colours = brewer.pal(9, "Reds")) +
  theme(legend.position = c(0.1,0.15))
cfr_map