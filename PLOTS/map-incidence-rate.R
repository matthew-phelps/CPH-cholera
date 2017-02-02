# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library (tidyverse)

source("PLOTS/map-base.R")
base_map
proj4string(hosp)
hosp_df <- as.data.frame(hosp@data)
hosp_tidy <- tidy(hosp)

inc_rate_map <- base_map + geom_polygon(data = mapdf,
                        aes(x = long, y = lat, group = id,
                            fill = inc_rate*100),
                        color = "grey")+
  scale_fill_gradientn(name = "Cumulative infections \nper 100 people",
                                colours = brewer.pal(9, "Reds")) +
  theme(legend.position = c(0.1,0.15))
inc_rate_map

hosp_map <- inc_rate_map + geom_point(data = hosp_tidy,
                                      aes(x = coords.x1, y = coords.x2),
                                      size = 3,
                                      color = "black")
hosp_map
z
