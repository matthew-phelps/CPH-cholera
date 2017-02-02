# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library (tidyverse)

source("PLOTS/map-base.R")
rm(wall, wall_fort, water, water_fort, start, hosp, pipes)
# Title location:


cfr_map <- function(mapdf){
  x_loc <- (xrng[2] - xrng[1]) /2 + xrng[1]
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = cfr*100),
                          color = "grey")+
    scale_fill_gradientn(name = "CFR",
                         colours = brewer.pal(9, "Reds"),
                         limits = c(50,80)) +
    theme(legend.position = c(0.1,0.15))
}
cfr_map(mapdf)

cfr_map(mapdf) + geom_point(data = hosp_tidy,
                               aes(x = coords.x1, y = coords.x2),
                               size = 3,
                               color = "black")
ggsave()
