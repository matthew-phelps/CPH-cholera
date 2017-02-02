# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

source("PLOTS/map-base.R")

first_case <- function(mapdf){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = start),
                          color = "grey")+
    scale_fill_gradientn(name = "Start week",
                         colours = brewer.pal(3, "Greens")) +
    theme(legend.position = c(0.1,0.15))
}
first_case(mapdf)

