# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library(cowplot) # multiplots: https://goo.gl/RK49b0
library(tidyverse)
library(RColorBrewer)
source("functions/MapBaseAndLayers.R")
source("Spatial-data-1.R")
plot <- multiPlotWrapper(mapdf, wall_fort, water_fort, l_size = 0.1,
                 wall_l_size = 0.3, p_size = 1.2, txt_size = 10,
                 leg_height = 2.5, transp = 0.7)

plot
save_plot(plot, file = "Plot-output/map_multi.pdf",
          base_height = 10)

# Get centroids of each polygon


  

