# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library (tidyverse)
library(broom)
library(RColorBrewer)
library(ggsn) # for scale bar
library(mapproj) # for map projections
## To Instal rgdal on my mac - -note: check pathnames to GDAL.framework
#install.packages("http://cran.r-project.org/src/contrib/rgdal_1.1-3.tar.gz", repos = NULL, type="source", configure.args = "--with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/1.11/unix/bin/gdal-config --with-proj-include=/Library/Frameworks/PROJ.framework/unix/include --with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib")

source("Spatial-data-1.R")

# BASE MAP------------------------------------------
base_map <- ggplot() +
  geom_polygon(data = mapdf,
               aes(x = long, y = lat, group = id),
               fill = "grey92", color = "grey") +
  
  theme(axis.title.x = element_blank(), # remove x,y, label
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), # remove tick marks
        axis.text = element_blank(), # no axis labels
        panel.grid.minor = element_blank(), # no gridlines
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), 'lines'))


base_map <- base_map + coord_quickmap()
# Get x/y range to set map to later after new layers added
xrng <- ggplot_build(base_map)$layout$panel_ranges[[1]]$x.range
yrng <- ggplot_build(base_map)$layout$panel_ranges[[1]]$y.range
xrng[2] <- xrng[2] - 200
yrng[2] <- yrng[2] - 200

# ADD LAYERS --------------------------------------------------------------
base_map <- base_map + geom_path(data = wall_fort,
                                 aes(x = long, y = lat,
                                     group = group),
                                 color = "grey")

base_map <- base_map + geom_polygon(data = water_fort,
                                    aes(x = long, y = lat,
                                        group = group),
                                    fill = "#99CCFF",
                                    alpha = 0.7)

base_map
# https://goo.gl/8035ro

base_map <- base_map + ggsn::scalebar(mapdf,
                                      location = "topleft",
                                      dist = 0.5, height = 0.01, st.size = 2,
                                      model = WGS84)
base_map <- base_map + coord_cartesian(xlim = c(xrng),
                                       ylim = c(yrng)) +
  theme(aspect.ratio = 1)





# CLEAN -------------------------------------------------------------------
rm(hosp_df, pipes, hosp, quarter_shp, wall, water)







