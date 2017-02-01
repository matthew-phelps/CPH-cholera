# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library (tidyverse)
library(broom)
library(dplyr)
library(RColorBrewer)
library(ggsn) # for scale bar
library(mapproj) # for map projections
## To Instal rgdal on my mac - -note: check pathnames to GDAL.framework
#install.packages("http://cran.r-project.org/src/contrib/rgdal_1.1-3.tar.gz", repos = NULL, type="source", configure.args = "--with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/1.11/unix/bin/gdal-config --with-proj-include=/Library/Frameworks/PROJ.framework/unix/include --with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib")

source("Spatial-data-1.R")
water@data
proj4string(quarter_shp)
proj4string(water)
# Get spatial data into a form that ggplot2 can handle
# mapdf is what ggplot will use
quarter_df <- as.data.frame(quarter_shp)
quarter_tidy <-tidy(quarter_shp, region = "quarter")
mapdf <- left_join(quarter_tidy, quarter_df, by = c("id" = "quarter"))
mapdf <- mapdf[ order(mapdf$order),]


wall_fort <- tidy(wall, region = "id")
water_fort <- tidy(water, region = "grp")

# BASE MAP------------------------------------------
base_map <- ggplot() +
  geom_polygon(data = mapdf,
               aes(x = long, y = lat, group = id),
               fill = "white", color = "grey") +
  
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


# ADD LAYERS --------------------------------------------------------------
base_map <- base_map + geom_path(data = wall_fort,
                                 aes(x = long, y = lat,
                                     group = group),
                                 color = "grey")

base_map <- base_map + geom_polygon(data = water_fort,
                                 aes(x = long, y = lat,
                                     group = group),
                                 fill = "#99CCFF",
                                 alpha = 0.8)

# LEGEND ------------------------------------------------------------------
# https://goo.gl/8035ro

base_map <- base_map + ggsn::scalebar(mapdf,
                                      location = "topleft",
                                      dist = 0.5, height = 0.01, st.size = 4,
                                      model = WGS84)
base_map <- base_map + coord_cartesian(xlim = c(xrng),
                           ylim = c(yrng)) +
  theme(aspect.ratio = 1)


# CITY WALLS --------------------------------------------------------------








