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

# LEGEND ------------------------------------------------------------------
# https://goo.gl/8035ro

base_map <- base_map + ggsn::scalebar(mapdf,
                                      location = "topleft",
                                      dist = 0.5, height = 0.01, st.size = 2,
                                      model = WGS84)
base_map <- base_map + coord_cartesian(xlim = c(xrng),
                           ylim = c(yrng)) +
  theme(aspect.ratio = 1)


# HOSPITAL LAYER FUNCTION -------------------------------------------------

add_hosp <- function(old_map, hosp_tidy)
  old_map + geom_point(data = hosp_tidy,
                       aes(x = coords.x1, y = coords.x2),
                       size = 3,
                       color = "darkblue")


# INCIDENCE RATE FN -------------------------------------------------------
inc_rate_map <- function(mapdf){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = inc_rate*100),
                          color = "grey")+
    scale_fill_gradientn(name = "Attack rate \nper 100 people",
                         colours = brewer.pal(9, "Reds"),
                         limits=c(0,10)) +
    theme(legend.position = c(0.15,0.15))
}


# CFR FN ------------------------------------------------------------------
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




# PIPE FN -----------------------------------------------------------------
pipe_map <- function(pipes_tidy){
  base_map + geom_path(data = pipes_tidy,
                       aes(x = long, y = lat,
                           group = group),
                       color = "black")
}




# FIRST CASE --------------------------------------------------------------
first_case_map <- function(mapdf){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = start),
                          color = "grey")+
    scale_fill_gradientn(name = "Start week",
                         colours = brewer.pal(3, "Greens")) +
    theme(legend.position = c(0.1,0.15))
}


# CLEAN -------------------------------------------------------------------
rm(hosp_df, pipes, hosp, quarter_shp, wall, water)







