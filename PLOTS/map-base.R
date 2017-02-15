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




add_hosp <- function(old_map, hosp_tidy){
  old_map + geom_point(data = hosp_tidy,
                       aes(x = coords.x1, y = coords.x2),
                       size = 3,
                       color = "darkblue")
}


# add_hosp_legend <- function(old_map, hosp_tidy){
#   get_y <- function(x){
#     ((max(x) - min(x)) / 2) + min(x)
#   }
#   old_map + geom_point(data = hosp_tidy,
#                        aes(x = 1401630, y = 7496379),
#                        size = 3,
#                        color = "green")
# }


inc_rate_map <- function(mapdf){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = AR),
                          color = "grey")+
    scale_fill_gradientn(name = "Attack rate \nper 100 people",
                         colours = brewer.pal(9, "Reds"),
                         limits=c(0,10)) +
    theme(legend.position = c(0.15,0.15))
}



cfr_map <- function(mapdf){
  x_loc <- (xrng[2] - xrng[1]) /2 + xrng[1]
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = CFR),
                          color = "grey")+
    scale_fill_gradientn(name = "CFR",
                         colours = brewer.pal(9, "Reds"),
                         limits = c(50,80)) +
    theme(legend.position = c(0.1,0.15))
}





pipe_map <- function(pipes_tidy){
  base_map + geom_path(data = pipes_tidy,
                       aes(x = long, y = lat,
                           group = group),
                       color = "black")
}





first_case_map <- function(mapdf){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = start),
                          color = "grey")+
    scale_fill_gradientn(name = "Start week",
                         colours = brewer.pal(3, "Greens"),
                         limits = c(1,3),
                         breaks = c(1,2,3)) +
    theme(legend.position = c(0.1,0.15))
}


## Look at:https://gist.github.com/hadley/233134
## for how to discritize the scale

R_ext_map <- function(mapdf, Log = FALSE){
  if(Log){
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = log(R_ext)),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(min(log(mapdf$R_ext)),
                                      max(log(mapdf$R_ext))))+
      theme(legend.position = c(0.1,0.15))
  } else {
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = R_ext),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(0,
                                      max(mapdf$R_ext)))+
      theme(legend.position = c(0.1,0.15))
  }
}

R_int_map <- function(mapdf, Log = FALSE){
  if(Log){
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = log(R_int)),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(min(log(mapdf$R_int)),
                                      max(log(mapdf$R_int))))+
      theme(legend.position = c(0.1,0.15))
  } else {
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = R_int),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(0,
                                      max(mapdf$R_int)))+
      theme(legend.position = c(0.1,0.15))
  }
}

add_map_lab <- function(old_map, centroids){
  old_map + geom_text(data = centroids,
                      aes(label = quarter,
                          x = lab_long, y = lab_lat))
}
# CLEAN -------------------------------------------------------------------
rm(hosp_df, pipes, hosp, quarter_shp, wall, water)







