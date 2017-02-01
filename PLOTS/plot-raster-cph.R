# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library (tidyverse)
library(rasterVis)
library(rgdal)
library(maptools)
library(dplyr)
library(rgeos)
## To Instal rgdal on my mac - -note: check pathnames to GDAL.framework
#install.packages("http://cran.r-project.org/src/contrib/rgdal_1.1-3.tar.gz", repos = NULL, type="source", configure.args = "--with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/1.11/unix/bin/gdal-config --with-proj-include=/Library/Frameworks/PROJ.framework/unix/include --with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib")



# SHP file loading --------------------------------------------------------

load("Rdata\\quarter_eng.Rdata")
quarter.sheet <- reshape::rename(quarter, replace = c("sick.total.week" = "I"))

# shapefile
quarter.shp <- readOGR(dsn = "GIS", layer = "CPH_Quarters3", stringsAsFactors = F)
plot(quarter.shp)
quarter.shp@data$id <- as.numeric(quarter.shp@data$id)
quarter.shp@data

# Find area of all quarters:
#https://goo.gl/No33KP
sapply(slot(quarter.shp, "polygons"), slot, "area")

# Get spatial data into a form that ggplot2 can handle
# mapdf is what ggplot will use
quarter.df <- as.data.frame(quarter.shp)
quarter.fortified <-fortify(quarter.shp, region = "Quarter")
# quarter.lines <- join (quarter.fortified, quarter.df, by = "id")

#quarter.fortified$id <- as.numeric(quarter.fortified$id)

mapdf <- left_join(quarter.fortified, quarter.sheet, by = c("id" = "quarter"))
mapdf <- mapdf[ order(mapdf$order),]




# BACKGROUND MAP ----------------------------------------------------------
# Help from http://goo.gl/gU0rJx
map <- raster("GIS/georeferenced_raster/IA_1855_CROP_modified_compress_low.tif")

# Only do this code chunk if I need auto color scales 
# map_points <- rasterToPoints(map)
# map_df <- data.frame(map_points)

cph_map <- gplot(map, maxpixels = 5e4) +
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = "black", high = "white", guide = F) +
  coord_equal()


map_p <- rasterToPoints(map)
map_df <- data.frame(map_p)
colnames(map_df) <- c("lon", "lat", "MAP")

ggplot(data = map_df,
       aes(y = lat, x = lon)) +
  geom_raster(aes(fill = MAP)) +
  

# OVERLAY DATA ON BACKGROUND MAP ------------------------------------------

cholera_map <- ggplot()+
  geom_polygon(data = mapdf[which(mapdf$week.id == 15),],
               aes(x = long, y = lat, group = id, 
                   fill = (cum.sick/est.pop.1853)*100)) +
  #coord_equal(ratio = 0) +
  scale_fill_gradientn(name = "Cumulative infections \nper 100 people", colours = brewer.pal(9, "Reds")) +
#   geom_segment(data = sbar,
#                size = 1.3,
#                aes(x = lon.start,
#                    xend = lon.end,
#                    y = lat.start,
#                    yend = lat.end)) +
#   geom_text(data = sbar,
#             aes(x = (lon.start + lon.end)/2,
#                 y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),
#                 label = paste(format(distance, 
#                                      digits = 2,
#                                      nsmall = 2),
#                               'km')),
#             hjust = 0.5,
#             vjust = 0,
#             size = 19/ptspermm) +
  theme(axis.title.x = element_blank(), # remove x,y, label
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), # remove tick marks
        axis.text = element_blank(), # no axis labels
        
        panel.grid.minor = element_blank(), # no gridlines
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), 'lines')
  ) +
  ggtitle("Cumulative infection \n at end of outbreak\n ") +
  theme(plot.title = element_text(size = 24, face="bold"),
        legend.title = element_text(size = 19),
        legend.position = 'bottom')
cholera_map


# LEGEND ------------------------------------------------------------------
# Legend: see http://goo.gl/utU1lh
bb <- attr(cph_map, 'bb')
distHaversine <- function(long, lat){
  dlong = (long[2] - long[1])*pi/180
  dlat  = (lat[2] - lat[1])*pi/180
  
  # Haversine formula:
  R = 6371;
  a = sin(dlat/2)*sin(dlat/2) + cos(lat[1])*cos(lat[2])*sin(dlong/2)*sin(dlong/2)
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  d = R * c
  return(d) # in km
}


sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                   lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                   lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                   lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))

sbar$distance = distHaversine(long = c(sbar$lon.start,sbar$lon.end),
                              lat = c(sbar$lat.start,sbar$lat.end))

ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.







