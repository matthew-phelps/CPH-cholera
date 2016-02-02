# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro
rm(list = ls())  
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data")

setwd(wd.path)

library (ggplot2)
library(raster)
library(rgdal)
## To Instal rgdal on my mac - -note: check pathnames to GDAL.framework
#install.packages("http://cran.r-project.org/src/contrib/rgdal_1.1-3.tar.gz", repos = NULL, type="source", configure.args = "--with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/1.11/unix/bin/gdal-config --with-proj-include=/Library/Frameworks/PROJ.framework/unix/include --with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib")

cph_raster <- brick("/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/GIS/georeferenced_raster/VA_1854_PS_modified.tif")
plotRGB(cph_raster, main="RasterLayer from file")

b <- writeRaster(cph_raster, filename = "test.tiff", format = "GTiff")
