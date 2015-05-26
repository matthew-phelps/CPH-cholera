# Author: Matthew Phelps
#Desc: Interpolation elevation data from old CPH map
# output datasets: raster and vector


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drev\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)

library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(rgdal)



# Load data ---------------------------------------------------------------

elev <- readOGR(dsn = "GIS", layer = "pointElev", stringsAsFactors = F)
plot(elev)
