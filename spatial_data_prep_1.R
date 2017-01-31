# Author: Matthew Phelps
# DESC: Preparing GIS data
# Output: shp files ready for plotting
rm(list=ls())
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
getwd()

source("Rcodes/A-1_simple_stats.R")

# POPULATION DENSITY ------------------------------------------------------
# 
# load("Rdata/quarter_eng.Rdata")
# quarter.sheet <- reshape::rename(quarter, replace = c("sick.total.week" = "I"))

# shapefile
quarter_shp <- readOGR(dsn = "GIS", layer = "CPH_Quarters3", stringsAsFactors = F)
plot(quarter_shp)

# Not sure why, byt readOGR assigns incorrect projection (I think) to obj. Need
# to overwrite assigned projection here, despite Warning message. This is NOT a
# reprojection. This sort of explains the difference: https://goo.gl/jN066m
proj4string(quarter_shp) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

# housekeeping
colnames(quarter_shp@data)[2] <- "quarter"
quarter_shp@data$id <- as.numeric(quarter_shp@data$id)
quarter_shp@data$cumSick_rt <- quarter_shp@data$cum_cases <- NULL
quarter_shp@data$area_1 <- NULL


quarter_shp@data



# Find area of all quarters:
#https://goo.gl/No33KP

# Projection defines units that "area" will be calculated in
proj4string(quarter_shp)

quarter_shp@data$area <- sapply(slot(quarter_shp, "polygons"), slot, "area")


# Join area data with disease data
quarter_shp@data <- inner_join(quarter_shp@data, comb_summary,
                               by = c("quarter"))

quarter_shp@data$pop_density <- quarter_shp@data$est.pop.1853 / quarter_shp@data$area
quarter_shp@data$infect_density <- quarter_shp@data$area / quarter_shp@data$cum.sick


plot(quarter_shp@data$cum_incidence ~ quarter_shp@data$pop_density)
plot(quarter_shp)
# SAVE SHP OUTPUT ---------------------------------------------------------

# Transform to EPSG:3857
quarter_shp <- spTransform(quarter_shp, CRSobj = "+init=epsg:3857")
proj4string(quarter_shp)

writeOGR(obj = quarter_shp, dsn = "GIS", layer = "CPH_Quarters_4",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)



# # AGE_ADJUSTED MORTALITY -------------------------------------------------
# mortality_rates <- age_ts$age[1:22]
# mortality_rates <- as.data.frame(mortality_rates)
# mortality_rates$rate <- age_mortality$total_sick / age_ts$total1853[1:22]
# mortality_rates <- plyr::rename(mortality_rates, replace = c('mortality_rates' = 'age'))
# 
# plot(mortality_rates)
# 
# save(mortality_rates, file = 'Rdata/mortality_rates.Rdata')
# 
