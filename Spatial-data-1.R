# Author: Matthew Phelps
# DESC: Prepare spatial data for plotting


source("Data-3-combine quarters.R")


# QUARTER SUMMARY DATA FRAME ---------------------------------------------------------
# Date of first infected case in each quarter
start <- combined %>%
  group_by(quarter) %>%
  dplyr::summarize(start = 1 + week.id[min(which(sick.total.week >0))])
combined <- left_join(combined, start, by="quarter")


# Total morb & mort rate for each quarter
comb_summary <- combined %>%
  dplyr::filter(week.id == 15) %>%
  dplyr::select(-week.id, -sick.total.week, -dead.total.week) %>%
  dplyr::arrange(quarter)
comb_summary$cum_incidence = comb_summary$cum.sick / comb_summary$est.pop.1853 * 1000


# QUARTER POLYGONS ------------------------------------------------------
# 
# load("Rdata/quarter_eng.Rdata")
# quarter.sheet <- reshape::rename(quarter, replace = c("sick.total.week" = "I"))

# shapefile
quarter_shp <- readOGR(dsn = "Data/GIS", layer = "CPH_Quarters3", stringsAsFactors = F)
# plot(quarter_shp)

# Not sure why, byt readOGR assigns incorrect projection (I think) to obj. Need
# to overwrite assigned projection here, despite Warning message. This is NOT a
# reprojection. This sort of explains the difference: https://goo.gl/jN066m
proj4string(quarter_shp) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

# housekeeping
colnames(quarter_shp@data)[2] <- "quarter"
quarter_shp@data$id <- as.numeric(quarter_shp@data$id)
quarter_shp@data$cumSick_rt <- quarter_shp@data$cum_cases <- NULL
quarter_shp@data$area_1 <- NULL


# quarter_shp@data



# Find area of all quarters:
#https://goo.gl/No33KP

# Projection defines units that "area" will be calculated in
# proj4string(quarter_shp)

quarter_shp@data$area <- sapply(slot(quarter_shp, "polygons"), slot, "area")


# Join area data with disease data
quarter_shp@data <- inner_join(quarter_shp@data, comb_summary,
                               by = c("quarter"))

quarter_shp@data$pop_density <- quarter_shp@data$est.pop.1853 / quarter_shp@data$area
quarter_shp@data$infect_density <- quarter_shp@data$area / quarter_shp@data$cum.sick


#plot(quarter_shp@data$cum_incidence ~ quarter_shp@data$pop_density)

# SHP OUTPUT ---------------------------------------------------------

# Transform to EPSG:3857
quarter_shp <- spTransform(quarter_shp, CRSobj = "+init=epsg:3857")
# proj4string(quarter_shp)

# Save if I need output for QGIS
# writeOGR(obj = quarter_shp, dsn = "Data/GIS", layer = "CPH_Quarters_4",
#          driver = "ESRI Shapefile", overwrite_layer = TRUE)


# CITY WALL & WATER POLYLINE ------------------------------------------------------
wall <- readOGR(dsn = "Data/GIS", layer = "city_wall", stringsAsFactors = F)
water <- readOGR(dsn = "Data/GIS", layer = "cph_water_area", stringsAsFactors = F)
pipes <- readOGR(dsn = "Data/GIS", layer = "Mads_Pipes", stringsAsFactors = F)
hosp <- readOGR(dsn = "Data/GIS", layer = "cph_hosp_poorhouses", stringsAsFactors = F)

# proj4string(hosp)
hosp <- spTransform(hosp, CRSobj = "+init=epsg:3857")
