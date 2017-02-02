# Author: Matthew Phelps
# DESC: Prepare spatial data for plotting


source("Data-3-combine quarters.R")


# QUARTER SUMMARY DATA FRAME ---------------------------------------------------------
# Date of first infected case in each quarter
start <- combined %>%
  group_by(quarter) %>%
  dplyr::summarize(start = 1 + week.id[min(which(sick.total.week >0))])
combined <- left_join(combined, start, by="quarter")


# QUARTER POLYGONS ------------------------------------------------------

# Load shapefile
quarter_shp <- readOGR(dsn = "Data/GIS", layer = "CPH_Quarters3", stringsAsFactors = F)


# Not sure why, byt readOGR assigns incorrect projection (I think) to obj. Need
# to overwrite assigned projection here, despite Warning message. This is NOT a
# reprojection. This sort of explains the difference: https://goo.gl/jN066m
proj4string(quarter_shp) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

# housekeeping
colnames(quarter_shp@data)[2] <- "quarter"
quarter_shp@data$id <- as.numeric(quarter_shp@data$id)
quarter_shp@data$cumSick_rt <- quarter_shp@data$cum_cases <- NULL
quarter_shp@data$area_1 <- NULL


# AREA CALCULATION --------------------------------------------------------
# Find area of all quarters: https://goo.gl/No33KP
# Projection defines units that "area" will be calculated in
quarter_shp@data$area <- sapply(slot(quarter_shp, "polygons"), slot, "area")



# JOIN DISEASE DATA -------------------------------------------------------
quarter_shp@data <- inner_join(quarter_shp@data, case_summary_combined,
                               by = c("quarter"))

# Calculate new variables
# quarter_shp@data$inc_rate <- quarter_shp@data$cases / quarter_shp@data$pop
# quarter_shp@data$cfr <- quarter_shp@data$deaths / quarter_shp@data$cases
quarter_shp@data$pop_density <- quarter_shp@data$pop / quarter_shp@data$area
quarter_shp@data$infect_density <- quarter_shp@data$area / quarter_shp@data$cases


# SHP OUTPUT ---------------------------------------------------------
# Transform to EPSG:3857
quarter_shp <- spTransform(quarter_shp, CRSobj = "+init=epsg:3857")
# proj4string(quarter_shp)

# Save if I need output for QGIS
writeOGR(obj = quarter_shp, dsn = "Data/GIS", layer = "CPH_Quarters_4",
          driver = "ESRI Shapefile", overwrite_layer = TRUE)


# CITY WALL & WATER POLYLINE ------------------------------------------------------
wall <- readOGR(dsn = "Data/GIS", layer = "city_wall", stringsAsFactors = F)
water <- readOGR(dsn = "Data/GIS", layer = "cph_water_area", stringsAsFactors = F)
pipes <- readOGR(dsn = "Data/GIS", layer = "Mads_Pipes", stringsAsFactors = F)
hosp <- readOGR(dsn = "Data/GIS", layer = "cph_hosp_poorhouses", stringsAsFactors = F)

# proj4string(hosp)
hosp <- spTransform(hosp, CRSobj = "+init=epsg:3857")
