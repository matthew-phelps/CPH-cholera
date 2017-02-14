# Author: Matthew Phelps
# DESC: Prepare spatial data for plotting
rm(list = ls())
library(rgdal)
library(sp)
source("Data-3-combine quarters.R")
source("multi-neighbor/R calculations.R")

# QUARTER SUMMARY DATA FRAME ---------------------------------------------------------
# Date of first infected case in each quarter
start <- combined %>%
  group_by(quarter) %>%
  dplyr::summarize(start = 1 + week.id[min(which(sick.total.week >0))])
case_summary_combined <- left_join(case_summary_combined, start, by="quarter")


# QUARTER POLYGONS ------------------------------------------------------

# Load shapefile
oldw <- getOption("warn")
options(warn = -1)
quarter_shp <- readOGR(dsn = "Data/GIS", layer = "CPH_Quarters3", stringsAsFactors = F)
options(warn = oldw)

# Not sure why, byt readOGR assigns incorrect projection (I think) to obj. Need
# to overwrite assigned projection here, despite Warning message. This is NOT a
# reprojection. This sort of explains the difference: https://goo.gl/jN066m
# Supress warnings for this call only
oldw <- getOption("warn")
options(warn = -1)
proj4string(quarter_shp) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
options(warn = oldw)


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
quarter_shp@data <- quarter_shp@data %>%
  inner_join(case_summary_combined, by = "quarter") %>%
  mutate(pop_density = pop / area,
         infect_density = area/cases)

# Join reproductive number data
R_ext <- R_ext %>%
  dplyr::select(quarter, R_value) %>%
  dplyr::mutate(quarter = as.character(quarter)) %>%
  dplyr::rename(R_ext = R_value)
R_int <- R_int %>%
  dplyr::select(quarter, R_value) %>%
  dplyr::mutate(quarter = as.character(quarter)) %>%
  dplyr::rename(R_int = R_value)
quarter_shp@data <- quarter_shp@data %>%
  inner_join(R_ext, by = "quarter") %>%
  inner_join(R_int, by = "quarter")

# SHP OUTPUT ---------------------------------------------------------
# Transform to EPSG:3857
quarter_shp <- spTransform(quarter_shp, CRSobj = "+init=epsg:3857")

# Save if I need output for QGIS
# writeOGR(obj = quarter_shp, dsn = "Data/GIS", layer = "CPH_Quarters_4",
#           driver = "ESRI Shapefile", overwrite_layer = TRUE)


# CITY WALL & WATER POLYLINE ------------------------------------------------------
wall <- readOGR(dsn = "Data/GIS", layer = "city_wall", stringsAsFactors = F)
water <- readOGR(dsn = "Data/GIS", layer = "cph_water_area", stringsAsFactors = F)
pipes <- readOGR(dsn = "Data/GIS", layer = "Mads_Pipes", stringsAsFactors = F)
hosp <- readOGR(dsn = "Data/GIS", layer = "cph_hosp_poorhouses", stringsAsFactors = F)


# proj4string(hosp)
hosp <- spTransform(hosp, CRSobj = "+init=epsg:3857")
hosp_df <- as.data.frame(hosp@data)

oldw <- getOption("warn")
options(warn = -1)
hosp_tidy <- tidy(hosp)
options(warn = oldw)

# SPATIAL DATA -> GGPLOT DATA ---------------------------------------------
# Get spatial data into a form that ggplot2 can handle
quarter_df <- as.data.frame(quarter_shp)
quarter_tidy <-tidy(quarter_shp, region = "quarter")
mapdf <- left_join(quarter_tidy, quarter_df, by = c("id" = "quarter"))
mapdf <- mapdf[ order(mapdf$order),]

wall_fort <- tidy(wall, region = "id")
water_fort <- tidy(water, region = "grp")

pipes <- spTransform(pipes, CRSobj = "+init=epsg:3857")
pipes@data$id <- as.numeric(as.factor(pipes@data$Company))
pipes_df <- as.data.frame(pipes@data)
pipes_tidy <- tidy(pipes, region = id)



#  CLEAN ------------------------------------------------------------------
rm(oldw, start, list = rm_list)

