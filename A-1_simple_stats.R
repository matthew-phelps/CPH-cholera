# Author: Matthew Phelps
# DESC: Simple analysis for 1853
# Output: Simple statistics on outbreak
## intro
rm(list = ls())  
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data")


setwd(wd.path)
library(plyr)
library(rgdal)
library(rgeos)
library(tidyverse)
library(dplyr)



# LOAD DATA ---------------------------------------------------------------

load(file = "Rdata/quarter_combined.Rdata")
load(file = "Rdata/quarter_eng.Rdata")
load(file = "Rdata/quarter_eng_secondary.Rdata")
qu_ls <- split(combined, f = combined$quarter)
q_names <- names(qu_ls)



# Simple numbers ----------------------------------------------------------

sick <- sum(combined$sick.total.week)
dead <- sum(combined$dead.total.week)
CFR <- dead/sick



# PEAK DATE ---------------------------------------------------------------
vec <- c("sick.total.week")
peak <- data.frame(inx = NA)
for(i in 1:length(qu_ls)){
  peak[i, ] <- which.max(qu_ls[[i]][, vec])
}







# CFR OVER TIME -------------------------------------------------------------
# CFR before / after peak
x <- dplyr::filter(combined, week.id <=7)
x2 <- dplyr::filter(combined, week.id >7)
cfr_early <- sum(x$dead.total.week) / sum(x$sick.total.week)
cfr_late <- sum(x2$dead.total.week) / sum(x2$sick.total.week)
rm(x, x2)


# CFR BY AREA -------------------------------------------------------------
cfr_area <- data.frame(quarter = q_names)
cfr_area$cfr <- sapply(qu_ls, function(x) sum(x$dead.total.week)/sum(x$sick.total.week))

mort_area <- data.frame(quarter = q_names)
mort_area$mort.rate <- sapply(qu_ls, function(x) sum(x$dead.total.week)/max(x$est.pop.1853))

morb_area <- data.frame(quarter = q_names)
morb_area$morb.rate <- sapply(qu_ls, function(x) sum(x$sick.total.week)/max(x$est.pop.1853))
sum(qu_ls$Christianshavn$sick.total.week)
max(qu_ls$Christianshavn$est.pop.1853)
sum(qu_ls$Combined_upper$sick.total.week) / max(qu_ls$Combined_upper$est.pop.1853)





# HOSPITALS / LOCATION SUMMARIES -----------------------------------------------

# overall CFR
sum(quarter$dead.total.week)/ sum(quarter$sick.total.week)


case_summary <- quarter %>%
  group_by(quarter) %>%
  dplyr::summarize(pop = max(est.pop.1853),
                   cases = sum(sick.total.week),
                   deaths = sum(dead.total.week),
                   outside = max(outside))
case_summary
sum(case_summary$cases)
total_reported_cases_everywhere <- sum(case_summary$cases)
case_summary$AR = case_summary$cases / case_summary$pop

# Cases outside the city walls:
in_out_cases <- case_summary %>%
  filter(quarter != "Fra skibe")%>%
  group_by(outside) %>%
  dplyr::summarize(cases = sum(cases),
                   deaths = sum(deaths))
in_out_cases
sum(in_out_cases$cases)
in_out_cases$deaths / in_out_cases$cases


ships <- case_summary %>%
  filter(quarter == "Fra skibe")%>%
  dplyr::summarize(cases = sum(cases),
                   deaths = sum(deaths))
ships


# Hopstial and poor-house cases
hosp_poor_cases <- quarter_secondary %>%
  group_by(hosp_poor) %>%
  dplyr::summarize(cases = sum(sick.total.week),
                   deaths = sum(dead.total.week),
                   outside = max(outside))
hosp_poor_cases
sum(hosp_poor_cases$cases) == sum(case_summary$cases) # TRUE is correct
hosp_poor_cases$deaths


# Percetanges
hosp_poor_cases$cases / total_reported_cases_everywhere
in_out_cases$cases / total_reported_cases_everywhere
ships$cases / total_reported_cases_everywhere



# Case summary on analysis subset of data - with combined quarters
case_summary_combined <- combined %>%
  group_by(quarter) %>%
  dplyr::summarize(pop = max(est.pop.1853),
                   cases = sum(sick.total.week),
                   deaths = sum(dead.total.week))


case_summary_combined$AR <- case_summary_combined$cases / case_summary_combined$pop * 100
case_summary_combined$CFR <- case_summary_combined$deaths / case_summary_combined$cases

case_summary_combined$AR <- round(case_summary_combined$AR, digits = 1)
case_summary_combined <- dplyr::arrange(case_summary_combined, quarter)

# Check that analysis subset includes all data
model_cases <- combined %>%
  group_by(quarter) %>%
  dplyr::summarise(cases = sum(sick.total.week),
                   deaths = sum(dead.total.week))

sum(model_cases$cases) == sum(combined$sick.total.week)


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

comb_summary$cum_incidence = comb_summary$cum.sick / comb_summary$est.pop.1853



# SAVE --------------------------------------------------------------------

save(case_summary_combined, file = "Rdata/case_summary_combined.Rdata")

# POPULATION DENSITY ------------------------------------------------------
# 
# # load("Rdata/quarter_eng.Rdata")
# # quarter.sheet <- reshape::rename(quarter, replace = c("sick.total.week" = "I"))
# 
# # shapefile
# quarter_shp <- readOGR(dsn = "GIS", layer = "CPH_Quarters3", stringsAsFactors = F)
# plot(quarter_shp)
# 
# # Not sure why, byt readOGR assigns incorrect projection (I think) to obj. Need
# # to overwrite assigned projection here, despite Warning message. This is NOT a
# # reprojection. This sort of explains the difference: https://goo.gl/jN066m
# proj4string(quarter_shp) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
# 
# # housekeeping
# colnames(quarter_shp@data)[2] <- "quarter"
# quarter_shp@data$id <- as.numeric(quarter_shp@data$id)
# quarter_shp@data$cumSick_rt <- quarter_shp@data$cum_cases <- NULL
# quarter_shp@data$area_1 <- NULL
# 
# 
# quarter_shp@data
# 
# 
# 
# # Find area of all quarters:
# #https://goo.gl/No33KP
# 
# # Projection defines units that "area" will be calculated in
# proj4string(quarter_shp)
# 
# quarter_shp@data$area <- sapply(slot(quarter_shp, "polygons"), slot, "area")
# 
# 
# # Join area data with disease data
# quarter_shp@data <- inner_join(quarter_shp@data, comb_summary,
#                                by = c("quarter"))
# 
# quarter_shp@data$pop_density <- quarter_shp@data$est.pop.1853 / quarter_shp@data$area
# quarter_shp@data$infect_density <- quarter_shp@data$area / quarter_shp@data$cum.sick
# 
# 
# plot(quarter_shp@data$cum_incidence ~ quarter_shp@data$pop_density)
# plot(quarter_shp)
# # SAVE SHP OUTPUT ---------------------------------------------------------
# 
# # Transform to EPSG:3857
# quarter_shp <- spTransform(quarter_shp, CRSobj = "+init=epsg:3857")
# proj4string(quarter_shp)
# 
# writeOGR(obj = quarter_shp, dsn = "GIS", layer = "CPH_Quarters_4",
#          driver = "ESRI Shapefile", overwrite_layer = TRUE)



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
