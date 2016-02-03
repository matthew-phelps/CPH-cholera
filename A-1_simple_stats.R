# Author: Matthew Phelps
# DESC: Simple analysis for 1853
# Output: Simple statistics on outbreak
## intro
rm(list = ls())  
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data")

setwd(wd.path)
library(rgdal)
library(rgeos)


# LOAD DATA ---------------------------------------------------------------

load("Rdata\\cholera_by_street.Rdata")
load("Rdata\\quarter_eng.Rdata")
load('Rdata\\age1855.Rdata')
load('Rdata\\age1850.Rdata')
load('Rdata\\age_mortality.Rdata')

city <-read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")




# Simple numbers ----------------------------------------------------------

sick.city <- sum(city$cholera.cases)

sick <- sum(quarter$sick.total.week)
dead <- sum(quarter$dead.total.week)
CFR <- dead/sick


# Interpolate 1853 population--------------------------------------------------

# Calculate interpolated population
age_ts <- age1850$age_range
age_ts <- data.frame(age_ts)
age_ts$total1850 <- age1850$total
age_ts$total1853 <- NA 
age_ts$total1855 <- age1855$total
age_ts <- plyr::rename(age_ts, replace = c("age_ts" = "age"))

# Weighted average to estimate pop in 1853
age_ts$total1853 <- round((2 * age_ts$total1850 + 3 * age_ts$total1855) / 5, digits = 0) 



# AGE_ADJUSTED MORTALITY --------------------------------------------------

mortality_rates <- age_ts$age[1:22]
mortality_rates <- as.data.frame(mortality_rates)
mortality_rates$rate <- age_mortality$total_sick / age_ts$total1853[1:22]
mortality_rates <- plyr::rename(mortality_rates, replace = c('mortality_rates' = 'age'))

plot(mortality_rates)

save(mortality_rates, file = 'Rdata\\mortality_rates.Rdata')



# QUARTER SUMMARY DATA FRAME ---------------------------------------------------------
quarter_summary <- quarter[which(quarter$week.id == 15),
                           c("quarter", "est.pop.1853", "cum.sick", "S")]



# POPULATION DENSITY ------------------------------------------------------

load("Rdata\\quarter_eng.Rdata")
quarter.sheet <- reshape::rename(quarter, replace = c("sick.total.week" = "I"))

# shapefile
quarter.shp <- readOGR(dsn = "GIS", layer = "CPH_Quarters3", stringsAsFactors = F)
plot(quarter.shp)
quarter.shp@data$id <- as.numeric(quarter.shp@data$id)
quarter_area_temp <- quarter.shp@data
quarter_area_temp$area <- NULL

# Find area of all quarters:
#https://goo.gl/No33KP
areas <- sapply(slot(quarter.shp, "polygons"), slot, "area")
quarter_area <- cbind(quarter_area_temp, areas)
rm(areas, quarter_area_temp)

# Join area data with disease data
quarter_summary <- inner_join(quarter_summary, quarter_area,
                             by = c("quarter" = "Quarter"))
quarter_summary$pop_density <- quarter_summary$est.pop.1853 / quarter_summary$areas
quarter_summary$infect_density <- quarter_summary$areas / quarter_summary$cum.sick
quarter_summary$cum_incidence <- quarter_summary$cum.sick / quarter_summary$est.pop.1853

plot(quarter_summary$cum_incidence ~ quarter_summary$pop_density)
