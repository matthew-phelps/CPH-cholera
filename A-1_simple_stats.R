# Author: Matthew Phelps
# DESC: Simple analysis for 1853
# Output: Simple statistics on outbreak
## intro
rm(list = ls())  
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")


setwd(wd.path)
library(rgdal)
library(rgeos)


# LOAD DATA ---------------------------------------------------------------

load(file = "quarter_combined.Rdata")
qu_ls <- split(combined, f = combined$quarter)
q_names <- names(qu_ls)
# load("Rdata/cholera_by_street.Rdata")
# load("Rdata/quarter_eng.Rdata")
# load('Rdata/age1855.Rdata')
# load('Rdata/age1850.Rdata')
# load('Rdata/age_mortality.Rdata')
# 
# city <-read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")




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


# AGE_ADJUSTED MORTALITY -------------------------------------------------
 
mortality_rates <- age_ts$age[1:22]
mortality_rates <- as.data.frame(mortality_rates)
mortality_rates$rate <- age_mortality$total_sick / age_ts$total1853[1:22]
mortality_rates <- plyr::rename(mortality_rates, replace = c('mortality_rates' = 'age'))

plot(mortality_rates)

save(mortality_rates, file = 'Rdata/mortality_rates.Rdata')



# QUARTER SUMMARY DATA FRAME ---------------------------------------------------------
quarter_summary <- quarter[which(quarter$week.id == 15),
                           c("quarter", "est.pop.1853", "cum.sick", "S")]



# POPULATION DENSITY ------------------------------------------------------

load("Rdata/quarter_eng.Rdata")
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
