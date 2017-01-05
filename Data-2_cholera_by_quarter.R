# Author: Matthew Phelps
# Desc: Prepare data - get cholera by quarter and combine quarters
# Output datasets: quarter.csv

## intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data")

setwd(wd.path)
rm(list = ls())
library(plyr)
library (MASS) # used for fiting distributions
library (ggplot2)
library (stats)
library (reshape) # for renaming variables
library(tidyverse)

load("Rdata/cholera_by_street.Rdata")





# Summarize each quarter -------------------------------------------------------
# summarize each quarter by day index (i.e. by week)
quarter_secondary <- street.data %>%
  group_by(quarter, quarter_secondary, startday.index, start.date,
           outside, hosp_poor) %>%
  dplyr::summarise(mensick.week = sum(male.sick, na.rm=T),
                   mendead.week = sum(male.dead, na.rm = T),
                   womensick.week = sum(female.sick, na.rm = T),
                   womendead.week = sum(female.dead, na.rm=T),
                   sick.total.week = sum(sick.total, na.rm = T),
                   dead.total.week = sum(dead.total, na.rm = T))

quarter <- street.data %>%
  group_by(quarter, startday.index, start.date,
           outside) %>%
  dplyr::summarise(mensick.week = sum(male.sick, na.rm=T),
                   mendead.week = sum(male.dead, na.rm = T),
                   womensick.week = sum(female.sick, na.rm = T),
                   womendead.week = sum(female.dead, na.rm=T),
                   sick.total.week = sum(sick.total, na.rm = T),
                   dead.total.week = sum(dead.total, na.rm = T)) %>%
  dplyr::arrange(quarter, start.date)


# CHECK SUMMATIONS --------------------------------------------------------
# Check to see if sums correspond to summations provided in original report
daily_cases <- quarter %>%
  group_by(startday.index) %>%
  dplyr::summarise(men = sum(mensick.week),
                   women = sum(womensick.week),
                   total = sum(sick.total.week),
                   week = min(start.date))
sum(daily_cases$total)

# Evaluates to TRUE if all streetlevel summations correct
all(daily_cases$men + daily_cases$women == daily_cases$total)

daily_cases_secondary <- quarter_secondary %>%
  group_by(startday.index) %>%
  dplyr::summarise(men = sum(mensick.week),
                   women = sum(womensick.week),
                   total = sum(sick.total.week),
                   week = min(start.date))
sum(daily_cases_secondary$total)

# TRUE if correct
all(daily_cases_secondary == daily_cases)

# # Find week of peak in each secondary quarter:
# peak.day <- ddply(quarter, .(quarter_secondary), summarize, peakday = startday.index[which.max(sick.total.week)] )
# 
# # Find week of first case in each qurter:
# start.day<- ddply (quarter, .(quarter_secondary), summarize, startday = startday.index[min(which(sick.total.week >0))])




# Normalize incidence by population ---------------------------------------
# Merge census and cholera data 
census <- read.csv ("Census - quarter.csv", sep=",", header=T, stringsAsFactors=F)
quarter <- merge(quarter, census, by.x="quarter", by.y="Quarter", all.x = T)

# Estimate 1853 population ------------------------------------------------
quarter$est.pop.1853 <- round(((3/5) * (quarter$pop1855 - quarter$pop1850) + quarter$pop1850), digits = 0)
quarter$pop1855 <- quarter$pop1850 <- NULL

quarter$normal.incidence <- quarter$sick.total.week / quarter$est.pop.1853 * 1000
quarter$normal.mortality <- quarter$dead.total.week / quarter$est.pop.1853 * 1000

quarter$normal.incidence <- round(quarter$normal.incidence, digits = 2)
quarter$normal.mortality <- round(quarter$normal.mortality, digits = 2)


rm(census, street.data)




# Add Cumulative No. infected at each week --------------------------------
quarter$week.id <- quarter$startday.index/7 # create time-step index
quarter$quarterID <- as.numeric(as.factor(quarter$quarter))
quarter$cum.sick <-NA


## Calculate the number of ppl in each compartment (S,I,R) at each time step:
# calculate cumilative number of infected in each quarter 

# Write function to pass to lapply
cumSick <- function(x){
  x$cum.sick <- cumsum(x$sick.total.week)
  x
}

quarter <- quarter %>%
  split(f = quarter$quarter) %>%
  lapply(cumSick) %>%
  bind_rows() %>%
  dplyr::arrange(quarter, start.date)

row.names(quarter) <- NULL


# now find S and R based on the N and "cumulative sick" numbers
quarter$S <- quarter$est.pop.1853 - quarter$cum.sick # no. of susceptibles at each timestep
quarter$R <- quarter$est.pop.1853 - (quarter$S + quarter$sick.total.week)



# SAVE --------------------------------------------------------------------
save(quarter, file = "Rdata/quarter_eng.Rdata") # not saving as CSV so as to discourage ppl corrupting data along the chain
save(quarter_secondary, file = "Rdata/quarter_eng_secondary.Rdata")
write.csv(quarter,
          file = "quarter_eng.csv",
          row.names = F)



# Prepare 1 grouping of of combined quarter NOT based on topography ----------------------------------------


# These are quarters to be combined
combined_lower <- ddply(quarter[which(quarter$quarter == "Vester" | 
                                        quarter$quarter ==  "Snarens"|
                                        quarter$quarter == "Strand" |
                                        quarter$quarter == "Frimands"),],
                        .(week.id), summarize,
                        sick.total.week = sum(sick.total.week),
                        dead.total.week = sum(dead.total.week),
                        est.pop.1853 = sum(est.pop.1853),
                        cum.sick = sum(cum.sick),
                        S = sum(S),
                        R = sum(R))
combined_upper <- ddply(quarter[which(quarter$quarter == "Noerre" | 
                                        quarter$quarter == "Klaedebo"),],
                        .(week.id), summarize,
                        sick.total.week = sum(sick.total.week),
                        dead.total.week = sum(dead.total.week),
                        est.pop.1853 = sum(est.pop.1853),
                        cum.sick = sum(cum.sick),
                        S = sum(S),
                        R = sum(R))

combined_lower$quarter <- "Combined_lower"
combined_lower$quarterID <- 99
combined_lower <- combined_lower[, c(8,1,2,3,4,9,5,6,7)]

combined_upper$quarter <- "Combined_upper"
combined_upper$quarterID <- 88
combined_upper <- combined_upper[, c(8,1,2,3,4,9,5,6,7)]




# Remove columns from original data from in order to bind with Cominbed df
temp_names <- colnames(combined_lower)
quarter <- quarter[(temp_names)]

combined <- rbind(combined_upper,
                  combined_lower,
                  quarter[which(quarter$quarter== "Nyboder" |
                                  quarter$quarter== "St. Annae Oester" |
                                  quarter$quarter== "St. Annae Vester" |
                                  quarter$quarter== "Kjoebmager" |
                                  quarter$quarter== "Rosenborg" |
                                  quarter$quarter== "Oester" |
                                  quarter$quarter== "Christianshavn"), ])

# renumber Quarter ID so it's sequential from 1:8
x1 <- with(combined, paste(quarterID))
combined <- within(combined, quarterID <- match(x1, unique(x1)))
rm(x1, combined_lower, temp_names)



# SAVE --------------------------------------------------------------------
save(combined, file = "Rdata/quarter_combined.Rdata")

