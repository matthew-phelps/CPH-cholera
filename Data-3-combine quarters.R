# Author: Matthew Phelps
# Desc: PCombine quarters 
# Output datasets: combined quarters data

## intro
library(plyr)
library(tidyverse)

source("Data-2_cholera_by_quarter.R")

# Prepare 1 grouping of of combined quarter NOT based on topography ----------------------------------------
week_date <- daily_cases$week 

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
combined <- dplyr::arrange(combined, quarter, week.id)
x1 <- with(combined, paste(quarterID))
combined <- within(combined, quarterID <- match(x1, unique(x1)))
rm(x1, combined_lower, temp_names)



# SUMMARIZE QUARTERS ------------------------------------------------------

# Case summary on analysis subset of data - with combined quarters
case_summary_combined <- combined %>%
  group_by(quarter) %>%
  dplyr::summarize(pop = max(est.pop.1853),
                   cases = sum(sick.total.week),
                   deaths = sum(dead.total.week))
case_summary_combined$AR <- case_summary_combined$cases / case_summary_combined$pop*100
case_summary_combined$CFR <- case_summary_combined$deaths / case_summary_combined$cases*100
case_summary_combined$AR <- round(case_summary_combined$AR, digits = 1)
case_summary_combined$CFR <- round(case_summary_combined$CFR, digits = 1)
q_names <- combined$quarter %>%
  unique() %>%
  data.frame() %>%
  `colnames<-` ("value")

q_names <- as.character(q_names$value)

rm(combined_upper, cumSick, daily_cases, daily_cases_secondary, quarter_secondary)

# rm(list = setdiff(ls(),c("combined", "case_summary_combined", "quarter",
                         # "week_date", "q_names")))
# SAVE --------------------------------------------------------------------
# save(combined, file = "Rdata/quarter_combined.Rdata")

