# Author: Matthew Phelps
#Desc: Getting data into correct shape for JAGS run. 
#       Multiple relpicates of random allocation over days of
#       week



# Intro -------------------------------------------------------------------

graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")

setwd(wd.path)
rm(list = ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(CholeraDataDK)


daily_temp <- cholera_daily_data

cases <- matrix(daily_temp[daily_temp$city == "aalborg", "cases"])
rm(daily_temp)


# DATA PREP FOR JAGS ------------------------------------------------------

# Calculate S
Nsteps <- length(cases)
N_pop <- aalborg_population # from: CholeraDataDK package
Nquarter <- 1 # allows us to use code from CPH in un-modified form
S_it_daily <- N_pop - cumsum(cases)




# Plot to check output
plot(cases, type = "l")



# SAVE --------------------------------------------------------------------

save(list = ls(), file = "Data-prep-jags-aal.Rdata")
