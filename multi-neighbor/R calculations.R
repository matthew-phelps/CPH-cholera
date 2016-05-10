# Author: Matthew Phelps 
# Desc: Calculate the R-internal and R-external for each neighborhood using
# beta values
# 
# 
# Intro -------------------------------------------------------------------
graphics.off()
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")
ifelse(grepl("wrz741", getwd()),
       save.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/Multi",
       save.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/Multi")

setwd(wd.path)


library(ggplot2)
library(tidyr)
require(grid)
library(coda)
library(CholeraDataDK)

# LOAD data ---------------------------------------------------------------

load(file = "sim-multi-1-data.Rdata") # update as more MCMCs are run
rm(jags_rep_4, I_it_daily, N_it, weekly_avg, phi, Nsteps)
duration <- 1/5


# R-INTERNAL --------------------------------------------------------------
x <- as.matrix(betas) # convert to matrix for diag() function to work
B_int <- diag(x) # extract diagonals
rm(x)

R_int <- data.frame(B_int / duration )



# R-EXTERNAL -------------------------------------------------------------- 
# Defined as: The estimated number of infectious cases casused in all other
# quarters by a single infecious case in the target quarter
x <- as.matrix(betas)
diag(x) <- NA
B_ext <- rowSums(x, na.rm = T)

R_ext <- data.frame(B_ext / duration)
