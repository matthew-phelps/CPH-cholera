# Author: Matthew Phelps
# DESC: Simple analysis for 1853
# Output: Simple statistics on outbreak

rm(list = ls())
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)


# LOAD DATA ---------------------------------------------------------------

load("data\\Rdata\\cholera_by_street.Rdata")
load("data\\Rdata\\quarter_eng.Rdata")




# Simple numbers ----------------------------------------------------------

sick <- sum(quarter$sick.total.week)
dead <- sum(quarter$dead.total.week)
CFR <- dead/sick
