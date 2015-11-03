# Author: Matthew Phelps
# DESC: Simple analysis for 1853
# Output: Simple statistics on outbreak

rm(list = ls())
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)


# LOAD DATA ---------------------------------------------------------------

load("data\\Rdata\\cholera_by_street.Rdata")
load("data\\Rdata\\quarter_eng.Rdata")

city <-read.table('data\\CPH cholera outbreak 1853.csv', header=T, sep=",")




# Simple numbers ----------------------------------------------------------

sick.city <- sum(city$cholera.cases)

sick <- sum(quarter$sick.total.week)
dead <- sum(quarter$dead.total.week)
CFR <- dead/sick
