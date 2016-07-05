# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.

# Intro -------------------------------------------------------------------
rm(list = ls())
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")

setwd(data.path)

library(runjags)
library(rjags)

# LOAD --------------------------------------------------------------------

load(file = "dic_m1.Rdata")
load(file = "dic_m2.Rdata")
load(file = "dic_m3.Rdata")
load(file = "dic_m5.Rdata")

dic_m1
dic_m2
dic_m3
dic_m5
dic <- data.frame(pd_1 = c(3941, 3844, 3977, 4086, 3988, 3994,
                              4049, 3925, 4016, 3994),
                     pd_2 = c(3637, 3552, 3648, 3753, 3666, 3671,
                              3728, 3622, 3718, 3659),
                     pd_3 = c(3605, 3552, 3613, 3705, 3632, 3629,
                              3682, 3558, 3685, 3622),
                     pd_5 = c(3507, 3417, 3538, 3660, 3525, 3498,
                              3597, 3526, 3542, 3545))
colSums(dic)
colMeans(dic)
