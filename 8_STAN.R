# Author: Matthew Phelps
#Desc: STAN for cholera data
# output datasets: unkown at this point


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)
library(reshape)
library(ggplot2)
library(plyr)
# library(AER)
#library(glmnet)
library(stargazer) # for nice output for html etc

load("Rdata\\quarter_combined_glm.Rdata")
quarter.glm <- quarter.merged.glm
rm(quarter.merged.glm)
quarter.glm$R <- quarter.glm$cum.sick <- NULL
