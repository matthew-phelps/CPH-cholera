# Author: Matthew Phelps
#Desc: Getting data into correct shape for JAGS run. 
#       Multiple relpicates of random allocation over days of
#       week
# Dependicies: Data 4


# Intro -------------------------------------------------------------------

graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")

setwd(wd.path)
rm(list = ls())
library(dplyr)
library(ggplot2)


load("Data_4.Rdata")



# SUBSET to CHRISTIANIA ---------------------------------------------------

I_chris <- I_multi_replicate[which(I_multi_replicate$quarter == "Christianshavn"), ]

# Selecton only columns with "rep" in name. See: http://goo.gl/s9xRKr
X_ch <- I_chris[, grepl("rep", names(I_chris))]
