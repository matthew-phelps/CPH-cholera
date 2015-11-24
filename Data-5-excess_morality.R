# Author: Matthew Phelps
#Desc: Excess mortality and age stratification
# Dependicies: None



# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())

library(plyr)
library(zoo) # For interpolation functions
library(data.table)
library(ggplot2)
library(grid)


# LOAD DATA ---------------------------------------------------------------

excess_df <- read.csv2('Data\\Excess_mortality.csv', sep = ',')

all_age <- excess_df[excess_df$Place == 'Koebenhavn', c('Year', 'Month','All')]


# DATES -------------------------------------------------------------------

all_age$month <- paste(all_age$Year, all_age$Month, sep = "-")
all_age$month <- base::as.Date(all_age$month, format = "%Y-%m")
