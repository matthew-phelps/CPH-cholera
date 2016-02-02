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
all_age$Month <- formatC(all_age$Month, width = 2, format = 'd', flag = 0)
all_age$month <- paste(all_age$Year, all_age$Month, 01, sep = "-")
all_age$month <- base::as.Date(all_age$month, format = "%Y-%m-%d")
all_age$fake_date <- paste('1800', all_age$Month, 01, sep = '-')
all_age$fake_date <- base::as.Date(all_age$fake_date, format = "%Y-%m-%d")
#all_age$Month <- as.numeric(all_age$Month)



# Age mortality -----------------------------------------------------------
rm(list = ls())
age1855 <- read.csv('Data\\census1855_age.csv')
age1850 <- read.csv('Data\\census1850_age.csv')
age_mortality <- read.csv('Data\\age_morbidity_mortality.csv')



# SAVE --------------------------------------------------------------------

save(age_mortality, file = 'Data\\Rdata\\age_mortality.Rdata')
save(age1855, file = 'Data\\Rdata\\age1855.Rdata')
save(age1850, file = 'Data\\Rdata\\age1850.Rdata')

