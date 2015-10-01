# Author: Matthew Phelps
# Desc: Prepare data - get & clean data at street level
# Output datasets: Clean street-level data

## intro
rm(list = ls())
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"

setwd(pc)

library (reshape) # for renaming variables
#library (gdata) # reading excel files


# Read in data ------------------------------------------------------------


street.data <- read.csv ("Cholera by street CPH_eng.csv", sep=",")
head(street.data)

# convert to date format
street.data$start.date <- as.Date(street.data$start.date, "%d-%m-%Y")
street.data$end.date <- as.Date(street.data$end.date, "%d-%m-%Y")

# Remove the word "Quarter" from data for brevity's sake
street.data$quarter <- gsub("Qvarter", "", street.data$quarter)

# Recode 888 to missing data:
street.data$female.dead[street.data$female.dead==888] <- NA

# Remove un-used variables
street.data$street.index <- street.data$end.date <- NULL

# Count missing data
missing <- street.data[!complete.cases(street.data[, 4:7]), ]

# create data index
day0 <- as.Date("1853-06-12")

street.data$startday.index <-0
for (i in 1:nrow(street.data)){
    street.data$startday.index[i] <- street.data$start.date[i] - day0

}

save(street.data, file = "Rdata\\cholera_by_street.Rdata") # save as an R object so it doesn't get confused with the csv/xls files








