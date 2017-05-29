# Author: Matthew Phelps
# Desc: Prepare data - get & clean data at street level
# Output datasets: Clean street-level data to be used to create quarter
# level data in next step


library(tidyverse)



# Read in data ------------------------------------------------------------


street.data <- read.csv ("Data/Cholera by street CPH_eng.csv", sep=",")
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

street.data <- dplyr::arrange(street.data, quarter, street, start.date)

street.data$startday.index <-0
for (i in 1:nrow(street.data)){
  street.data$startday.index[i] <- street.data$start.date[i] - day0
  
}

# Trouble shoot NA dead
x <- which(is.na(street.data$female.dead))
# street.data[x, ]
# which(is.na(street.data$male.dead)) # no male NA dead

# Since there were no female sick recorded, change NA to 0
street.data$female.dead[x] <- 0

# Combine male-female -----------------------------------------------------

street.data$sick.total <- NA
street.data$dead.total <- NA

sickSum <- function(x) {
  for (i in 1:nrow(x)){
      x$sick.total[i] <- sum(x$male.sick[i], x$female.sick[i], na.rm = T)
      x$dead.total[i] <- sum(x$male.dead[i], x$female.dead[i], na.rm = T)
  }
  x
}

street.data <- street.data %>%
  split(f = street.data$street) %>%
  lapply(sickSum) %>%
  bind_rows() %>%
  dplyr::arrange(quarter, street, start.date)

rm(missing, day0, i, x, sickSum)
# save(street.data, file = "Data/Rdata/cholera_by_street.Rdata") # save as an R object so it doesn't get confused with the csv/xls files








