# Author: Matthew Phelps
# DESC: Simple analysis for 1853
# Output: Simple statistics on outbreak

rm(list = ls())
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)


# LOAD DATA ---------------------------------------------------------------

load("data\\Rdata\\cholera_by_street.Rdata")
load("data\\Rdata\\quarter_eng.Rdata")
load('Data\\Rdata\\age1855.Rdata')
load('Data\\Rdata\\age1850.Rdata')
load('Data\\Rdata\\age_mortality.Rdata')

city <-read.table('data\\CPH cholera outbreak 1853.csv', header=T, sep=",")




# Simple numbers ----------------------------------------------------------

sick.city <- sum(city$cholera.cases)

sick <- sum(quarter$sick.total.week)
dead <- sum(quarter$dead.total.week)
CFR <- dead/sick


# Interpolate 1853 population--------------------------------------------------

# Calculate interpolated population
age_ts <- age1850$age_range
age_ts <- data.frame(age_ts)
age_ts$total1850 <- age1850$total
age_ts$total1853 <- NA 
age_ts$total1855 <- age1855$total
age_ts <- plyr::rename(age_ts, replace = c("age_ts" = "age"))

# Weighted average to estimate pop in 1853
age_ts$total1853 <- round((2 * age_ts$total1850 + 3 * age_ts$total1855) / 5, digits = 0) 



# AGE_ADJUSTED MORTALITY --------------------------------------------------

mortality_rates <- age_ts$age[1:22]
mortality_rates <- as.data.frame(mortality_rates)
mortality_rates$rate <- age_mortality$total_sick / age_ts$total1853[1:22]
mortality_rates <- plyr::rename(mortality_rates, replace = c('mortality_rates' = 'age'))

plot(mortality_rates)

save(mortality_rates, file = 'Data\\Rdata\\mortality_rates.Rdata')
