# Author: Matthew Phelps
#Desc: R0 calcualtion city-wide from daily counts


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)
rm(pc, mac)

data1 <- read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")



# CUMULATIVE CASES --------------------------------------------------------
data1 <- within(data1, cumCases <- cumsum(cholera.cases))

# plot data to look
plot(data1$cumCases)



# EXPONENTIAL FITTING -----------------------------------------------------

lim <- 0:50 # Specify the range of days to fit to.
r.fit <- glm.fit(data1$day.index[lim], data1$cumCases[lim], family = poisson())
r.fit$coefficients

# Method from Lipsitch et. al
latent <- 1.4 #Azman - The incubation of cholera: a systematic review
duration <- 3.1 #
serial.interval <- latent + duration
