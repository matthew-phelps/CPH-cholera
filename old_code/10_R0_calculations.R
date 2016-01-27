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
plot(data1$cholera.cases ~ data1$cumCases)


# EXPONENTIAL FITTING -----------------------------------------------------

lim <- 1:35 # Specify the range of days to fit to.
r.fit <- glm.fit(data1$cholera.cases[lim], data1$cumCases[lim],family = poisson())
r <- r.fit$coefficients
r.fit$residuals
# Method from Lipsitch et. al. DOI =  10.1126/science.1086616
latent <- 1.4 #doi = 10.1016/j.jinf.2012.11.013
duration <- 3.1 # doi = 10.1016/S0140-6736(64)92099-9
serial.interval <- latent + duration
f <- latent / serial.interval
R <- 1 + serial.interval * r + f * (1 - f) * (serial.interval * r)^2



# Loop through end day values ---------------------------------------------
# Based on methods from: doi:10.1111/j.1365-3156.2006.01560.x

r2 <- data.frame(1:90, 1:90)
R <- data.frame(1:90, 1:90)

for (i in 2:90){
  end <- 1:i
  r.fit2 <- glm.fit(data1$cholera.cases[end], data1$cumCases[end],family = poisson())
  r2[i,1] <- r.fit2$coefficients
  R[i,1] <- 1 + serial.interval * r2[i,1] + f * (1 - f) * (serial.interval * r2[i,1])^2
}
plot(R[,2],R[,1])
plot(r2[2:nrow(r2),2],r2[2:nrow(r2),1])
