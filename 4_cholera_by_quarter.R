# Author: Matthew Phelps
#Desc: Rshape data for GLM model
# output datasets: glm cholera data.csv

## intro
rm(list = ls())
mac<- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"

setwd(pc)

library (MASS) # used for fiting distributions
library (ggplot2)
library (stats)
library (reshape) # for renaming variables
library(plyr)

load("cholera_by_street.Rdata")


# Summarize each quarter -------------------------------------------------------
# summarize each quarter by day index (i.e. by week)
quarter <- ddply( street.data, .(quarter, startday.index), summarize, mensick.week = sum(male.sick), mendead.week = sum(male.dead), womensick.week = sum(female.sick), womendead.week = sum(female.dead, na.rm=T))

# combine male and female counts
for (i in 1:nrow(quarter)){
  quarter$sick.total.week[i] <- quarter$mensick.week[i] + quarter$womensick.week[i]
  quarter$dead.total.week[i] <- quarter$mendead.week[i] + quarter$womendead.week[i]
}


# Plot each quarter -------------------------------------------------------

# fem.sick <- ggplot (quarter, aes( x = startday.index, y = womensick.week, group = quarter, color = quarter))+
#     geom_line()+
#     geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black")
# 
# men.sick <- ggplot (quarter, aes( x = startday.index, y = mensick.week, group = quarter, color = quarter))+
#     geom_line()+
#     geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black")




incident.cases <- ggplot (quarter, aes( x = startday.index, y = sick.total.week, group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("Incident cases") +
  xlim(0, 75) +
  ggtitle ("Incident cases per week by quarter")

incident.cases






# Find week of peak in each quarter:
peak.day <- ddply(quarter, .(quarter), summarize, peakday = startday.index[which.max(sick.total.week)] )

# Find week of first case in each qurter:
start.day<- ddply (quarter, .(quarter), summarize, startday = startday.index[min(which(sick.total.week >0))])








# Normalize incidence by population ---------------------------------------
census <- read.csv ("data/Census - quarter.csv", sep=";", header=T, stringsAsFactors=F)

#merge census and cholera data 
quarter.by.week <- merge(quarter, census, by.x="quarter", by.y="Quarter")

for (i in 1:nrow(quarter.by.week)){
  quarter.by.week$normal.incidence[i] <- quarter.by.week$sick.total.week[i] / quarter.by.week$pop1855[i] * 1000
  quarter.by.week$normal.mortality[i] <- quarter.by.week$dead.total.week[i] / quarter.by.week$pop1855[i] * 1000
}

write.csv (quarter.by.week, "Incident cases per week by quarter.csv")

normal.incident.cases <- ggplot (quarter.by.week, aes( x = startday.index, y = normal.incidence, group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("Incident cases per 1000 ppl") +
  xlim(0, 75) +
  ggtitle ("Normalized incident cases per week by quarter")

normal.incident.cases

