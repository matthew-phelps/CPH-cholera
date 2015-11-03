# Author: Matthew Phelps
#Desc: Various plots of the cholera data
# output datasets: many plots

## intro
rm(list = ls())
graphics.off()
mac<- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"

setwd(pc)

library (ggplot2)
library (reshape) # for renaming variables
library(plyr)
library(rCharts)




# City-wide time-series ---------------------------------------------------

outbreak <-read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")
citywide <- ggplot(outbreak, aes(x = day.index))+
  geom_line(aes(y = cholera.cases, color = "cases"), size = 1) +
  geom_line(aes( y = cholera.deaths, color = "deaths"), size = 1) +
  xlab("Day index") +
  ylab("People") +
  ggtitle ("Cholera cases and deaths, Copenhagen 1853") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 20, face="bold"))

  citywide

# Quarter - Incident cases per week ---------------------------------------

load("Rdata\\incident_cases_per_week.Rd")
incident.cases <- ggplot (quarter, aes( x = startday.index, y = sick.total.week, group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("Incident cases") +
  xlim(0, 75) +
  ggtitle ("Incident cases per week by quarter")

incident.cases

# Quarter - Normalized incidence per week ---------------------------------
load("Rdata\\Quarter - normailzed incidence per week.Rdata")
normal.incident.cases <- ggplot (quarter.by.week, aes( x = startday.index, y = normal.incidence, group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("Incident cases per 1000 ppl") +
  xlim(0, 75) +
  ggtitle ("Normalized incident cases per week by quarter")

normal.incident.cases



# Quarter - Panal Normalized incidence per week ---------------------------------
load("Rdata\\Quarter - normailzed incidence per week.Rdata")
panal.incident.cases <- ggplot (quarter.by.week, aes( x = startday.index, y = normal.incidence, group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black") +
  facet_wrap(~quarter) +
  xlab("Day index") +
  ylab("Incident cases per 1000 ppl") +
  xlim(0, 75) +
  ggtitle ("Normalized incident cases per week by quarter")

panal.incident.cases
