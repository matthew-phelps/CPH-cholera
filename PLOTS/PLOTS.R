# Author: Matthew Phelps
#Desc: Various plots of the cholera data
# output datasets: many plots

## intro
rm(list = ls())
graphics.off()
mac<- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"

setwd(mac)

library (ggplot2)
library (reshape) # for renaming variables
library(plyr)
library(rCharts)
library(dplyr)



# City-wide time-series ---------------------------------------------------

outbreak <-read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")
citywide <- ggplot(outbreak, aes(x = day.index))+
  geom_line(aes(y = cholera.cases, color = "cases"), size = 1) +
  geom_line(aes( y = cholera.deaths, color = "deaths"), size = 1) +
  geom_vline( xintercept = 40, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("People") +
  ggtitle ("Cholera cases and deaths, Copenhagen 1853") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.5),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 20, face="bold"))


citywide
ggsave(citywide,
       filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Fig 1 - citywide morbidity and mortality.pdf',
       width = 15,
       height = 9,
       units = 'in')

# City-wide aggregated to the week ----------------------------------------
load(file = "Rdata\\quarter_combined.Rdata")
Nsteps <- 16
quarterID <- as.numeric(combined$quarterID)
n <- as.numeric(length(quarterID))
Nquarter <- length(table(quarterID))
q_names <-as.data.frame(unique(combined$quarter))

S_it <- matrix(0, Nquarter, Nsteps)
I_it <- matrix(0, Nquarter, Nsteps)
N_i <- matrix(0, Nquarter, Nsteps)
for (i in 1:Nquarter){
  for( t in 1:Nsteps){
    S_it[i, t] <- (combined$S[which(combined$quarterID==i)])[t]
    I_it[i, t] <- (combined$sick.total.week[which(combined$quarterID==i)])[t]
    N_i[i, t] <- (combined$pop1855[which(combined$quarterID==i)])[t]
  }
}
row.names(I_it) <- q_names[, 1]

city_week <- as.matrix(colSums(I_it))
city_week <- as.data.frame(city_week)
city_week$week_index <- 1:Nsteps
city_week$day_index <- city_week$week_index * 7
city_week$week_index <- NULL



city_week_plot <- ggplot(data = city_week, aes(x = day_index, y = V1)) +
  geom_line(color = 'darkgreen', alpha = 0.5, size = 1.3) +
  geom_vline( xintercept = 40, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("People") +
  ggtitle('Copenhagen weekly incidence\n') +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 20, face="bold"))
  
city_week_plot

ggsave(city_week_plot,
       filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Fig X - Citywide Weekly Incidence.pdf',
       width = 15,
       height = 9,
       units = 'in')

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
load("Rdata/Quarter - normailzed incidence per week.Rdata")
normal.incident.cases <- ggplot (quarter.by.week, aes( x = startday.index, y = normal.incidence, group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("Incident cases per 1000 ppl") +
  xlim(0, 75) +
  ggtitle ("Normalized incident cases per week by quarter")

normal.incident.cases



# Quarter - Panal Normalized incidence per week ---------------------------------
load("Rdata/Quarter - normailzed incidence per week.Rdata")
panal.incident.cases <- ggplot (quarter.by.week, aes( x = startday.index, y = normal.incidence, group = quarter, color = quarter))+
  geom_line() +
  geom_vline( xintercept = 39, linetype = 2, alpha = 0.6, color = "black") +
  facet_wrap(~quarter) +
  xlab("Day index") +
  ylab("Incident cases per 1000 ppl") +
  xlim(0, 75) +
  ggtitle ("Normalized incident cases per week by quarter")

panal.incident.cases
