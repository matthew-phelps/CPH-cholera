# Author: Matthew Phelps
#Desc: Plots for output to poster presentation (Epidemics 2015)
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
library(foreign)
library(sp)
library(rgdal) # read shp files and do projections
library(maptools)
library(RColorBrewer)
library(ggmap)
library(dplyr)



# City-wide time-series ---------------------------------------------------

outbreak <-read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")
citywide <- ggplot(outbreak, aes(x = day.index))+
  geom_line(aes(y = cholera.cases, color = "Cases"), size = 1.5) +
  geom_line(aes( y = cholera.deaths, color = "Deaths"), size = 1.5) +
  xlab("Day index") +
  ylab("People") +
  ggtitle ("Cholera cases and deaths") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size = 19),
         axis.text.x = element_text(size = 16),
         axis.text.y = element_text(size = 16),
         axis.title.x = element_text(size = 18, face = "bold"),
         axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 24, face="bold")) +
  coord_cartesian(xlim = c(0, 102), ylim = c(-5,max(outbreak$cholera.cases)+5))


citywide
ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\citywide.wmf',
       plot = citywide,
       width = 20,
       height = 18,
       units = 'cm')
rm(outbreak)


# Quarter - Panal Normalized incidence per week ---------------------------------
load("Rdata\\quarter_eng.Rdata")

# Remove areas with no census data:
quarter.by.week <- quarter[!is.na(quarter$est.pop.1853), ]
rm(quarter)

panal.incident.cases <- ggplot (quarter.by.week, aes( x = startday.index, y = normal.incidence, group = quarter, color = quarter))+
  geom_line(size = 1) +
  geom_vline( xintercept = 40, linetype = 2, color = "black") +
  facet_wrap(~quarter) +
  xlab("Day index") +
  ylab("Incidence per 1000") +
  xlim(0, 75) +
  ggtitle (" Incident cases per 1000 people \n by week by quarter\n") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        plot.title = element_text(size = 20, face="bold"),
        strip.text.x = element_text(size = 11),
        strip.background = element_rect(color = '#F0F0F0', fill = '#F0F0F0')) 

panal.incident.cases
ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\quarter_panel.wmf',
       plot = panal.incident.cases,
       width = 20,
       height = 18,
       units = 'cm')

# MAP ---------------------------------------------------------------------



load("Rdata\\quarter_eng.Rdata")
quarter.sheet <- reshape::rename(quarter, replace = c("sick.total.week" = "I"))

# shapefile
quarter.shp <- readOGR(dsn = "GIS", layer = "CPH_Quarters2", stringsAsFactors = F)
plot(quarter.shp)
quarter.shp@data$id <- as.numeric(quarter.shp@data$id)
quarter.shp@data




# Get spatial data into a form that ggplot2 can handle
# mapdf is what ggplot will use
quarter.df <- as.data.frame(quarter.shp)
quarter.fortified <-fortify(quarter.shp, region = "Quarter")
# quarter.lines <- join (quarter.fortified, quarter.df, by = "id")

#quarter.fortified$id <- as.numeric(quarter.fortified$id)

mapdf <- left_join(quarter.fortified, quarter.sheet, by = c("id" = "quarter"))
mapdf <- mapdf[ order(mapdf$order),]



################
### Plotting ###----------------------------------------------------------------
################

# check exisiting projection & coordinate system
proj4string(quarter.shp)

# Get background map
cph_map <- get_map(location = "Kongens Nytorv", 
                   zoom = 14, 
                   maptype = "toner-lite")

cph <- ggmap(cph_map, darken = c(.7))

# Normalized total infections
map <- cph +
  geom_polygon(data = mapdf[which(mapdf$week.id == 15),], aes(x = long, y = lat, group = id, 
                                                              fill = (cum.sick/est.pop.1853)*100),
               color = "black") +
  #coord_equal(ratio = 0) +
  scale_fill_gradientn(name = "Cumulative infections \nper 100 people", colours = brewer.pal(9, "Reds")) +
  
  theme(axis.title.x = element_blank(), # remove x,y, label
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), # remove tick marks
        axis.text = element_blank(), # no axis labels
        
        panel.grid.minor = element_blank(), # no gridlines
        panel.grid.major = element_blank(),
        panel.background = element_blank()
  ) +
  ggtitle("Highest infection rate in \n Nyboder & Christianshavn \n ") +
  theme(plot.title = element_text(size = 23, face="bold"),
        legend.title = element_text(size = 18),
        legend.position = 'bottom')
map

ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\map.wmf',
       plot = map,
       width = 30,
       height = 26,
       units = 'cm')