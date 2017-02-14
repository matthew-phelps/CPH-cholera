# Author: Matthew Phelps
#Desc: Mapping cholera in CPH using ggmaps and ggplot 


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Cholera Denmark/CPH/Data"
pc <- 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data\\'

setwd(pc)
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_45') 
library(foreign)
library(sp)
library(reshape)
library(rgdal) # read shp files and do projections
library(maptools)
library(RColorBrewer)
library(ggmap)
library(dplyr)

load("Rdata\\quarter_eng.Rdata")
quarter.sheet <- reshape::rename(quarter, replace = c("sick.total.week" = "I"))
quarter.sheet$cum_sick_rate <- (quarter.sheet$cum.sick/ quarter.sheet$est.pop.1853)*1000

cum_sick_rate <- quarter.sheet[quarter.sheet$week.id==15, c("quarter", "cum_sick_rate")]
cum_sick_rate <- cum_sick_rate[complete.cases(cum_sick_rate), ]

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

# get background map
cph_map <- get_map(location = "Kongens Nytorv", zoom = 14, maptype = "toner-lite")
cph <- ggmap(cph_map, darken = c(.7))

# Normalized total infections
cph +
     geom_polygon(data = mapdf[which(mapdf$week.id == 15),], aes(x = long, y = lat, group = id, 
                                    fill = (cum.sick/est.pop.1853)*100),
                  color = "black") +
     #coord_equal(ratio = 0) +
     scale_fill_gradientn(name = "Cumulative Infections \nper 100 people", colours = brewer.pal(9, "Reds")) +
          
     theme(axis.title.x = element_blank(), # remove x,y, label
           axis.title.y = element_blank(),
           axis.line = element_blank(),
           axis.ticks = element_blank(), # remove tick marks
           axis.text = element_blank(), # no axis labels
           
           panel.grid.minor = element_blank(), # no gridlines
           panel.grid.major = element_blank(),
           panel.background = element_blank()
     ) +
     ggtitle("Cumulative infections per 100 people") +
  theme(plot.title = element_text(size = 25, face="bold"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))
       


# Un-normalized total infections
cph +
  geom_polygon(data = mapdf[which(mapdf$week.id == 15),], aes(x = long, y = lat, group = id, 
                                                              fill = (cum.sick)),
               color = "black") +
  #coord_equal(ratio = 0) +
  scale_fill_gradientn(name = "Cumulative Infections", colours = brewer.pal(9, "Reds")) +
  
  theme(axis.title.x = element_blank(), # remove x,y, label
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), # remove tick marks
        axis.text = element_blank(), # no axis labels
        
        panel.grid.minor = element_blank(), # no gridlines
        panel.grid.major = element_blank(),
        panel.background = element_blank()
  ) +
  ggtitle("Cumulative Infections ")


# map matrix showing the No. infectious at weekly intervals---------------------

cph +
     geom_polygon(data = mapdf, aes(x = long, y = lat, group = id,  fill = (I/pop1855)*100),
                  color = "black") +
     scale_fill_gradientn(name = "Incident Infections \nper 100ppl", colours = brewer.pal(9, "Reds")) +
     facet_wrap(~ week.id)+
     
     theme(axis.title.x = element_blank(), # remove x,y, label
           axis.title.y = element_blank(),
           axis.line = element_blank(),
           axis.ticks = element_blank(), # remove tick marks
           axis.text = element_blank(), # no axis labels
           
           panel.grid.minor = element_blank(), # no gridlines
           panel.grid.major = element_blank(),
           panel.background = element_blank()
     ) +
       ggtitle("Infectious per week per 100 ppl")



# map matrix showing the No. infectious at weekly intervals - no background map
ggplot()+
     coord_equal(ratio = 2)+
     geom_polygon(data = mapdf, aes(x = long, y = lat, group = id, fill = (I/pop1855)*100), color = "black") +
     scale_fill_gradientn(colours = brewer.pal(9, "Reds")) +
     facet_wrap(~ week.id)+
     
     theme(axis.title.x = element_blank(), # remove x,y, label
           axis.title.y = element_blank(),
           axis.line = element_blank(),
           axis.ticks = element_blank(), # remove tick marks
           axis.text = element_blank(), # no axis labels
           
           panel.grid.minor = element_blank(), # no gridlines
           panel.grid.major = element_blank(),
           panel.background = element_blank()
     ) +
     ggtitle("Infectious per week")



cph <-get_map(location = "Kongens Nytorv", maptype = "toner-lite", zoom = 14)
    
     
ggmap(cph, darken = .5)
