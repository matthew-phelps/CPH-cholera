# Author: Matthew Phelps
#Desc: Mapping cholera in CPH using ggmaps and ggplot 


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Cholera Denmark/CPH/Data"
pc <- 'C:\\Users\\wrz741\\Google Drev\\Copenhagen\\DK Cholera\\CPH\\Data\\'

setwd(pc)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_31') 
library (xlsx) # reading excel files
library(foreign)
library(sp)
library(reshape)
library(rgdal) # read shp files and do projections
library(maptools)
library(RColorBrewer)
library(ggmap)


quarter.sheet <- read.xlsx2(file = "quarter.xlsx",
                            sheetIndex = 1, colClasses = c("character", rep("numeric", 8)))
quarter.sheet <- rename(quarter.sheet, replace = c("sick.total.week" = "I"))

# reshape data
quarter.wide <- reshape(quarter.sheet, timevar = "week.id", idvar = c("quarter", "quarterID", "pop1855"), direction ="wide")
#rm(quarter.sheet)

# shapefile
quarter.shp <- readOGR(dsn = "GIS", layer = "CPH_Quarters", stringsAsFactors = F)
plot(quarter.shp)
quarter.shp@data$id <- as.numeric(quarter.shp@data$id)
quarter.shp@data

## Merging data to .shp file. This is not needed to R work, but used to write a
## .shp file that can be read into a GIS
#quarter.shp@data <- merge(quarter.shp@data, quarter.wide, by.x = "id", by.y = "quarterID")
#quarter.shp@data$Quarter <- NULL
#writeOGR(quarter.shp, dsn = "GIS", layer = "CHP_Quarters1", driver = "ESRI Shapefile", overwrite_layer = T)
#rm(quarter.wide)


# Get spatial data into a form that ggplot2 can handle
# mapdf is what ggplot will use
quarter.df <- as.data.frame(quarter.shp)
quarter.fortified <-fortify(quarter.shp, region = "id")
# quarter.lines <- join (quarter.fortified, quarter.df, by = "id")

mapdf <- merge(quarter.fortified, quarter.sheet, by.x = "id", by.y = "quarterID", all = T )
mapdf <- mapdf[ order(mapdf$order),]





#################
### Plotting ###----------------------------------------------------------------
################

# check exisiting projection & coordinate system
proj4string(quarter.shp)

# get background map
cph_map <- get_map(location = "Kongens Nytorv", zoom = 14, maptype = "toner-lite")
cph <- ggmap(cph_map, darken = c(.7))

# map showing the Total infections
cph +
     geom_polygon(data = mapdf[which(mapdf$week.id == 15),], aes(x = long, y = lat, group = id, 
                                    fill = (cum.sick/pop1855)*100),
                  color = "black") +
     #coord_equal(ratio = 0) +
     scale_fill_gradientn(colours = brewer.pal(9, "Reds")) +
          
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

# map matrix showing the No. infectious at weekly intervals---------------------

cph +
     geom_polygon(data = mapdf, aes(x = long, y = lat, group = id,  fill = (I/pop1855)*100),
                  color = "black") +
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
