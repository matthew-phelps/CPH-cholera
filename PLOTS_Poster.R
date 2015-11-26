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
library(grid)
library(extrafont) 
library(scales)
loadfonts(device = "win")# Use fonts with tiff


# City-wide time-series ---------------------------------------------------

outbreak <-read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")
citywide <- ggplot(outbreak, aes(x = day.index))+
  geom_line(aes(y = cholera.cases, color = "Cases"), size = 1.5) +
  geom_line(aes( y = cholera.deaths, color = "Deaths"), size = 1.5) +
  xlab("Day index") +
  ylab("People") +
  ggtitle ("Daily cholera morbidity and mortality\n") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 28, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines')) +
  coord_cartesian(xlim = c(0, 102), ylim = c(-5,max(outbreak$cholera.cases)+5))


citywide
ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\citywide.tiff',
       plot = citywide,
       width = 24,
       height = 18,
       units = 'cm',
       dpi = 300)
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
  ggtitle ("How did the outbreak look in\ndifferent parts of the city?\n") +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold", vjust = 0.5),
        plot.title = element_text(size = 28, face="bold"),
        plot.margin = unit(c(0,0,0.5,0), 'lines'),
        strip.background = element_rect(color = '#F0F0F0', fill = '#F0F0F0')) +
  theme(panel.margin = unit(c(0.5,0.5,0.5,2), "lines"))


panal.incident.cases
ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\quarter_panel.tiff',
       plot = panal.incident.cases,
       width = 24,
       height = 22,
       units = 'cm',
       dpi = 300)

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
                   maptype = "toner-lite",
                   color = 'bw')

cph <- ggmap(cph_map, darken = c(0.0))

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
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), 'lines')
  ) +
  ggtitle("Cumulative infection \n at end of outbreak ") +
  theme(plot.title = element_text(size = 23, face="bold"),
        legend.title = element_text(size = 18),
        legend.position = 'bottom')
map

ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\map.png',
       plot = map,
       type = 'cairo-png',
       width = 40,
       height = 40,
       units = 'cm',
       dpi = 100)



# EXCESS MORTALITY --------------------------------------------------------
rm(list = ls())
load('Rdata\\all_age.Rdata')
all_age$Year <- as.character(all_age$Year)

mortality_plot <- ggplot(data = all_age,
                         aes(x = fake_date,
                             y = All,
                             group = Year,
                             color = Year)) +
  geom_line(size = 1.5) +
  geom_point(size = 5) +
  
  xlab("Month") +
  ylab("Mortality") +
  ggtitle ("Excess mortality in 1853\n") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9, 0.5),
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 22, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 22, face = "bold", vjust = 1.3),
        plot.title = element_text(size = 28, face="bold"),
        plot.margin = unit(c(0,0.5,0.5,0.3), 'lines')) +
  scale_x_date(breaks = '1 month', labels = date_format('%b')) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000))


mortality_plot

ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\mortality_plot.tiff',
       plot = mortality_plot,
       width = 22,
       height = 18,
       units = 'cm',
       dpi = 300)



# AGE ADJUSTED MORTALITY --------------------------------------------------
rm(list = ls())
load('Rdata\\age_mortality.Rdata')

# Re-order levels of factor so that plotting works: http://goo.gl/CD2fEC
age_char <- as.character(age_mortality$age)
age_mortality$age <- factor(age_char, levels = c(age_char))

age_mortality_plot <- ggplot() +
  geom_bar(data = age_mortality,
           stat = 'identity',
           aes(x = age, y = deaths, fill = 'red')) +
  xlab("Age group") +
  ylab("Mortality per 100 people") +
  ggtitle ("How did mortality vary with age? \n") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 18, angle = 45, vjust = 0.4),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 22,
                                    face = "bold",
                                    vjust = -0.1),
        axis.title.y = element_text(size = 22,
                                    face = "bold",
                                    vjust = 1.3),
        plot.title = element_text(size = 28, face="bold"),
        plot.margin = unit(c(0,0.5,0.5,0.3), 'lines'))
  
  age_mortality_plot

  
  ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\Conferences\\Epidemics 2015\\age_mortality_plot.tiff',
         plot = age_mortality_plot,
         width = 24,
         height = 18,
         units = 'cm',
         dpi = 300)