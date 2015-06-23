# Author: Matthew Phelps
#Desc: Interactive plots of data using rCharts
# output datasets: many plots
# 

## intro
rm(list = ls())
graphics.off()
mac<- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"

setwd(pc)
rm(pc)
library(rCharts)




# Un-normalized incident cases per week -----------------------------------

load("Rdata\\incident_cases_per_week.Rd")
m1 <- nPlot(sick.total.week~startday.index, group ='quarter', type='lineChart',  data = quarter)
m1$yAxis(axisLabel = "Number infected", width = 40)
m1$xAxis(axisLabel = "Day Index", width = 90)
m1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
m1$set(title = "Un-normalized weekly incidence")
m1

quarter$quarter <- as.factor(quarter$quarter)
quarter$sick_total_week <- as.numeric(quarter$sick.total.week) # polycharts cannot take names with "." in them
quarter$startday_index <- quarter$startday.index
p1 <- rPlot(sick_total_week~startday_index | quarter, color = 'quarter', 
            data = quarter, type = 'line')
p1


m1 <- mPlot(x = "startday_index", y = "sick_total_week", color = "quarter", type = 'line', data = quarter)
m1


# NORMALIZED weekly incidence ---------------------------------------------

load("Rdata\\Quarter - normailzed incidence per week.Rdata")
quarter.by.week$normal.incidence <- round(quarter.by.week$normal.incidence, digits = 1)
m2 <- nPlot(normal.incidence~startday.index, group ='quarter', type='lineChart',  data = quarter.by.week)
m2$yAxis(axisLabel = "Number infected", width = 40)
m2$xAxis(axisLabel = "Day Index", width = 90)
m2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
m2$set(title = "Normalized weekly incidence")
# m2$params$facet="quarter"
# m2$templates$script = system.file("/libraries/nvd3/layouts/nvd3FacetPlot.html",
#   package = "rCharts")
m2

lattice