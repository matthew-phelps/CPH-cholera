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


p1 <- rPlot(sick.total.week~startday.index | quarter, color = 'quarter', data = quarter, type = 'point')
p1

# NORMALIZED weekly incidence ---------------------------------------------

load("Rdata\\Quarter - normailzed incidence per week.Rdata")
quarter.by.week$normal.incidence <- round(quarter.by.week$normal.incidence, digits = 1)
m2 <- nPlot(normal.incidence~startday.index, group ='quarter', type='lineChart',  data = quarter.by.week)
m2$yAxis(axisLabel = "Number infected", width = 40)
m2$xAxis(axisLabel = "Day Index", width = 90)
m2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
m2$set(title = "Normalized weekly incidence")
m2$setLib(lib="C:\\Users\\wrz741\\Google Drive\\Scripts\\rCharts\\inst\\libraries\\widgets\\nvd3")
m2$params$facet="quarter"
m2$templates$script = system.file(
  "C:\\Users\\wrz741\\Google Drive\\Scripts\\rCharts\\inst\\libraries\\nvd3\\layouts\\nvd3FacetPlot.html",
  package = "rCharts")
m2

lattice