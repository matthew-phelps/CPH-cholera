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
library(reshape)



# Total outbreak ----------------------------------------------------------

outbreak <-read.table('CPH cholera outbreak 1853.csv', header=T, sep=",")

colSums((outbreak))
n0 <- nPlot(cholera.cases~day.index,type='lineChart',  data = outbreak)
n0$yAxis(axisLabel = "Number infected", width = 40)
n0$xAxis(axisLabel = "Day Index", width = 90)
n0$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
n0$set(title = "Citywide daily incidence")
n0$params$height = 500
n0


# Un-normalized incident cases per week -----------------------------------

load(file = "Rdata\\Data_3.Rdata")
m1 <- nPlot(sick.total.week~week.id, group ='quarter', type='lineChart',  data = combined)
m1$yAxis(axisLabel = "Number infected", width = 40)
m1$xAxis(axisLabel = "Day Index", width = 90)
m1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
m1$set(title = "Un-normalized weekly incidence")
m1$set(width = 1000, height = 600)
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
m2$yAxis(axisLabel = "Number infected per 100 people", width = 40)
m2$xAxis(axisLabel = "Day Index", width = 90)
m2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
m2$set(title = "Normalized weekly incidence")
m2$params$height = 500
m2

lattice