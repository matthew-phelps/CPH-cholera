require(grid)
library(coda)
library(plotrix)
source("functions/plot-functions.R")
load("Data/Rdata/r-values-model-1.Rdata")
load("Data/Rdata/r-values-model-5.Rdata")

rTable(R_model5$R_median, log = FALSE)

dev.copy(png,
         file = "Plot-output/R1-R median table.png",
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()
