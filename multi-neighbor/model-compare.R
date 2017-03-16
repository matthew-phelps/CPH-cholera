rm(list = ls())
source("functions/trimSims.R")
source("functions/SimulationAndPlots.R")
load(file =  "data/Rdata/sim3_full_data.Rdata")
load(file =  "data/Rdata/sim5_full_data.Rdata")



sim3_trim <- trimSims(sim3_full_data, 35)
sim5_trim <- trimSims(sim5_full_data, 35)


sim3_summary <- SimCI(sim3_trim)
