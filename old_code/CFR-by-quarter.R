# Author: Matthew Phelps 
# Desc: The CFR is each quarter. If there is a wide range of CFR this might be
# indicitive of differential reporting amongst the quarters
rm(list = ls())

library(dplyr)
library(CholeraDataDK)

load(file = "data/Rdata/quarter_combined.Rdata")


quart2 <- split(combined, f = combined$quarter)

# Get total sick and dead in each quarter and/or hospital
sick <- sapply(names(quart2), function(x) sum(quart2[[x]]$sick.total.week))
dead <- sapply(names(quart2), function(x) sum (quart2[[x]]$dead.total.week))

rm(quart2)
names(combined)
cfr <- data.frame(unique(combined[, 1]))
colnames(cfr) <- "quarter"
cfr$cfr <- dead/sick
