# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.

# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")

setwd(data.path)


load(file = "multi-model1-data-prep.Rdata")

x <- runif(Nquarter^2)
alphas <- matrix(x, nrow = Nquarter, ncol = Nquarter)
x <- I_reps[[1]]
beta <- 0.4