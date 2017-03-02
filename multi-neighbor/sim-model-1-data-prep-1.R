
graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
source("Data-3-combine quarters.R")
source("multi-neighbor/sim-data-prep-functions.R")
load(file = "Data/Rdata/jags_m1_ls-new-inits.Rdata")

# MCMC PREP ---------------------------------------------------------------

x <- mcmcPrep(jags_m1_ls, q_names, testing = TRUE)
# rm(jags_m1_ls)
gc()
mcmc_out <- smMcmc(x)
rm(x)
gc()
save(mcmc_out, file = 'Data/Rdata/sim-model-1-data-1.Rdata' )
