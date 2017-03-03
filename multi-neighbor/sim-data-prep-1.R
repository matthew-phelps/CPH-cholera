
graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
source("Data-3-combine quarters.R")
source("multi-neighbor/sim-data-prep-functions.R")


# MODEL 1 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m1_ls-new.Rdata")
x <- mcmcPrep(jags_m1_ls, q_names, testing = FALSE)
rm(jags_m1_ls)
gc()
mcmc_out <- smMcmc(x)
rm(x)
gc()
save(mcmc_out, file = 'Data/Rdata/sim-model-1-data-1.Rdata' )

# MODEL 2 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m2_ls-new.Rdata")
x <- mcmcPrep(jags_m2_ls, q_names, testing = TRUE)
rm(jags_m2_ls)
x$int_hpd$int_hpd
gc()
mcmc_out <- smMcmc(x)
rm(x)
gc()
save(mcmc_out, file = 'Data/Rdata/sim-model-2-data-1.Rdata' )




# MODEL 5 ---------------------------------------------------------------

load(file = "Data/Rdata/jags_m5_ls-new.Rdata")
x <- mcmcPrep(jags_m5_ls, q_names, testing = FALSE)
rm(jags_m5_ls)
gc()
mcmc_out <- smMcmc(x)
rm(x)
gc()
save(mcmc_out, file = 'Data/Rdata/sim-model-5-data-1.Rdata' )
