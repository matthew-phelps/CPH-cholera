# Compare orignal parameters with parameters fitted from simulations
rm(list=ls())
library(tidyverse)
library(ggmcmc)

 
load(file = 'Data/Rdata/sim-model-5recapture-data.Rdata' )
re_cap <- mcmc_out$int_hpd
re_cap$param <- row.names(re_cap)
re_cap$median <- mcmc_out$median_vals$mcmc_median

load(file = 'Data/Rdata/sim-model-5-data-1.Rdata' )
m5 <- mcmc_out$int_hpd
m5$param <- row.names(m5)
m5$median <- mcmc_out$median_vals$mcmc_median


(m5$median - re_cap$median) / m5$median

param_check <- data.frame(m5$upper < re_cap$lower | m5$lower > re_cap$upper)
param_check$names <- m5$param
param_check

# Number of parameter values not captured:
sum(param_check$m5.upper...re_cap.lower...m5.lower...re_cap.upper)
