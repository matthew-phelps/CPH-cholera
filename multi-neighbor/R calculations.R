# Author: Matthew Phelps 
# Desc: Calculate the R-internal and R-external for each neighborhood using
# beta values
# 
# 
# Intro -------------------------------------------------------------------
library(tidyverse)
source("functions/CalculateRFun.R")
# LOAD data ---------------------------------------------------------------
load(file = "Data/Rdata/int_hpd.Rdata")
load(file = "Data/Rdata/param_ci.Rdata")

#load(file = "sim-model-5-data.Rdata") # update as more MCMCs are run
load(file = "Data/Rdata/sim-model-5-data.Rdata")
rm(N_it, weekly_avg, phi, Nsteps)


R_int <- Rint(betas = mcmc_out$betas_95hpd,
              lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
              q_names = q_names, order = TRUE)
R_ext <- Rext(betas = betas, lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = gamma,
              q_names = q_names, order = TRUE)

R <- rbind(R_int, R_ext)

