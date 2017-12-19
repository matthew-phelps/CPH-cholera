rm(list=ls())
library(tidyverse)
library(coda)
library(runjags)
source("functions/sim-data-prep-functions.R")

source("Data-3-combine quarters.R")
load(file = "Data/Rdata/jags_m5_ls-new.Rdata")
x <- combine.mcmc(jags_m5_ls)
stats <- summary(x)

stats$statistics

# Calculate log medians and log SD for all transmission parameters
mcmc_summary <- mcmcSummary(jags_m5_ls, q_names = q_names, testing = FALSE)
save(mcmc_summary, file = "Data/Rdata/mcmc_summary_stats.Rdata")
mcmc_summary$phi_median
mcmc_summary$gamma_sd
