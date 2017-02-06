# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta
rm(list = ls())

# Intro -------------------------------------------------------------------
graphics.off()
library(plyr)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
# library(ggmcmc)
# library(ggplot2)
options(mc.cores = 4)

# LOAD -------------------------------------------------------
load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
source("Functions/WAIC-function.R")

# SETUP JAGS-------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m3_ls <- list()
dataList <- list()
TestFlag <- F # T = use only 1 imputation for testing
ifelse(TestFlag, num_reps <- 1, num_reps <- length(I_reps))

for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

# RUN JAGS -----------------------------------------------------------------
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  print(reps)
  jags_m3_ls[[reps]] <- run.jags(model = 'multi-neighbor/JAGS-multi-quarter-3.stan',
                                 method = 'rjparallel',
                                 monitor = c("beta", 'phi'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 4e4,
                                 sample = 4e4,
                                 thin = 2,
                                 plots = T)
}


save(jags_m3_ls, file = "Data/Rdata/jags_m3_ls-new-inits.Rdata")

# Get summary table
jags_summary <- data.frame(add.summary(jags_m3_ls[[reps]])$summaries)

# Check that no prsf is higher than our 1.02 cutoff value
max(jags_summary$psrf)
which.max(jags_summary$psrf)



# WAIC --------------------------------------------------------------------
load(file = "jags_m3_ls.Rdata")
waic_m3_ls <- list()
for(i in 1:reps){
  print(reps)
  Sys.time()
  ll <- jags.samples(as.jags(jags_m3_ls[[reps]]), c('lik', 'llsim'), type=c('mean','variance'), 10000)
  
  mean_lik <- apply(ll$mean$lik,c(1,2),mean)
  
  var_loglik <- apply(ll$variance$llsim, c(1,2),mean)
  # Remove first row because we start at t + 1
  mean_lik <- mean_lik[2:nrow(mean_lik), ]
  var_loglik <- var_loglik[2:nrow(var_loglik), ]
  waic_m3_ls[[i]] <-  get_waic(mean_lik, var_loglik)
}

save(waic_m3_ls, file = "waic_m3_ls.Rdata")

# DIC ---------------------------------------------------------------------
dic_m3 <- list()
dic_m3 <- mclapply(jags_m3_ls, extract.runjags, "dic")
save(dic_m3, file = "dic_m3.Rdata")
dic_m3


