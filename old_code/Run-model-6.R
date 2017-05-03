# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
rm(list=ls())
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
# library(ggmcmc)
# library(ggplot2)
options(mc.cores = 4)

# LOAD -------------------------------------------------------

water_temp <- read.csv("Data/water-matrix.csv")
water_temp[is.na(water_temp)] <- 0
water <- as.matrix(water_temp)
rm(water_temp)

load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS

dataList <- list()
num_reps <- length(I_reps)
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}


# Supply initial parameter values
inits1 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_eta = 0,
               logit_phi = 0,
               log_beta = 0)
inits2 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_eta = 1,
               logit_phi = 0,
               log_beta = -1)

inits3 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_eta = -1,
               logit_phi = 0,
               log_beta = 0)

inits4 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_eta = 1,
               logit_phi = 0,
               log_beta = 0)

inits_list <- list(inits1, inits2, inits3, inits4)
# Model 1 -----------------------------------------------------------------

# JAGS
jags_m6_ls <- list()
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  print(reps)
  print(Sys.time())
  jags_m6_ls[[reps]] <- run.jags(model = 'JAGS/JAGS-multi-quarter-6.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'gamma_b', 'eta'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 inits = inits_list,
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 4e4,
                                 sample = 3e4,
                                 thin = 2,
                                 plots = T)
}
save(jags_m6_ls, file = "Data/Rdata/jags_m6_ls-new.Rdata")
