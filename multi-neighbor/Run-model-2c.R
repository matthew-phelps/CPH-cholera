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
model_num <- "2c"
# LOAD -------------------------------------------------------

water_temp <- read.csv("Data/water-matrix.csv")
water_temp[is.na(water_temp)] <- 0
water <- as.matrix(water_temp)

border_temp <- read.csv("Data/border-matrix.csv")
border_temp[is.na(border_temp)] <- 0
border <- as.matrix(border_temp)

rm(water_temp, border_temp)

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
model_path <- paste("JAGS/JAGS-multi-quarter-", model_num, ".stan", sep="")
fit_model <- list()
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  print(reps)
  print(Sys.time())
  fit_model[[reps]] <- autorun.jags(model = 'JAGS/JAGS-multi-quarter-2c.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'gamma_b', 'eta', 'kappa'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 inits = inits_list,
                                 n.chains = 4,
                                 adapt = 1e3,
                                 startburnin =  2e4,
                                 startsample =  5e3,
                                 thin = 2,
                                 plots = T)
}
add.summary(fit_model[[1]])

data_path <- paste("data/Rdata/jags_m", model_num, ".Rdata", sep="")
save(fit_model, file = data_path)

