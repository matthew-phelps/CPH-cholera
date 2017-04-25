
# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
source("functions/sim-data-prep-functions.R")
# library(ggplot2)
options(mc.cores = 4)

# LOAD -------------------------------------------------------

water_temp <- read.csv("Data/water-matrix.csv")
water_temp[is.na(water_temp)] <- 1
water <- water_temp %>%
  as.matrix() %>%
  matOrderFun()

border <- read.csv("Data/border-matrix.csv")
border[is.na(border)] <- 1
border <- border %>%
  as.matrix() %>%
  matOrderFun()

water_OR_border <- as.numeric(water | border) %>%
  matrix(nrow = 9)


load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS

dataList <- list()
num_reps <- length(I_reps)
num_reps <- 1
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

dataList[[1]]$I_incidence
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
model_num <- 1
# JAGS
model_path <- paste("JAGS-2/model-", model_num, ".stan", sep="")
fit_model <- list()
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  print(reps)
  print(Sys.time())
  fit_model[[reps]] <- autorun.jags(model = model_path, method = 'parallel',
                                    monitor = c('phi', 'gamma_b', 'foi'),
                                    modules = "glm", data = dataList[[reps]],
                                    inits = inits_list,
                                    n.chains = 4,
                                    adapt = 1e3,
                                    startburnin = 1e4,
                                    startsample = 4e3,
                                    thin = 2,
                                    plots = T)
}
add.summary(fit_model[[1]])

path_name <- paste("Data/Rdata/model_", model_num, "_jags.Rdata", sep="")
save(fit_model, file = paste("Data/Rdata/model_", model_num, "_jags.Rdata", sep=""))


# Model 2 -----------------------------------------------------------------
