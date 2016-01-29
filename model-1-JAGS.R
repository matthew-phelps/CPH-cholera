# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853. Using interpolated incidence
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, Data-3
# Intro -------------------------------------------------------------------
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())
library(plyr)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
library(ggmcmc)
options(mc.cores = (parallel::detectCores()-1 ))


# LOAD -------------------------------------------------------
rm(list = ls())
load(file = "data\\Rdata\\model-1-data-prep.Rdata")


# DATA SHAPE --------------------------------------------------------------
# Restrict to only one replicate
I_incidence <- data.frame(I_incidence[, 1])
I_prev <- matrix(data = NA, nrow = Nsteps, ncol = 1)
I_prev[1] <- 0
S_it_daily <- data.frame(S_it_daily[, 1])

# Save in list form to pass to JAGS
dataList <- list(N_i_daily = N_i_daily,
                 I_incidence=I_incidence,
                 Nsteps=Nsteps)


# JAGS 1 ------------------------------------------------------------------
jags <- jags.model('Rcodes\\stan-model_Fitting-one-replicate.stan',
                   data = dataList,
                   n.chains = 1,
                   n.adapt = 1000)