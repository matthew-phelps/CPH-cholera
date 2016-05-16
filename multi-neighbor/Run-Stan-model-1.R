# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.


# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")

# If using AWS use this path:
amazon <- F
ifelse(amazon == T,
       data.path <- "~/Dropbox/AWS-Rstudio",
       data.path <- data.path)
setwd(data.path)
rm(amazon)
library(coda)
library(parallel)
library(rstan)

# For execution on a local, multicore CPU with excess RAM we recommend calling
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# LOAD -------------------------------------------------------

load(file = "multi-model1-data-prep.Rdata")


# Stan Prep -------------------------------------------------------------
# Save in list form to pass to JAGS

dataList <- list()
num_reps <- length(I_reps)
rep_num <- 4
dataList<- list(N_i_daily = N_pop[, 2],
                I_incidence=I_reps[[rep_num]],
                Nsteps=Nsteps,
                Nquarter = Nquarter)

setwd(model.path)

stan_obj_1_rep_4 <- rstan::stan_model(file = "Stan-multi-quarter-1-OldToNew.stan")

samp_size <- 100
stan_samples_1_rep_4 <- rstan::sampling(
  object = stan_obj_1_rep_4,
  data = dataList,
  chains = 1,
  iter = samp_size,
  warmup = samp_size/2,
  refresh = samp_size / 100,
  seed = 1
)
 