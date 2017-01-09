# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")
ifelse(grepl("wrz741", getwd()),
       fun.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/RCodes",
       fun.path <-"/Users/Matthew/GitClones/RCodes")

setwd(data.path)

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

load(file = "multi-model1-data-prep.Rdata")

setwd(fun.path)
source("WAIC-function.R")
setwd(data.path)

# SETUP JAGS-------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m4_ls <- list()
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

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)

for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m4_ls[[reps]] <- run.jags(model = 'JAGS-multi-quarter-2.stan',
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

setwd(data.path)
save(jags_m4_ls, file = "jags_m4_ls.Rdata")

# Get summary table
jags_summary <- data.frame(add.summary(jags_m4_ls[[reps]])$summaries)

# Check that no prsf is higher than our 1.02 cutoff value
max(jags_summary$psrf)
which.max(jags_summary$psrf)



#################################################
#################################################
#################################################
#################################################


# WAIC --------------------------------------------------------------------


load(file = "jags_m4_ls.Rdata")



waic_m4_ls <- list()
for(i in 1:reps){
  ll <- jags.samples(as.jags(jags_m4_ls[[reps]]), c('lik', 'llsim'), type=c('mean','variance'), 10000)
  
  mean_lik <- apply(ll$mean$lik,c(1,2),mean)
  
  var_loglik <- apply(ll$variance$llsim, c(1,2),mean)
  # Remove first row because we start at t + 1
  mean_lik <- mean_lik[2:nrow(mean_lik), ]
  var_loglik <- var_loglik[2:nrow(var_loglik), ]
  waic_m4_ls[[i]] <-  get_waic(mean_lik, var_loglik)
}

save(waic_m4_ls, file = "waic_m4_ls.Rdata")


# DIC ---------------------------------------------------------------------

dic_m4 <- list()

dic_m4 <- mclapply(jags_m4_ls, extract.runjags, "dic")

save(dic_m4, file = "dic_m4.Rdata")
dic_m4
