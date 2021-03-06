# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/RCodes",
       model.path <-"/Users/Matthew/GitClones/RCodes")
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
jags_m2_ls <- list()
dataList <- list()
TestFlag <- T # T = use only 1 imputation for testing
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
  print(reps)
  jags_m2_ls[[reps]] <- run.jags(model = 'NB/nb-model-2.stan',
                                 method = 'rjags',
                                 monitor = c("beta", 'phi'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 n.chains = 1,
                                 adapt = 1,
                                 burnin = 4e2,
                                 sample = 4e2,
                                 thin = 1,
                                 plots = T)
}


#add.summary(jags_m2_ls[[reps]])
# m2_mcmc <- combine.mcmc(jags_m2_ls[[reps]], collapse.chains = F)
# mcmcplot(m2_mcmc)


setwd(data.path)
save(jags_m2_ls, file = "jags_m2_ls.Rdata")

#################################################
#################################################
#################################################
#################################################


# WAIC --------------------------------------------------------------------


load(file = "jags_m2_ls.Rdata")


ll <- jags.samples(as.jags(jags_m2_ls[[reps]]), c('lik', 'llsim'), type=c('mean','variance'), 10000)

mean_lik <- apply(ll$mean$lik,c(1,2),mean)

var_loglik <- apply(ll$variance$llsim, c(1,2),mean)
# Remove first row because we start at t + 1
mean_lik <- mean_lik[2:nrow(mean_lik), ]
var_loglik <- var_loglik[2:nrow(var_loglik), ]

waic_m2 <- get_waic(mean_lik, var_loglik)
waic_m2$waic
waic_m2$p_waic
save(waic_m2, file = "waic_m2.Rdata")



# DIC ---------------------------------------------------------------------
# 
# dic_m2 <- list()
# for (i in 1:length(jags_m2_ls)){
#   dic_m2[[i]] <- extract.runjags(jags_m2_ls[[i]], what = "dic")
# }
# save(dic_m2, file = "dic_m2.Rdata")
# dic_m2
