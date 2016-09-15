# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
rm(list=ls())
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")


amazon <- F

ifelse(amazon == T,
       data.path <- "~/Dropbox/AWS-Rstudio",
       data.path <- data.path)
setwd(data.path)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
# library(ggmcmc)
# library(ggplot2)
options(mc.cores = 4)
rm(amazon)

# LOAD -------------------------------------------------------
water_url <- RCurl::getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/water-matrix.csv")
border_url <- RCurl::getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/border-matrix.csv")


water_temp <- read.csv(text = water_url)
border <- read.csv(text = border_url)
load(file = "multi-model1-data-prep.Rdata")
water_temp[is.na(water_temp)] <- 0
water <- as.matrix(water_temp)
# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS

dataList <- list()
num_reps <- length(I_reps)
num_reps <- 1
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[5]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter,
                           water = water)
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
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)
jags_m6_ls <- list()
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m6_ls[[reps]] <- run.jags(model = 'JAGS-multi-quarter-6.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'gamma_b', "eta"),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 inits = inits_list,
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 3e3,
                                 sample = 3e3,
                                 thin = 2,
                                 plots = T)
}


add.summary(jags_m6_ls[[reps]])
m6_mcmc <- combine.mcmc(jags_m6_ls[[reps]], collapse.chains = F)
mcmcplot(m6_mcmc)

setwd(data.path)
save(jags_m6_ls, file = "jags_m6_ls.Rdata")

#################################################
#################################################
#################################################
#################################################

load(file = "jags_m6_ls.Rdata")
dic_m6 <- list()
for (i in 1:length(jags_m6_ls)){
  dic_m6[[i]] <- extract.runjags(jags_m6_ls[[i]], what = "dic")
}
save(dic_m6, file = "dic_m6.Rdata")
dic_m6
