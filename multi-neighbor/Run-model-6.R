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
jags_m5_ls <- list()
dataList <- list()
num_reps <- length(I_reps)
num_reps <- 1
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter,
                           water = water)
}
inits_list <- list()
inits_list <- list(mu = 0,
                   tau = 0.01,
                   mu_2 = 0,
                   tau_2 = 0.01,
                   eta = 0.01,
                   logit_phi = 0,
                   log_beta = 0)

# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m5_ls[[reps]] <- run.jags(model = 'JAGS-multi-quarter-6.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'gamma_b', "eta"),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 inits = inits_list,
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 1e3,
                                 sample = 1e3,
                                 thin = 1,
                                 plots = T)
}


add.summary(jags_m5_ls[[reps]])
m5_mcmc <- combine.mcmc(jags_m5_ls[[reps]], collapse.chains = F)
mcmcplot(m5_mcmc)


setwd(data.path)
save(jags_m5_ls, file = "jags_m5_ls.Rdata")

#################################################
#################################################
#################################################
#################################################

load(file = "jags_m5_ls.Rdata")
dic_m5 <- list()
for (i in 1:length(jags_m5_ls)){
  dic_m5[[i]] <- extract.runjags(jags_m5_ls[[i]], what = "dic")
}
save(dic_m5, file = "dic_m5.Rdata")
dic_m5
