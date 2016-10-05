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
library(plyr)
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

load(file = "multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m5_ls_b <- list()
dataList <- list()
TestFlag <- T # T = use only 1 imputation for testing
ifelse(TestFlag, num_reps <- 1, num_reps <- length(I_reps))

for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)
load.runjagsmodule() # makes dmouch dist available to jags
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m5_ls_b[[reps]] <- run.jags(model = 'JAGS-multi-quarter-5-b.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 1e3,
                                 sample = 1e3,
                                 thin = 1,
                                 plots = T)
}


add.summary(jags_m5_ls_b[[reps]])
m5_mcmc <- combine.mcmc(jags_m5_ls_b[[reps]], collapse.chains = F)
mcmcplot(m5_mcmc)


setwd(data.path)
save(jags_m5_ls_b, file = "jags_m5_ls_b.Rdata")

#################################################
#################################################
#################################################
#################################################

load(file = "jags_m5_ls_b.Rdata")
dic_m5_b <- list()
for (i in 1:length(jags_m5_ls_b)){
  dic_m5_b[[i]] <- extract.runjags(jags_m5_ls_b[[i]], what = "dic")
}
save(dic_m5_b, file = "dic_m5_b.Rdata")
dic_m5_b
