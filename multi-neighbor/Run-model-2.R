# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
graphics.off()
library(plyr)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
options(mc.cores = 4)

# LOAD -------------------------------------------------------
load(file = "Data/Rdata/multi-model1-data-prep.Rdata")

# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m2_ls <- list()
dataList <- list()
num_reps <- length(I_reps)
for (reps in 1:1){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

# RUN JAGS -----------------------------------------------------------------

# JAGS
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  print(reps)
  print(Sys.time())
  jags_m2_ls[[reps]] <- run.jags(model = 'JAGS/JAGS-multi-quarter-2.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'gamma_b'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 4e2,
                                 sample = 4e2,
                                 thin = 1,
                                 plots = T)
}


#add.summary(jags_m2_ls[[reps]])
# m2_mcmc <- combine.mcmc(jags_m2_ls[[reps]], collapse.chains = F)
# mcmcplot(m2_mcmc)
save(jags_m2_ls, file = "Data/Rdata/jags_m2_ls-new.Rdata")
