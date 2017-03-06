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
# library(ggmcmc)
# library(ggplot2)
options(mc.cores = 4)

# LOAD -------------------------------------------------------

load(file = "Data/Rdata/multi-model1-data-prep.Rdata")

# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m0_ls <- list()
dataList <- list()
num_reps <- length(I_reps)
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

# RUN JAGS -----------------------------------------------------------------
# JAGS
for (reps in 1:num_reps){
  print(reps)
  print(Sys.time())
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m0_ls[[reps]] <- run.jags(model = 'multi-neighbor/JAGS/JAGS-multi-quarter-0.stan',
                                 method = 'rjparallel',
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
save(jags_m0_ls, file = "Data/Rdata/jags_m0_ls-new-inits.Rdata")
