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
jags_m5_ls <- list()
dataList <- list()
num_reps <- length(I_reps)
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each

for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  print(reps)
  print(Sys.time())
  jags_m5_ls[[reps]] <- run.jags(model = 'multi-neighbor/JAGS/JAGS-multi-quarter-5.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'gamma_b'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 4e4,
                                 sample = 4e4,
                                 thin = 2,
                                 plots = T)
}

save(jags_m5_ls, file = "Data/Rdata/jags_m5_ls-new.Rdata")

# Get summary table
jags_summary <- data.frame(add.summary(jags_m5_ls[[reps]])$summaries)

# Check that no prsf is higher than our 1.02 cutoff value
max(jags_summary$psrf)
which.max(jags_summary$psrf)
