# Author: Matthew Phelps
# Desc: Simulations from t = 0 and for t + 1 for multi-neighborhood model

rm(list = ls())
library(tidyverse)
require(grid)
library(plotly)

# LOAD data ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
Nweeks <- Nsteps
source("multi-neighbor/sim-model-6-data-prep.R")
source("functions/SimulationAndPlots.R")


# GLOBAL VARIABLES ----------------------------------------------------------------
n_loops <- 1000

# T + 1: SIMULATION -----------------------------------------------------
# "I_reps" is the daily "observed" incidence.
sim6_step <- SimPlusOne(loops=n_loops, 
                        I_reps = I_reps, N_it = N_it,
                        betas_95hpd = mcmc_out$betas_95hpd,
                        phi_95hpd = mcmc_out$phi_95hpd,
                        gamma_95hpd = mcmc_out$gamma_95hpd)

sim6_step_summary <- SimCI(sim6_step)
sim6_step_data <- SimDataToPlot(sim6_step)

save(sim6_step_summary, file = "data/Rdata/sim6_step_summary.Rdata")
save(sim6_step_data, file = "data/Rdata/sim6_step_data.Rdata")



# FULL SIMULATION ---------------------------------------------------------

sim6_full <- SimFromZero(loops=n_loops, 
                         I_reps = I_reps, N_it = N_it,
                         betas_95hpd = mcmc_out$betas_95hpd,
                         phi_95hpd = mcmc_out$phi_95hpd,
                         gamma_95hpd = mcmc_out$gamma_95hpd)

# Generate 95% CI around simulation
sim6_full_summary <- SimCI(sim6_full)
sim6_full_data <- SimDataToPlot(sim6_full)

save(sim6_full_summary, file = "data/Rdata/sim6_full_summary.Rdata")
save(sim6_full_data, file =  "data/Rdata/sim6_full_data.Rdata")







# T = 0: Attributable cases -----------------------------------------------
# Element-wise mean of list of matrices. From : http://goo.gl/VA7S66
I_att_mean <- data.frame(Reduce("+", I_attr) / length(I_attr), row.names = q_names)
colnames(I_att_mean) <- q_names

I_proportion <- data.frame(matrix(data = NA, nrow = Nquarter, ncol = Nquarter), row.names = q_names)
colnames(I_proportion) <- q_names
for (i in 1:Nquarter){
  for (j in 1:Nquarter){
    I_proportion[j, i] <- I_att_mean[j, i] / quarter_sums[i, 2]
    
  }
}


save(I_att_mean, file = "Attributable-cases-t0.Rdata")
save(I_proportion, file = "Proportion-attributable-t0.Rdata")

