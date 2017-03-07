# Author: Matthew Phelps
# Desc: Simulations from t = 0 and for t + 1 for multi-neighborhood model

rm(list = ls())
library(tidyverse)
require(grid)


# LOAD data ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
Nweeks <- Nsteps
source("multi-neighbor/sim-model-2-data-prep.R")
source("multi-neighbor/SimulationAndPlots.R")


# T + 1: SIMULATION -----------------------------------------------------
# "I_reps" is the daily "observed" incidence.
sim_m2 <- SimPlusOne(loops=200, 
                   I_reps = I_reps, N_it = N_it,
                   betas_95hpd = mcmc_out$betas_95hpd,
                   phi_95hpd = mcmc_out$phi_95hpd,
                   gamma_95hpd = mcmc_out$gamma_95hpd)

ci <- SimCI(sim_m2)

# Data reshape for plotting
sim_plot <- sim_m2 %>%
  SimDataToPlot() %>%
  SimPlot(., I_reps_plot, alpha_sim = 0.01, ci = ci)
sim_plot + ggtitle("model 2: one-step-ahead")
sim_plot

# SSAVe -------------------------------------------------------------------
ggsave(filename = 'Plot-output/Sim-m2-tplus1.jpg',
       plot = sim1_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 150)



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


# FULL SIMULATION ---------------------------------------------------------


sim_m2_0 <- SimFromZero(loops=2000, 
                    I_reps = I_reps, N_it = N_it,
                    betas_95hpd = mcmc_out$betas_95hpd,
                    phi_95hpd = mcmc_out$phi_95hpd,
                    gamma_95hpd = mcmc_out$gamma_95hpd)

# Generate 95% CI around simulation
ci <- SimCI(sim_m2_0)

# Plot simulation results 
sim2_plot <- sim_m2_0 %>%
  SimDataToPlot() %>%
  SimPlot(., I_reps_plot, color = "blue", alpha_sim = 0.01, ci = ci)
sim2_plot <- sim2_plot + ggtitle("model 2: Full Sim")
sim2_plot

ggsave(filename = 'Plot-output/Sim-m2-full.jpg',
       plot = sim2_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 150)


# Check how the timing of the epidemic between different simulations relates to noen another
sim2_timing <- SimAndData(10) %>%
  SimPlotReps(., I_reps_plot, alpha_sim = 1) + ggtitle ("model 1: Full Sim")

ggsave(filename = 'Plot-output/Sim-m1-full-timing.jpg',
       plot = sim2_timing,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 150)


