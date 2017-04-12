# Author: Matthew Phelps
# Desc: Simulations from t = 0 and for t + 1 for multi-neighborhood model

rm(list = ls())
library(tidyverse)
require(grid)
library(plotly)

# LOAD data ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
Nweeks <- Nsteps
source("multi-neighbor/sim-model-5-data-prep.R")
source("functions/SimulationAndPlots.R")
source("functions/simNStepsAhead.R")
source("functions/listBetasToZero.R")

# GLOBAL VARIABLES ----------------------------------------------------------------
n_loops <- 1000

# FULL SIMULATION ---------------------------------------------------------




zeros_8_9 <- listBetasToZero(mcmc_out$betas_95hpd, c(9))


sim5_full <- SimFromZero(loops=n_loops, 
                         I_reps = I_reps, N_it = N_it,
                         betas_95hpd = zeros_8_9,
                         phi_95hpd = mcmc_out$phi_95hpd,
                         gamma_95hpd = mcmc_out$gamma_95hpd)

# Generate 95% CI around simulation

sim5_full_data <- SimDataToPlot(sim5_full)

sim5_full_summary <- sim5_full_data %>%
  rmNonOutbreaks(min_cases = 500) %>%
  SimCI()

SimPlot(observed_data = I_reps_plot,
        ci = sim5_full_summary$sim_summary, ribbon = TRUE)

# save(sim5_full_data, file =  "data/Rdata/sim5_full_data.Rdata")
# save(sim5_full_summary, file = "data/Rdata/sim5_full_summary.Rdata")




prob_vec <- wrapQuaterBetaToZero(mcmc_out, c(1, 5), n_loops = n_loops,
                     min_cases = 500)




x <- wrapQuaterBetaToZero(mcmc_out, c(5, 8, 9), n_loops = n_loops,
                                 min_cases = 500, group = TRUE)

# NStepsAhead -------------------------------------------------------------

n_ahead_5 <- nStepsAheahWrap(loops=n_loops, Nsteps = Nsteps,
                             I_reps = I_reps, N_it = N_it,
                             betas_95hpd = mcmc_out$betas_95hpd,
                             phi_95hpd = mcmc_out$phi_95hpd,
                             gamma_95hpd = mcmc_out$gamma_95hpd,
                             seed = 14)

sim5_n_ahead_data <- SimDataToPlot(n_ahead_5, nAhead = 7)

sim5_n_ahead_summary <- sim5_n_ahead_data %>%
  rmNonOutbreaks(min_cases = 10) %>%
  SimCI()

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

