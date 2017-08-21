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



# Remove all quarters with R_int >1
prob_vec <- wrapQuaterBetaToZero(mcmc_out, c(1, 5, 8, 9), n_loops = n_loops,
                     min_cases = 500)

# Normal conditions with all quarters
prob_vec_normal <- wrapQuaterBetaToZero(mcmc_out, n_loops = n_loops,
                                 min_cases = 500)

# Remove quarters with low Betas
prob_vec_counter <- wrapQuaterBetaToZero(mcmc_out, c(2, 3, 4, 6, 5), n_loops = n_loops,
                                         min_cases = 500)

# Keep all quarters


x <- wrapQuaterBetaToZero(mcmc_out, c(5, 8, 9), n_loops = n_loops,
                                 min_cases = 500, group = TRUE)
