# Author: Matthew Phelps
# Desc: Simulations from t = 0 and for t + 1 for multi-neighborhood model

graphics.off()
rm(list = ls())
library(tidyverse)
require(grid)


# LOAD data ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
Nweeks <- Nsteps
source("multi-neighbor/sim-model-5-data-prep-2.R")
source("multi-neighbor/SimulationAndPlots.R")


# GLOBAL VARIABLES -------------------------------------------------------------------

# T + 1: SIMULATION -----------------------------------------------------
# "I_reps" is the daily "observed" incidence.
sim1 <- SimPlusOne(loops=500, 
                   I_reps = I_reps, N_it = N_it,
                   betas_95hpd = mcmc_out$betas_95hpd,
                   phi_95hpd = mcmc_out$phi_95hpd,
                   gamma_95hpd = mcmc_out$gamma_95hpd)

# Data reshape for plotting
sim_data <- SimDataToPlot(sim1)
sim1_plot <- SimPlot(sim_data, I_reps_plot)
sim1_plot + ggtitle("model 1: 1-step-ahead")

# SSAVe -------------------------------------------------------------------
ggsave(filename = 'Plot-output/Sim-m1-tplus1-gamma.png',
       plot = sim1_plus1,
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

sim2 <- SimFromZero(loops=5, 
                    I_reps = I_reps, N_it = N_it,
                    betas_95hpd = mcmc_out$betas_95hpd,
                    phi_95hpd = mcmc_out$phi_95hpd,
                    gamma_95hpd = mcmc_out$gamma_95hpd)
x <- sim2$I_new_plus1
z <- sim2$I_new_plus1[[1]]


QuarterExtract <- function(x){
  browser()
  x[[1]]
}

sim2$I_new_plus1 %>%
  lapply(QuarterExtract) 


day1 <- 
sim2_plot <- sim2 %>%
  SimDataToPlot() %>%
  SimPlot(., I_reps_plot, alpha_sim = 0.01)
sim2_plot <- sim2_plot + ggtitle("model 1: Full Sim")

ggsave(filename = 'Plot-output/Sim-m5-full.jpg',
       plot = sim2_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 150)





OutbreaksOnly(10) %>%
  SimPlotReps(., I_reps_plot, alpha_sim = 1) + ggtitle ("model 5: Full Sim")



sim_tim<- SimDataToPlot(sim_tim)
SimPlot(sim_tim, I_reps_plot, alpha_sim = 1)

