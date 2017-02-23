# Author: Matthew Phelps
# Desc: Simulations from t = 0 and for t + 1 for multi-neighborhood model

graphics.off()
rm(list = ls())
library(tidyverse)
require(grid)


# LOAD data ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
Nweeks <- Nsteps
source("multi-neighbor/sim-model-1-data-prep-2.R")
source("multi-neighbor/simulation.R")


# GLOBAL VARIABLES -------------------------------------------------------------------
gamma <- 1/5

# T + 1: SIMULATION -----------------------------------------------------
# "I_reps" is the daily "observed" incidence.
sim1 <- simPlusOne(loops=300, gamma = gamma, I_reps = I_reps, N_it = N_it)

# T + 1 : DATA RESHAPE --------------------------------------------------------------
# Simulated daily incidence to long format:

I_simulated_plus1 <- sim1$I_new_plus1 %>%
  bind_rows() %>%
  `colnames<-` (c(q_names, "sim_num")) %>%
  mutate(day = 1:112) %>%
  gather(quarter, I_simulated, 1:9)
I_simulated_plus1[is.na(I_simulated_plus1)] <- 0


# T + 1: PLOT PANEL QUARTERS-------------------------------------------------------------
sim1_plus1 <- ggplot() + 
  geom_line(data = I_simulated_plus1, 
            alpha = 0.01,
            aes(x = day, y = I_simulated,
                group = interaction(quarter, sim_num),
                color = quarter)) +
  geom_line(data = I_reps_plot,
            alpha = 0.1,
            aes(x = day,
                y = I_new, 
                group = interaction(quarter, rep))) +
  facet_wrap(~quarter) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("model 1: 1-step-ahead")
sim1_plus1


# SSAVe -------------------------------------------------------------------
ggsave(filename = 'Plot-output/Sim-m1-tplus1.png',
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

