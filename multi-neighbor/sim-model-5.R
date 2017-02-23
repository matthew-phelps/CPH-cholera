# Author: Matthew Phelps
# Desc: Simulations from t = 0 and for t + 1 for multi-neighborhood model

graphics.off()
rm(list = ls())

library(tidyverse)
require(grid)
library(coda)
library(CholeraDataDK)
library(plotly)

# LOAD data ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
Nweeks <- Nsteps
source("multi-neighbor/sim-model-5-data-prep2.R")
source("multi-neighbor/simulation.R")

# # CITY LEVEL DATA ---------------------------------------------------------
# city_obs <- cholera_daily_data %>%
#   dplyr::filter(city == "copenhagen") %>%
#   dplyr::select(c(day_index, cases))
# 
# 
# city_obs_2 <- data.frame(t(I_it))
# colnames(city_obs_2) <- q_names
# city_obs_2$day_index <- seq(from = 7, to = (Nweeks )* 7, length.out = Nweeks)
# city_obs_2$I <- rowSums(city_obs_2[, 1:9]) / 7
# quarter_sums <- dplyr::select(combined, c(quarter, week.id, cum.sick))
# quarter_sums <- quarter_sums[quarter_sums$week.id == 15, c(1, 3)]
# 

# GLOBAL VARIABLES -------------------------------------------------------------------
gamma <- 1/5


# T + 1: SIMULATION -----------------------------------------------------
# "I_reps" is the daily "observed" incidence.


sim1 <- simPlusOne(loops=30, gamma = gamma, I_reps = I_reps, N_it = N_it)



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
  ggtitle("model 5: 1-step-ahead")
sim1_plus1


# SSAVe -------------------------------------------------------------------
ggsave(filename = 'Plot-output/Sim-m5-tplus1.png',
       plot = sim1_plus1,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 150)
# T + 1: Attributable cases -----------------------------------------------

#Avergae over all the simulations
# Element-wise mean of list of matrices. From : http://goo.gl/VA7S66
I <- Reduce("+", sim1$store_prev) / length(sim1$store_prev)
S <- Reduce("+", sim1$store_S) / length(sim1$store_S)

# Number of infectious quarter 1 contributes to all others at time t
quarter_att_cases <- function(I=I, S=S, quarter_numb){
  I_att <- data.frame(matrix(NA, nrow = Nsteps-1, ncol = Nquarter))
  for (t in 1:(Nsteps-1)){
    I_att[t, ] <- S[t, ] / t(N_it) * I[t, quarter_numb] * betas[quarter_numb, ] * phi
  }
  I_att
}

all_quart_att_cases <- function(I=I, S=S, Nquarter){
  I_att <- data.frame(matrix(NA, nrow = Nquarter, ncol = Nquarter))
  for(i in 1:Nquarter){
    I_att[i, ] <- colSums(quarter_att_cases(I = I, S = S, i))
  }
  round(I_att, digits = 1)
}

I_att <- all_quart_att_cases(I=I, S=S, Nquarter = Nquarter)
colnames(I_att) <- q_names
rownames(I_att) <- q_names
I_att

I_proportion <- data.frame(matrix(data = NA, nrow = Nquarter, ncol = Nquarter), row.names = q_names)
colnames(I_proportion) <- q_names
for (i in 1:Nquarter){
  for (j in 1:Nquarter){
    I_proportion[j, i] <- I_att[j, i] / quarter_sums$cum.sick[i]
  }
}
round(colSums(I_proportion), digits = 2)


# T + 1: Save -------------------------------------------------------
ggsave(filename = 'Plot-output/Sim-m5-tplus1.png',
       plot = sim1_plus1,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)
save(I_att, file = "Data/Rdata/Attributable-cases.Rdata")
save(I_proportion, file = "Data/Rdata/Proportion-attributable.Rdata")



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################



# 
# 
# # T = 0: PREP-----------------------------------------------------
# 
# Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
# LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
# R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
# I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
# I_prev_vect <-    matrix(nrow = Nsteps, ncol = Nquarter)
# S_it_est <-       matrix(nrow = Nsteps, ncol = Nquarter)
# 
# Lambda_quart <-   list(data.frame(matrix(nrow = Nsteps, ncol = Nquarter)))
# I_new_mat <- list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))
# # Attributable infections variables:
# Lambda_quart<-lapply(1:Nquarter,
#                      function(x) data.frame(matrix(NA,
#                                                    nrow=Nsteps,
#                                                    ncol=Nquarter)))
# I_attr <-list((matrix(data = NA, nrow = Nquarter, ncol = Nquarter)))
# # Starting values
# I_prev_vect[1, ] <- I_reps[1]
# I_prev_vect[1, c(5, 8, 9)] <- 1 # Init Nyboder with 2 cases
# S_it_est[1, ] <- N_it[, 1] # init all S
# 
# # T = 0: SIMULATION-----------------------------------------------------
# ptm <- proc.time()
# set.seed(13)
# for (z in 1:loops){
#   for (t in 1:(Nsteps-1)){
#     for(i in 1:Nquarter){
#       
#       Lambda_est_pe[t, i] <- S_it_est[t, i] / N_it[i] * sum( betas[, i] * I_prev_vect[t, ])
#       LambdaR[t, i] <- I_prev_vect[t, i] * gamma
#       R_temp <- LambdaR[t, i]
#       R_new[t, i] <- min(R_temp, I_prev_vect[t, i]) # no more recovereds than infected
#       I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] * phi ) )
#       I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_new[t, i] / phi - R_new[t, i]))
#       S_temp <- (S_it_est[t, i]) -    (I_new[t, i]) / (phi)
#       S_it_est[t + 1, i] <- max(0, S_temp)
#       Lambda_quart[[i]][t, ] <- ((S_it_est[t, i] / N_it[i]) * ( betas[, i] * I_prev_vect[t, ])) * phi
#     }
#   }
#   # For each quarter: store sum of infections attributed to each quarter over
#   # all time-steps
#   I_attr[[z]] <- sapply(Lambda_quart, colSums, na.rm=T)
#   # Store new infections at each timestep for each quarter for each simulation
#   I_new_mat[[z]] <- data.frame(I_new)
#   I_new_mat[[z]]$sim_num <- z
# }
# proc.time() - ptm
# 
# 
# 
# 
# 
# # T = 0 : DATA RESHAPE --------------------------------------------------------------
# 
# # Simulated daily for each quarter
# x <- do.call(rbind.data.frame, I_new_mat) # merge all sims to 1 df
# colnames(x) <- c(q_names, "sim_num")
# x$day <- 1:Nsteps
# I_simulated <- gather(x, quarter, I_simulated, 1:Nquarter) # wide to long
# 
# # Remove all outbreaks that didn't take hold:
# # Helper function to test if sim is valid
# test_outbreak <- function(x){
#   # test if quarter 5 has 0 infections on day 45
#   if(x$X5[45] == 0){F} else{T}
# }
# # Index where sims didn't catch using helper function
# null_idx <- sapply(I_new_mat, test_outbreak)
# x <- I_new_mat[null_idx]
# x2 <- do.call(rbind.data.frame, x) # merge all sims to 1 df
# colnames(x2) <- c(q_names, "sim_num")
# x2$day <- 1:Nsteps
# I_sim_filtered <- gather(x2, quarter, I_simulated, 1:Nquarter) # wide to long
# 
# 
# # Observed data
# I_obs_df <- data.frame(I_reps)
# colnames(I_obs_df) <- q_names
# I_obs_df$day <- 1:Nsteps
# I_obs_lng <- gather(I_obs_df, quarter, I_obs, 1:Nquarter)
# rm(x)
# 
# 
# # T = 0: PLOT PANEL QUARTERS-------------------------------------------------------------
# 
# # Remove outbreaks that don't take hold
# 
# 
# sim1_plot <- ggplot() +
#   geom_line(data = I_sim_filtered,
#             alpha = 0.1,
#             aes(x = day, y = I_simulated,
#                 group = interaction(quarter, sim_num),
#                 color = quarter)) +
#   geom_line(data = combined, aes(x = (week.id+1) * 7,
#                                  y = sick.total.week/7, group = quarter)) +
#   facet_wrap(~quarter) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   ggtitle("Simulation from t = 0")
# sim1_plot
# 
# 
# 
# # T = 0: Citywide ---------------------------------------------------------
# # 
# # city_sim <- data.frame(rowSums(x[, 1:9]))
# # colnames(city_sim) <- "I_simulated"
# # city_sim$day <- x$day
# # city_sim$sim_num <- x$sim_num
# # 
# # city1_plot <- ggplot() +
# #   geom_line(data = city_sim,
# #             aes(x = day, y = I_simulated, group = sim_num),
# #             color = "orange",
# #             alpha = 0.02) +
# #   geom_line(data = city_obs_2,
# #             aes(x = day_index, y = I)) +
# #   ylab("Daily incidence") +
# #   theme_minimal() +
# #   ggtitle("Simulated from t = 0 \naggregated to city level")
# # city1_plot
# 
# 
# # T = 0: Attributable cases -----------------------------------------------
# # Element-wise mean of list of matrices. From : http://goo.gl/VA7S66
# I_att_mean <- data.frame(Reduce("+", I_attr) / length(I_attr), row.names = q_names)
# colnames(I_att_mean) <- q_names
# 
# I_proportion <- data.frame(matrix(data = NA, nrow = Nquarter, ncol = Nquarter), row.names = q_names)
# colnames(I_proportion) <- q_names
# for (i in 1:Nquarter){
#   for (j in 1:Nquarter){
#     I_proportion[j, i] <- I_att_mean[j, i] / quarter_sums[i, 2]
#     
#   }
# }
# 
# 
# # T = 0: Save -------------------------------------------------------
# 
# setwd(save.plot.path)
# ggsave(filename = 'Sim-1-quarter.png',
#        plot = sim1_plot,
#        width = 26,
#        height = 16,
#        units = 'cm',
#        dpi = 300)
# 
# setwd(wd.path)
# save(I_att_mean, file = "Attributable-cases-t0.Rdata")
# save(I_proportion, file = "Proportion-attributable-t0.Rdata")
# 
# 


###############################################################################
###############################################################################
###############################################################################



