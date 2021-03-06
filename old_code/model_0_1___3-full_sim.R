# Author: Matthew Phelps
#Desc: Full model from Initial time-step
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())

library(ggplot2)
library(reshape)
require(grid)

# LOAD data ---------------------------------------------------------------

load(file = 'data\\Rdata\\model_0_1_sim_data.Rdata')
load(file = 'data\\Rdata\\Data_3.Rdata')
set.seed(13)

I_it <- I_it[1,]
S_it <- S_it[1,]
N_i <- N_i[1, ]
#  Point Eestimate MODEL FROM INITIAL STATE ------------------------------------------------------------
I_it_est[2] <- 1/phi_pe
S_it_est[2] <- N_it[2]
loops <- 1000
I_est_pe_list <- list()
S_it_est_pe_list <- list()
for (z in 1:loops){
  Lambda_est_pe <- matrix(data = 0, nrow = 1, ncol = Nsteps)
  for (t in 2:(Nsteps-1)){
    Lambda_est_pe[t] <- S_it_est[t] / N_it[t] *    (beta_pe[1]*(I_it_est[t]))
    I_it_est[t+1] <- rpois(1, Lambda_est_pe[t])
    S_it_est[t+1] <- (S_it_est[t]) -    (I_it_est[t]) / (phi_pe[1])
  }
  I_est_pe_list[[z]] <- I_it_est
  S_it_est_pe_list[[z]] <- S_it_est
}

# SAVE for likelhood calculation
I_best_beta <- I_est_pe_list
save(I_best_beta, file = 'data\\Rdata\\I_best_beta.Rdata')


# PE RESHAPE DATA ---------------------------------------------------------

# Infectious Data for all quarters (city_pe level). Flatten each matrix
christ_full <- as.data.frame(matrix(data = 0, nrow = Nsteps, ncol = loops))
christ_full$week_index <- 1:Nsteps
christ_full$day_index <- christ_full$week_index * 7

for (z in 1:loops){
  christ_full[z] <- as.data.frame(colSums(I_est_pe_list[[z]]))
}
christ_full_melt <- melt(christ_full, id.vars = 'day_index')





# Prepare observed data aggregated to the week ----------------------------------------


chrit_obs <- as.data.frame(I_it)
chrit_obs$week_index <- 1:Nsteps
chrit_obs$day_index <- chrit_obs$week_index * 7
chrit_obs$week_index <- NULL


# # city_pe level Infectious
christ_full_sim_plot <- ggplot() +
  geom_line(data = christ_full_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.05) +
  geom_line (data = chrit_obs,
            aes(x = day_index, y = I_it),
            color = 'darkred', alpha = 0.5, size = 1.2) +
  theme_minimal()+
  ylab("People") +
  xlab("Day index") + 
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4))+
  ggtitle('Christianshavn infectious\n simulated n = 2000')
christ_full_sim_plot


ggsave(christ_full_sim_plot, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\christ_full_sim.tiff',
       width=15, height=9,
       units = 'in')




# STEP AHEAD SIMULATION ---------------------------------------------------




# Run simulations
loops <- 1000
I_plus1_list <- list() # List to contain all the loop outputs
rm(I_it_est, S_it_est, step1, N_it)

# Model for debugging. 1-loop only. Remove after debugging:
 I_est_tplus1 <- (matrix(data = 0, nrow = 1, ncol = Nsteps))
S_est_tplus1 <- (matrix(data = 0, nrow = 1, ncol = Nsteps))
Lambda_est_tplus1 <- (matrix(data = 0, nrow = 1, ncol = Nsteps))
S_est_tplus1[1] <- N_it[1] # S at t = 0 equals N

for (t in 1:(Nsteps-1)){
  Lambda_est_tplus1[t] <- S_est_tplus1[t] / N_it[t] * (beta_pe * I_it[t])
  I_est_tplus1[t] <- rpois(1, Lambda_est_tplus1[1, t])
  S_est_tplus1[t+1] <- S_it[t] - (I_it[t] / phi_pe)
}




for (z in 1:loops){
  # Initialize dataframes
  I_est_tplus1 <- as.data.frame(matrix(data = 0, nrow = 1, ncol = Nsteps))
  S_est_tplus1 <- as.data.frame(matrix(data = 0, nrow = 1, ncol = Nsteps))
  Lambda_est_tplus1 <- as.data.frame(matrix(data = 0, nrow = 1, ncol = Nsteps))
  S_est_tplus1[1] <- N_it[1] # S at t = 0 equals N

  # Model
  for (t in 1:(Nsteps-1)){
    Lambda_est_tplus1[t] <- S_est_tplus1[t] / N_it[t] * (beta_pe * I_it[t])
    I_est_tplus1[t+1] <- rpois(1, Lambda_est_tplus1[1, t])
    S_est_tplus1[t+1] <- S_it[t] - (I_it[t] / phi_pe)
  }
  
  
  I_plus1_list[[z]] <- I_est_tplus1
  row.names(I_plus1_list[[z]]) <- q_names[1, 1]
  
}

christ_tplus1 <- as.data.frame(matrix(data = 0, nrow = Nsteps, ncol = loops))
christ_tplus1$week_index <- 1:Nsteps
christ_tplus1$day_index <- christ_tplus1$week_index * 7

for (z in 1:loops){
  christ_tplus1[z] <- as.data.frame(colSums(I_plus1_list[[z]]))
}
christ_tplus1_melt <- melt(christ_tplus1, id.vars = 'day_index')


# Plot output
christ_tplus1_plot <- ggplot() +
  geom_line(data = christ_tplus1_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.05) +
  geom_line(data = chrit_obs,
            aes(x = day_index, y = I_it),
            color = 'darkred', alpha = 0.5, size = 1.3) +
  theme_minimal()+
  ylab("People") +
  xlab("Day index") + 
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4))+
  ggtitle('Christianshavn Infectious\n 1-step-ahead simulated n = 2000')
christ_tplus1_plot


ggsave(christ_tplus1_plot, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\christ_tplus1-I.tiff',
       width=15, height=9,
       units = 'in')



