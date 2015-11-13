# Author: Matthew Phelps
#Desc: Full model playing around with Beta
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




# VECTOR OF BETAS ---------------------------------------------------------

beta_fake_vect <- matrix(data = 0, nrow = 20, ncol = 1)
beta_fake_vect <- seq(from = 1.5, to = 2.5, length.out = 20)

#  Point Eestimate MODEL FROM INITIAL STATE ------------------------------------------------------------
I_container_list <- list()

for(k in 1:length(beta_fake_vect)){
  
  I_it_est[2] <- 1/phi_pe
  S_it_est[2] <- N_it[2]
  loops <- 5000
  I_est_pe_list <- list()
  S_it_est_pe_list <- list()
  for (z in 1:loops){
    Lambda_est_pe <- matrix(data = 0, nrow = 1, ncol = Nsteps)
    for (t in 2:(Nsteps-1)){
      Lambda_est_pe[t] <- S_it_est[t] / N_it[t] *    (beta_fake_vect[k]*(I_it_est[t]))
      I_it_est[t+1] <- rpois(1, Lambda_est_pe[t])
      S_it_est[t+1] <- (S_it_est[t]) -    (I_it_est[t]) / (phi_pe[1])
    }
    I_est_pe_list[[z]] <- I_it_est
    S_it_est_pe_list[[z]] <- S_it_est
  }
  I_container_list[[k]] <- I_est_pe_list
}




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
