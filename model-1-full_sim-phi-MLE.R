# Author: Matthew Phelps
#Desc: Full model from Initial time-step. Model 0_3
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
rm(list = ls())

library(ggplot2)
library(reshape)
require(grid)

# LOAD data ---------------------------------------------------------------

load(file = 'Data/Rdata/model-1-sim_data.Rdata')

set.seed(13)


#  Point Eestimate MODEL FROM INITIAL STATE ------------------------------------------------------------

duration <- 5 # In days. "1-2 weeks" from DOI:  10.1038/nrmicro2204
gamma <- 1/duration
phi_pe <- seq(from = 0.02, to = 0.028, length.out = 20)
loops <- 2000

# Initialize lists and matrices
container_ls <- vector("list", length(phi_pe))
R_i <- seq(from = 0, to = 0, length.out = length(I_it_daily))
R_new <- matrix(data =  NA, nrow = 1, ncol = Nsteps)

Lambda_est_pe <- matrix(data = NA, nrow = 1, ncol = Nsteps)
LambdaR <- matrix(data = NA, nrow = 1, ncol = Nsteps)

system.time(for(phi_vect in 1:length(phi_pe)){
  I_est_pe_list <- vector("list", loops)
  S_it_est_pe_list <- vector("list", loops)
  
  for (z in 1:loops){
    
    for (t in 1:(Nsteps-1)){
      Lambda_est_pe[t] <- S_it_est[t] / N_it[1] * (beta_pe[1] *(I_it_est[t]))
      LambdaR[t] <- I_it_est[t] * gamma
      R_new[t +1 ] <- rpois(1, LambdaR[t])
      I_new <- rpois(1, (Lambda_est_pe[t] ) )
      I_it_est[t + 1] <- max(0, (I_new + I_it_est[t] - R_new[t + 1]))
      S_temp <- (S_it_est[t]) -    (I_new) / (phi_pe[phi_vect])
      S_it_est[t + 1] <- max(0, S_temp)
    }
    
    I_est_pe_list[[z]] <- I_it_est
    S_it_est_pe_list[[z]] <- S_it_est
  }
  container_ls[[phi_vect]] <- I_est_pe_list
})
container_ls[[1]][2]


# SAVE for likelhood calculation
I_phi_vect <- container_ls
save(I_phi_vect, file = 'data\\Rdata\\I_phi_vect.Rdata')
save(phi_pe, file = 'data\\Rdata\\phi_vect.Rdata')


# PE RESHAPE DATA ---------------------------------------------------------

# Infectious Data for all quarters (city_pe level). Flatten each matrix
model_1_full <- as.data.frame(matrix(data = 0, nrow = Nsteps, ncol = loops))
model_1_full$day_index <- 1:Nsteps


for (z in 1:loops){
  model_1_full[z] <- as.data.frame(colSums(I_est_pe_list[[z]]))
}
model_1_full_melt <- melt(model_1_full, id.vars = 'day_index')





# Prepare observed data aggregated to the week ----------------------------------------


model_1_obs <- as.data.frame(I_it_daily)
model_1_obs$day_index <- 1:Nsteps


# # city_pe level Infectious
model_1_full_sim_plot <- ggplot() +
  geom_line(data = model_1_full_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.02) +
  geom_line (data = model_1_obs,
             aes(x = day_index, y = I_it_daily),
             color = 'darkred', alpha = 0.5, size = 1.2) +
  theme_minimal()+
  ylab("People") +
  xlab("Day index") + 
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4))+
  ggtitle('St. Annae Vester infectious\n simulated from t = 0; n = 2000')
model_1_full_sim_plot

system.time(
  ggsave(model_1_full_sim_plot, 
         file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\model-1-full-sim-fake-phi.pdf',
         width=15, height=9,
         units = 'in')
)



# STEP AHEAD SIMULATION ---------------------------------------------------

loops <- 2000
R_i <- seq(from = 0, to = 0, length.out = length(I_it_daily))
R_new <- matrix(data =  NA, nrow = 1, ncol = Nsteps)
I_plus1_list <- list()
S_plus1_list <- list()
for (z in 1:loops){
  
  Lambda_est_pe <- matrix(data = NA, nrow = 1, ncol = Nsteps)
  LambdaR <- matrix(data = NA, nrow = 1, ncol = Nsteps)
  
  for (t in 1:(Nsteps-1)){
    Lambda_est_pe[t] <- S_plus1[t] / N_i_daily * (beta_pe[1] *(I_it_daily[t]))
    LambdaR[t] <- I_it_daily[t] * gamma
    R_new[t +1 ] <- rpois(1, LambdaR[t])
    I_new <- rpois(1, (Lambda_est_pe[t] ) )
    I_plus1[t + 1] <- max(0, (I_new + I_it_daily[t] - R_new[t + 1]))
    S_temp <- (S_plus1[t]) -    (I_new) / (phi_pe[1])
    S_plus1[t + 1] <- max(0, S_temp)
  }
  
  I_plus1_list[[z]] <- I_plus1
  S_plus1_list[[z]] <- S_plus1
}


model_1_tplus1 <- as.data.frame(matrix(data = 0, nrow = Nsteps, ncol = loops))
model_1_tplus1$day_index <- 1:Nsteps

for (z in 1:loops){
  model_1_tplus1[z] <- as.data.frame(colSums(I_plus1_list[[z]]))
}
model_1_tplus1_melt <- melt(model_1_tplus1, id.vars = 'day_index')


# Plot output
model_1_tplus1_plot <- ggplot() +
  geom_line(data = model_1_tplus1_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.05) +
  geom_line(data = model_1_obs,
            aes(x = day_index, y = I_it_daily),
            color = 'darkred', alpha = 0.5, size = 1.3) +
  theme_minimal()+
  ylab("People") +
  xlab("Day index") + 
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4))+
  ggtitle('St. Annae Vester daily infected\n 1-step-ahead simulated n = 2000')
model_1_tplus1_plot

system.time(ggsave(model_1_tplus1_plot, 
                   file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\model_1_tplus1-I.pdf',
                   width=15, height=9,
                   units = 'in')
)

