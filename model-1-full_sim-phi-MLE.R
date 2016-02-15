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

loops <- 1000 # Has to be the same for both full sum and t+1 sim
duration <- 5 # In days. "1-2 weeks" from DOI:  10.1038/nrmicro2204
gamma <- 1/duration
phi_pe <- seq(from = 0.01, to = 0.025, length.out = 250)



# # FULL SIMULATION ---------------------------------------------------------
# # Initialize lists and matrices
# container_ls <- vector("list", length(phi_pe))
# R_i <- seq(from = 0, to = 0, length.out = length(I_it_daily))
# R_new <- matrix(data =  NA, nrow = 1, ncol = Nsteps)
# 
# Lambda_est_pe <- matrix(data = NA, nrow = 1, ncol = Nsteps)
# LambdaR <- matrix(data = NA, nrow = 1, ncol = Nsteps)
# 
# system.time(
#   for(phi_vect in 1:length(phi_pe)){
#     I_est_pe_list <- vector("list", loops)
#     S_it_est_pe_list <- vector("list", loops)
#     
#     for (z in 1:loops){
#       
#       for (t in 1:(Nsteps-1)){
#         Lambda_est_pe[t] <- S_it_est[t] / N_it[1] * (beta_pe[1] *(I_it_est[t]))
#         LambdaR[t] <- I_it_est[t] * gamma
#         R_new[t +1 ] <- rpois(1, LambdaR[t])
#         I_new <- rpois(1, (Lambda_est_pe[t] ) )
#         I_it_est[t + 1] <- max(0, (I_new + I_it_est[t] - R_new[t + 1]))
#         S_temp <- (S_it_est[t]) -    (I_new) / (phi_pe[phi_vect])
#         S_it_est[t + 1] <- max(0, S_temp)
#       }
#       
#       I_est_pe_list[[z]] <- I_it_est
#       S_it_est_pe_list[[z]] <- S_it_est
#     }
#     container_ls[[phi_vect]] <- I_est_pe_list
#   })
# container_ls[[1]][2]
# 
# # SAVE for likelhood calculation
# I_phi_vect <- container_ls
# save(I_phi_vect, file = 'data\\Rdata\\I_phi_vect.Rdata')
# save(phi_pe, file = 'data\\Rdata\\phi_vect.Rdata')


# STEP AHEAD SIMULATION ---------------------------------------------------
loops <- loops # See Intro to set loops - has to be same for t+1 & Full sim
duration <- 5 # In days. "1-2 weeks" from DOI:  10.1038/nrmicro2204
gamma <- 1/duration
phi_pe <- phi_pe
container_tplus1_ls <- vector("list", length(phi_pe))
R_i <- seq(from = 0, to = 0, length.out = length(I_it_daily))
R_new <- matrix(data =  NA, nrow = 1, ncol = Nsteps)

Lambda_est_pe <- matrix(data = NA, nrow = 1, ncol = Nsteps)
LambdaR <- matrix(data = NA, nrow = 1, ncol = Nsteps)

system.time(
  for(phi_vect in 1:length(phi_pe)){
  I_plus1_list <- vector("list", loops)
  S_plus1_list <- vector("list", loops)
  
  for (z in 1:loops){
    
    for (t in 1:(Nsteps-1)){
      Lambda_est_pe[t] <- S_plus1[t] / N_i_daily * (beta_pe[1] *(I_it_daily[t]))
      LambdaR[t] <- I_it_daily[t] * gamma
      R_new[t +1 ] <- rpois(1, LambdaR[t])
      I_new <- rpois(1, (Lambda_est_pe[t] ) )
      I_plus1[t + 1] <- max(0, (I_new + I_it_daily[t] - R_new[t + 1]))
      S_temp <- (S_plus1[t]) -    (I_new) / (phi_pe[phi_vect])
      S_plus1[t + 1] <- max(0, S_temp)
    }
    
    I_plus1_list[[z]] <- I_plus1
    S_plus1_list[[z]] <- S_plus1
  }
  container_tplus1_ls[[phi_vect]] <- I_plus1_list
})

# SAVE for likelhood calculation
I_phi_plus1_vect <- container_tplus1_ls
save(I_phi_plus1_vect, file = 'data\\Rdata\\I_phi_plus1_vect.Rdata')
save(phi_pe, file = 'data\\Rdata\\phi_vect.Rdata')



