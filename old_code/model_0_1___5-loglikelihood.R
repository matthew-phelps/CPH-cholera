# Author: Matthew Phelps
#Desc: Calculate the LL of the simulated data
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
load(file = 'data\\Rdata\\weekly_sim.Rdata')
load(file = 'data\\Rdata\\I_best_beta.Rdata')
load(file = 'data\\Rdata\\Data_3.Rdata')

rm(beta_pe, beta_summary_0_1, betas_matrix, phi_matrix, phi_pe, phi_summary_0_1,
   step1, dataList, i, lower_sample, upper_sample, t, n, I_it_est, S_it_est,
   N_i, n_iter, sample_size, quarterID)
I_it <- I_it[1, ]


# AVERGAGE likelihoods fake beta ---------------------------------------------------------
# Only run likelihood calculation until timestep 8 - otherwise we get -Inf 
I_it_8 <- I_it[1:8]
I_est_8 <- list()
for(z in 1:length(I_est_pe_list)){
  I_est_8[[z]] <- I_est_pe_list[[z]][1,1:8] 
}

# Sum log-likelihood across each timestep 
ll_t <- list()
ll_z <- 0
for (z in 1:length(I_est_8)){
  ll_t[[z]] <- dpois(I_it_8, I_est_8[[z]], log = T)
  ll_z[z] <-exp(sum(ll_t[[z]]))
}
model_ll_fake_beta <- sum(ll_z)




# AVERGAGE likelihoods PE beta
# LIKELIHOOD RATIO --------------------------------------------------------

I_it_8 <- I_it[1:8]
I_PE_beta_8 <- list()
for(z in 1:length(I_best_beta)){
  I_PE_beta_8[[z]] <- I_best_beta[[z]][1,1:8] 
}

# Sum log-likelihood across each timestep 
ll_t <- list()
ll_z <- 0
for (z in 1:length(I_PE_beta_8)){
  ll_t[[z]] <- dpois(I_it_8, I_PE_beta_8[[z]], log = T)
  ll_z[z] <-exp(sum(ll_t[[z]]))
}
model_ll_PE_beta <- sum(ll_z)



# TEST LIKELIHOODS --------------------------------------------------------

# If "TRUE" then our fake beta is better? Right?
model_ll_fake_beta > model_ll_PE_beta
