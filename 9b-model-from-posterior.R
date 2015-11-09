# Author: Matthew Phelps
#Desc: Prepare data for model simulation based on posterior
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())



# LOAD data ---------------------------------------------------------------

load(file = 'data\\Rdata\\model_sim_data.Rdata')



# MODEL -------------------------------------------------------------------

# For each model run, create 8x8 matrix so each neighborhood pair has beta estimate
# Value is drawn from one row of MCMC posterior. Sampleing with replacement
set.seed(123)
step1 <- betas_matrix[sample(nrow(betas_matrix), 1), ]
step2 <- do.call(rbind, step1)
step3 <- matrix(step2, nrow = 8, ncol = 8, byrow = F)
beta_sample_itr <- as.data.frame(step3)
rm(step1, step2, step3)

phi_sample <- phi_matrix[sample(nrow(phi_matrix), 1), ]



Lambda_est <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)
for (t in 1:(Nsteps-1)){
  for (i in 1:Nquarter){
    Lambda_est[i, t] <- S_it_est[i, t] / N_it[i, t] *   sum(beta_sample_itr[i, ] * I_it_est[, t])
    I_it_est[i, t+1] <- rpois(1, Lambda_est[i, t])
    S_it_est[i, t+1] <- (S_it_est[i, t] * phi_sample ) - (I_it_est[i, t] / phi_sample)
  }
}
plot(S_it_est[1, ], type = 'l', col = 'darkred', lwd = 2)
