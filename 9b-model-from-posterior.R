# Author: Matthew Phelps
#Desc: Prepare data for model simulation based on posterior
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())

library(ggplot2)
library(reshape)

# LOAD data ---------------------------------------------------------------

load(file = 'data\\Rdata\\model_sim_data.Rdata')
set.seed(123)


# MODEL -------------------------------------------------------------------

# For each model run, create 8x8 matrix so each neighborhood pair has beta estimate
# Value is drawn from one row of MCMC posterior. Sampleing with replacement
loops <- 30
I_est_list <- list()
S_it_est_list <- list()
for (z in 1:loops){
  
  # Use MCMC output from one MCMC draw
  step1 <- betas_matrix[sample(nrow(betas_matrix), 1), ]
  step2 <- do.call(rbind, step1)
  step3 <- matrix(step2, nrow = 8, ncol = 8, byrow = F)
  beta_sample_itr <- as.data.frame(step3)
  rm(step1, step2, step3)
  
  phi_sample <- phi_matrix[sample(nrow(phi_matrix), 1), ]
  
  
  # Estimate epidemic from initial state
  Lambda_est <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      Lambda_est[i, t] <- S_it_est[i, t] / N_it[i, t] *   sum(beta_sample_itr[i, ] * I_it_est[, t])
      I_it_est[i, t+1] <- rpois(1, Lambda_est[i, t])
      S_it_est[i, t+1] <- (S_it_est[i, t]) - (I_it_est[i, t] / phi_sample)
    }
  }
  I_est_list[[z]] <- I_it_est
  S_it_est_list[[z]] <- S_it_est
}


# DATA RESHAPING ----------------------------------------------------------

# Infectious Data for all quarters (city level). Flatten each matrix
city <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city$week_index <- 1:Nsteps
for (z in 1:loops){
city[z] <- as.data.frame(colSums(I_est_list[[z]]))
}
city_melt <- melt(city, id.vars = 'week_index')


# Infectious for single quarter
I_quarter <-(I_est_list[[1]][1, ])
for (z in 2:(loops)){
  I_quarter <- rbind(I_quarter, I_est_list[[z]][1, ])
}
I_quarter <- as.data.frame(t(I_quarter))
I_quarter$week_index <- 1:Nsteps
I_quarter_melt <- melt(I_quarter, id.vars = 'week_index')


# Susceptible city-wide
city_S <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_S$week_index <- 1:Nsteps
for (z in 1:loops){
  city_S[z] <- as.data.frame(colSums(S_it_est_list[[z]]))
}
city_S_melt <- melt(city_S, id.vars = 'week_index')



# PLOTS -------------------------------------------------------------------

# Quartery Infectious (Christianshavn)
ggplot(data = I_quarter_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkred', alpha = 0.05) +
  ggtitle('Christianshavn simulated\n n = 3000')


# City level Infectious
ggplot(data = city_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkgreen', alpha = 0.05) +
  ggtitle('City level simulated\n n = 3000')


# City level Infectious
ggplot(data = city_S_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkblue', alpha = 0.05) +
  ggtitle('City level Susceptible\n n = 3000')

# Plot last model run
# plot(S_it_est[1, ], type = 'l', col = 'darkred', lwd = 2)
plot(I_it_est[1, ], type = 'l', col = 'darkred', lwd = '2')


