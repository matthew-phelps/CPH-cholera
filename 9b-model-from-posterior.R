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


# MODEL (STOCHASTIC) -------------------------------------------------------------------

# For each model run, create 8x8 matrix so each neighborhood pair has beta estimate
# Value is drawn from one row of MCMC posterior. Sampleing with replacement
loops <- 3000
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



# MODEL (POINT ESTIMATES) -------------------------------------------------------------------

# Reshape parameters PE data
step1 <- as.matrix(beta_summary['Mean'])
beta_pe <- matrix(step1, nrow = Nquarter, ncol = Nquarter, byrow = F)

phi_pe <- as.matrix(phi_summary['Mean'])


# Estimate epidemic from initial state

loops <- 3000
I_est_pe_list <- list()
S_it_est_pe_list <- list()
for (z in 1:loops){
  Lambda_est_pe <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      Lambda_est_pe[i, t] <- S_it_est[i, t] / N_it[i, t] *   sum(beta_pe[i, ] * I_it_est[, t])
      I_it_est[i, t+1] <- rpois(1, Lambda_est_pe[i, t])
      S_it_est[i, t+1] <- (S_it_est[i, t]) - (I_it_est[i, t] / phi_pe)
    }
  }
  I_est_pe_list[[z]] <- I_it_est
  S_it_est_pe_list[[z]] <- S_it_est
}


# DATA RESHAPING for PLOTTING ----------------------------------------------------------

# Infectious Data for all quarters (city_stoch level). Flatten each matrix
city_stoch <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_stoch$week_index <- 1:Nsteps
for (z in 1:loops){
  city_stoch[z] <- as.data.frame(colSums(I_est_list[[z]]))
}
city_stoch_melt <- melt(city_stoch, id.vars = 'week_index')


# Infectious Data for all quarters (city_pe level). Flatten each matrix
city_pe <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_pe$week_index <- 1:Nsteps
for (z in 1:loops){
  city_pe[z] <- as.data.frame(colSums(I_est_pe_list[[z]]))
}
city_pe_melt <- melt(city_pe, id.vars = 'week_index')



# Infectious for single quarter
I_quarter <-(I_est_list[[1]][1, ])
for (z in 2:(loops)){
  I_quarter <- rbind(I_quarter, I_est_list[[z]][1, ])
}
I_quarter <- as.data.frame(t(I_quarter))
I_quarter$week_index <- 1:Nsteps
I_quarter_melt <- melt(I_quarter, id.vars = 'week_index')


# Susceptible city_stoch-wide
city_stoch_S <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_stoch_S$week_index <- 1:Nsteps
for (z in 1:loops){
  city_stoch_S[z] <- as.data.frame(colSums(S_it_est_list[[z]]))
}
city_stoch_S_melt <- melt(city_stoch_S, id.vars = 'week_index')



# PLOTS -------------------------------------------------------------------

# Quartery Infectious (Christianshavn)
plot1 <- ggplot(data = I_quarter_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkred', alpha = 0.05) +
  ggtitle('Christianshavn simulated\n n = 3000')
plot1
ggsave(plot1, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\Christanshavn.pdf',
       width=15, height=9,
       units = 'in')


# city_stoch level Infectious
plot2 <- ggplot(data = city_stoch_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkgreen', alpha = 0.05) +
  ggtitle('city_stoch level simulated\n n = 3000')
plot2
ggsave(plot2, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\city_stoch-I.pdf',
       width=15, height=9,
       units = 'in')


# city_stoch level Infectious
plot3 <- ggplot(data = city_stoch_S_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkblue', alpha = 0.05) +
  ggtitle('city_stoch level Susceptible\n n = 3000')
plot3
ggsave(plot3, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\city_stoch-S.pdf',
       width=15, height=9,
       units = 'in')


# city_pe level Infectious
plot4 <- ggplot(data = city_pe_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkgreen', alpha = 0.05) +
  ggtitle('city_pe level simulated\n n = 3000')
plot4
ggsave(plot4, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\city_pe-I.pdf',
       width=15, height=9,
       units = 'in')



# Plot last model run
# plot(S_it_est[1, ], type = 'l', col = 'darkred', lwd = 2)
plot(I_it_est[1, ], type = 'l', col = 'darkred', lwd = '2')


