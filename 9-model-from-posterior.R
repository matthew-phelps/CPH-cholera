# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())




# LOAD & PREP DATA ---------------------------------------------------------------


load(file = "data\\Rdata\\quarter_combined.Rdata")
load(file = 'data\\Rdata\\beta_summary.Rdata')
load(file = 'data\\Rdata\\phi_sumphi_summary.Rdata')
load(file = 'data\\Rdata\\par_jags_2.Rdata')



# INITIALIZE EMPTY DF -----------------------------------------------------

Nsteps <- 16
quarterID <- as.numeric(combined$quarterID)
Nquarter <- length(table(quarterID))
q_names <-as.data.frame(unique(combined$quarter))


I_it <- matrix(NA, Nquarter, Nsteps-1)
I_i_t1 <- matrix(0, nrow = Nquarter, ncol = 1)
N <- matrix(NA, Nquarter, Nsteps-1)
S <- matrix(NA, Nquarter, Nsteps-1)
# Initial infectious state for all quarters
for (i in 1:Nquarter){
  I_i_t1[i, 1] <- (combined$sick.total.week[which(combined$quarterID==i)])[1]
  S_i_t1[i, 1] <- 
}

# Bind first time-step of infection data to block of NAs the size of the remaining
# timesteps. These NAs will be overwritten with simulated data 
I_it_est <- as.data.frame(cbind(I_i_t1, I_it))




# PREPARE MCMC DRAWS ------------------------------------------------------

# Remove 1st 5000K iterations for burn in from each chain
n_iter <- length(par.jags.2$mcmc[[1]][, 1])
n_param <- as.numeric(length(par.jags.2$mcmc[[1]][1, ]))
chain1 <- as.data.frame(par.jags.2$mcmc[[1]][5000:n_iter, ])
chain2 <- as.data.frame(par.jags.2$mcmc[[2]][5000:n_iter, ])
chain3 <- as.data.frame(par.jags.2$mcmc[[3]][5000:n_iter, ])

betas_matrix <- rbind(chain1[, 1:n_param-1], chain2[, 1:n_param-1], chain3[, 1:n_param-1])

# Drop = F is because the "[]" operatore on DF changes a single column into a row vector
# Drop = F stops this from happening
phi_matrix <- rbind(chain1[, 'phi', drop = FALSE], chain2[, 'phi', drop = FALSE], chain3[, 'phi', drop = FALSE])

rm(chain1, chain2, chain3)



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


# # Calculate Lambda for every neighborhood-Timestep pair

# for (i in 1:Nquarter){
#   for (t in 1:Nsteps){
#    
#   }
# }

# Estimate I,t+1
Lambda_est <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)
for (t in 1:(Nsteps-1)){
  for (i in 1:Nquarter){
    Lambda_est[i, t] <- S_est[i, t] / N_est[i, t] * phi_sample * sum(beta_sample_itr[i, ] * I_it_est[, t])
    I_it_est[i, t+1] <- rpois(1, Lambda_est[i, t])
    S
  }
}

