# Author: Matthew Phelps
#Desc: Prepare data simulation Model 0_3
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())




# LOAD & PREP DATA ---------------------------------------------------------------


load(file = "data\\Rdata\\quarter_combined.Rdata")
load(file = 'data\\Rdata\\beta_summary_0_3.Rdata')
load(file = 'data\\Rdata\\phi_summary_0_3.Rdata')
load(file = 'data\\Rdata\\model_0_3_jags.Rdata')
load(file = 'data\\Rdata\\Data_4.Rdata')
JagsOutput <- model_0_3_jags
rm(model_0_3_jags)
summary(JagsOutput)
# INITIALIZE EMPTY DF -----------------------------------------------------


I_it <- matrix(NA, 1, Nsteps-1)
S_it <- matrix(NA, 1, Nsteps-1)

N_it <- matrix(NA, 1, Nsteps)

I_i_t1 <- matrix(0, nrow = 1, ncol = 1)
S_i_t1 <- matrix(0, nrow = 1, ncol = 1)
N_i_t1 <- matrix(0, nrow = 1, ncol = 1)
I_i_t1[1, 1] <- (combined$sick.total.week[which(combined$quarterID==1)])[1]
S_i_t1[1, 1] <- (combined$S[which(combined$quarterID==1)])[1]

for (t in 1:Nsteps){
  N_it[1, t] <- (combined$pop1855[which(combined$quarterID==1)])[t]
}

# Bind first time-step of infection data to block of NAs the size of the remaining
# timesteps. These NAs will be overwritten with simulated data 
I_it_est <- (cbind(I_i_t1, I_it))
S_it_est <- (cbind(S_i_t1, S_it))

rm(N_i_t1, S_i_t1 , I_i_t1, I_it, S_it)

# PREPARE MCMC DRAWS ------------------------------------------------------

# Remove 1st 5000K iterations for burn in from each chain
n_iter <- length(JagsOutput$mcmc[[1]][, 1])
n_param <- as.numeric(length(JagsOutput$mcmc[[1]][1, ]))
chain1 <- as.data.frame(JagsOutput$mcmc[[1]][5000:n_iter, ])
chain2 <- as.data.frame(JagsOutput$mcmc[[2]][5000:n_iter, ])
chain3 <- as.data.frame(JagsOutput$mcmc[[3]][5000:n_iter, ])
chain4 <- as.data.frame(JagsOutput$mcmc[[4]][5000:n_iter, ])

betas_matrix <- rbind(chain1[, 1:n_param-1, drop = FALSE],
                      chain2[, 1:n_param-1, drop = FALSE],
                      chain3[, 1:n_param-1, drop = FALSE],
                      chain4[, 1:n_param-1, drop = FALSE])
# Drop = F is because the "[]" operatore on DF changes a single column into a row vector
# Drop = F stops this from happening
phi_matrix <- rbind(chain1[, 'phi', drop = FALSE],
                    chain2[, 'phi', drop = FALSE],
                    chain3[, 'phi', drop = FALSE],
                    chain4[, 'phi', drop = FALSE])

rm(chain1, chain2, chain3, chain4, JagsOutput)


# 95% HDI for EACH PARAMETER ----------------------------------------------


lower_sample <- round( 0.025 * nrow(betas_matrix), digits = 0)
upper_sample <- nrow(betas_matrix) - lower_sample
sample_size <- length(lower_sample + 1:upper_sample)

# Beta parameter
step1 <- as.data.frame(betas_matrix[order(betas_matrix[, 1]), ])
betas_matrix <- as.data.frame(step1[(lower_sample + 1) : upper_sample, 1])
rm(step1)

# Phi parameter
step1 <- as.data.frame(phi_matrix[order(phi_matrix[, 1]), ])
phi_matrix <- as.data.frame(step1[lower_sample + 1 : upper_sample, 1])
rm(step1)


# POINT ESTIMATES ---------------------------------------------------------

# Beta: extact mean from jags file
step1 <- as.matrix(beta_summary_0_3['Mean'])
beta_pe <- matrix(step1, nrow = 1, ncol = 1, byrow = F)

# Phi
phi_pe <- as.matrix(phi_summary_0_3['Mean'])



# SAVE --------------------------------------------------------------------

save(list = ls(), file = 'data\\Rdata\\model_0_3_sim_data.Rdata' )

