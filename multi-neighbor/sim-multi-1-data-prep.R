# Author: Matthew Phelps
#Desc: Prepare data from JAGS for simulations
# Dependicies: model-1-JAGS


# Intro -------------------------------------------------------------------

graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
rm(list = ls())

library(coda)
library(runjags)



# LOAD & PREP DATA ---------------------------------------------------------------


load(file = "Data/Rdata/quarter_combined.Rdata")
load(file = "Data/Rdata/multi-model-1-dataList.Rdata")
load(file = "/Users/Matthew/Dropbox (Personal)/AWS-Rstudio/JAGS-rep-4-ext1.Rdata")
y <- jags_rep_4


N_i_daily <- dataList[[1]]$N_i_daily
I_it_daily <- dataList[[1]]$I_incidence
Nsteps <- dataList[[1]]$Nsteps
Nquarter <- dataList[[1]]$Nquarter
q_names <- colnames(dataList[[1]]$I_incidence)



# WEEKLY AVG --------------------------------------------------------------

# Find daily avg incidence each week. Plot daily avg incidence at weekly
# time-steps to use as our "observed" data.

weekly_avg <- combined
weekly_avg$week.id <- 1:16
weekly_avg$avg <- weekly_avg$sick.total.week/7
weekly_avg <- select(weekly_avg, c(quarter, week.id, avg))



# MCMC PREP ---------------------------------------------------------------

# Combine chains into 1
mcmc_1 <- combine.mcmc(y)
mcmc_median <- apply(mcmc_1, MARGIN = 2, FUN = median)
# Get median values for each parameter
mcmc_names <- names(mcmc_1[1, ]) # name the rows


# Convert to matrix format for easier reading
betas_temp <- mcmc_median[1:81]
betas <- data.frame(matrix(betas_temp, nrow = 9, ncol = 9))
rm(betas_temp)
rownames(betas) <- q_names
colnames(betas) <- q_names

phi <- mcmc_median[82]



# INITIALIZE EMPTY DF -----------------------------------------------------


N_it <- matrix(NA, Nquarter, 1)
N_it[, 1] <- unique(combined$est.pop.1853)



# # PREPARE MCMC DRAWS ------------------------------------------------------
# 
# # Remove 1st 5000K iterations for burn in from each chain
# n_iter <- length(JagsOutput$mcmc[[1]][, 1])
# n_param <- as.numeric(length(JagsOutput$mcmc[[1]][1, ]))
# chain1 <- as.data.frame(JagsOutput$mcmc[[1]][5000:n_iter, ])
# chain2 <- as.data.frame(JagsOutput$mcmc[[2]][5000:n_iter, ])
# chain3 <- as.data.frame(JagsOutput$mcmc[[3]][5000:n_iter, ])
# chain4 <- as.data.frame(JagsOutput$mcmc[[4]][5000:n_iter, ])
# 
# betas_matrix <- rbind(chain1[, 1:n_param-1, drop = FALSE],
#                       chain2[, 1:n_param-1, drop = FALSE],
#                       chain3[, 1:n_param-1, drop = FALSE],
#                       chain4[, 1:n_param-1, drop = FALSE])
# # Drop = F is because the "[]" operatore on DF changes a single column into a row vector
# # Drop = F stops this from happening
# phi_matrix <- rbind(chain1[, 'phi', drop = FALSE],
#                     chain2[, 'phi', drop = FALSE],
#                     chain3[, 'phi', drop = FALSE],
#                     chain4[, 'phi', drop = FALSE])
# 
# rm(chain1, chain2, chain3, chain4, JagsOutput)


# # 95% HDI for EACH PARAMETER ----------------------------------------------
# 
# 
# lower_sample <- round( 0.025 * nrow(betas_matrix), digits = 0)
# upper_sample <- nrow(betas_matrix) - lower_sample
# sample_size <- length(lower_sample + 1:upper_sample)
# 
# # Beta parameter
# step1 <- as.data.frame(betas_matrix[order(betas_matrix[, 1]), ])
# betas_matrix <- as.data.frame(step1[(lower_sample + 1) : upper_sample, 1])
# rm(step1)
# 
# # Phi parameter
# step1 <- as.data.frame(phi_matrix[order(phi_matrix[, 1]), ])
# phi_matrix <- as.data.frame(step1[lower_sample + 1 : upper_sample, 1])
# rm(step1)
# 
# 
# # POINT ESTIMATES ---------------------------------------------------------
# 
# # Beta: extact mean from jags file
# step1 <- as.matrix(beta_summary_1['Mean'])
# beta_pe <- matrix(step1, nrow = 1, ncol = 1, byrow = F)
# 
# # Phi
# phi_pe <- as.matrix(phi_summary_1['Mean'])
# 
# 

# SAVE --------------------------------------------------------------------
rm(combined, dataList, N_i_daily, mcmc_median, mcmc_1, mcmc_names, y)

save(list = ls(), file = 'data/Rdata/sim-multi-1-data.Rdata' )

