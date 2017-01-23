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
library(dplyr)
library(runjags)



# LOAD & PREP DATA ---------------------------------------------------------------


load(file = "Data/Rdata/quarter_combined.Rdata")
load(file = "Data/Rdata/multi-model-1-dataList.Rdata")
load(file = "Data/Rdata/jags_m5_ls.Rdata")

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
z <- combine.MCMC(jags_m5_ls) # Combine different reps into one
rm(jags_m5_ls)
gc()
y <- combine.MCMC(z) # Combine different chains into 1 chain
rm(z)
gc()
mcmc_median <- apply(y, MARGIN = 2, FUN = median)
# Get median values for each parameter
mcmc_names <- names(y[1, ]) # name the rows


# Convert to matrix format for easier reading
betas_temp <- mcmc_median[1:81]
betas <- data.frame(matrix(betas_temp, nrow = 9, ncol = 9))
rm(betas_temp)
rownames(betas) <- q_names
colnames(betas) <- q_names

phi <- mcmc_median['phi']
gamma <- mcmc_median['gamma_b']

# 95% HDI
int_hpd <- data.frame(HPDinterval(y, 0.95))


# INITIALIZE EMPTY DF -----------------------------------------------------


N_it <- matrix(NA, Nquarter, 1)
N_it[, 1] <- unique(combined$est.pop.1853)



# SAVE --------------------------------------------------------------------
rm(combined, dataList, N_i_daily, mcmc_median)
gc()
save(int_hpd, file = 'data/Rdata/int_hpd.Rdata')
rm(int_hpd)
save(list = ls(), file = 'data/Rdata/sim-model-5-data.Rdata' )

