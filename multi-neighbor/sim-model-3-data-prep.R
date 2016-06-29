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
load(file = "Data/Rdata/jags_m3_ls.Rdata")
zx <- jags_m3_ls # Rename variable here so easier when switching b/w scripts


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
z <- combine.MCMC(zx) # Combine different reps into one
y <- combine.MCMC(z) # Combine different chains into 1 chain
mcmc_median <- apply(y, MARGIN = 2, FUN = median)
# Get median values for each parameter
mcmc_names <- names(y[1, ]) # name the rows


# Convert to matrix format for easier reading
betas_temp <- mcmc_median[1:18]

betas <- data.frame(matrix(NA, nrow = 9, ncol = 9)) # external beta
for(i in 1:nrow(betas)){
  betas[i, ] <- betas_temp[i + 9]
}

diag(betas) <- betas_temp[1:9] # internal betas
rm(betas_temp)
rownames(betas) <- q_names
colnames(betas) <- q_names

phi <- mcmc_median["phi"]



# INITIALIZE EMPTY DF -----------------------------------------------------


N_it <- matrix(NA, Nquarter, 1)
N_it[, 1] <- unique(combined$est.pop.1853)


# SAVE --------------------------------------------------------------------
rm(combined, dataList, N_i_daily, mcmc_median, z, zx, i, jags_m3_ls)

save(list = ls(), file = 'data/Rdata/sim-model-3-data.Rdata' )

