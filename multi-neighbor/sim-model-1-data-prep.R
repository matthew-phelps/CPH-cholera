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
load(file = "Data/Rdata/jags_m1_rep_3_ext_1.Rdata")
y <- jags_m1_rep_3


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
betas_temp <- mcmc_median[1:2]
betas <- data.frame(matrix(betas_temp[2], nrow = 9, ncol = 9))
diag(betas) <- betas_temp[1]
rm(betas_temp)
rownames(betas) <- q_names
colnames(betas) <- q_names

phi <- mcmc_median["phi"]



# INITIALIZE EMPTY DF -----------------------------------------------------


N_it <- matrix(NA, Nquarter, 1)
N_it[, 1] <- unique(combined$est.pop.1853)


# SAVE --------------------------------------------------------------------
rm(combined, dataList, N_i_daily, mcmc_median, mcmc_1, mcmc_names, y)

save(list = ls(), file = 'data/Rdata/sim-model-1-data.Rdata' )

