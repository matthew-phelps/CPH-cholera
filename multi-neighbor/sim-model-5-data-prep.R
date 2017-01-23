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
load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
load(file = "Data/Rdata/multi-model-1-dataList.Rdata")
load(file = "Data/Rdata/jags_m5_ls.Rdata")

N_i_daily <- N_pop
I_it_daily <- I_reps[[1]]
Nsteps <- dataList[[1]]$Nsteps
Nquarter <- nrow(N_i_daily)
q_names <- N_pop$quarter
q_names_old <- colnames(dataList[[1]]$I_incidence)



# WEEKLY AVG --------------------------------------------------------------

# Find daily avg incidence each week. Plot daily avg incidence at weekly
# time-steps to use as our "observed" data.

weekly_avg <- combined
weekly_avg$week.id <- 1:16
weekly_avg$avg <- weekly_avg$sick.total.week/7
weekly_avg <- dplyr::select(weekly_avg, c(quarter, week.id, avg))



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

# Re-order based on alphabetical. When I ran model_5 the data was ordered
# incorrecly (not alphabetically by quarter). All other time I use quarter data
# it's arranged alphabetically, so here I re-order the output of model 5 to be
# the same
rownames(betas) <- q_names_old
colnames(betas) <- q_names_old
betas <- betas[order(rownames(betas)), order(colnames(betas))]


phi <- mcmc_median['phi']

# 95% HDI
int_hpd <- data.frame(HPDinterval(y, 0.95))


# INITIALIZE EMPTY DF -----------------------------------------------------


N_it <- matrix(NA, Nquarter, 1)
N_it[, 1] <- unique(combined$est.pop.1853)



# SAVE --------------------------------------------------------------------
# If in future we sample from posterior, keep "y" object that I remove below 
rm(combined, dataList, N_i_daily, mcmc_median, q_names_old, y)
gc()
save(int_hpd, file = 'data/Rdata/int_hpd.Rdata')
rm(int_hpd)
save(list = ls(), file = 'data/Rdata/sim-model-5-data.Rdata' )

