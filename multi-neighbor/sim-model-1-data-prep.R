

# Intro -------------------------------------------------------------------

graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
load(file = "Data/Rdata/quarter_combined.Rdata")
load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
load(file = "Data/Rdata/multi-model-1-dataList.Rdata")
load(file = "Data/Rdata/jags_m1_ls.Rdata")

N_i_daily <- N_pop
I_it_daily <- I_reps
q_names <- N_pop$quarter




# WEEKLY AVG --------------------------------------------------------------

# Find daily avg incidence each week. Plot daily avg incidence at weekly
# time-steps to use as our "observed" data.

weekly_avg <- combined
weekly_avg$week.id <- 1:16
weekly_avg$avg <- weekly_avg$sick.total.week/7
weekly_avg <- dplyr::select(weekly_avg, c(quarter, week.id, avg))



# MCMC PREP ---------------------------------------------------------------

# Combine chains into 1
y <- jags_m1_ls %>%
  combine.MCMC() %>%
  combine.MCMC()
rm(jags_m1_ls)
gc()

# Get median values for each parameter
mcmc_median <- apply(y, MARGIN = 2, FUN = median)
mcmc_names <- names(y[1, ]) # name the rows


# Convert to matrix format for easier reading
betas_temp <- mcmc_median[1:81]
mkDf <- function(x){
  dimx <- sqrt(length(x))
  data.frame(matrix(x, nrow = dimx, ncol = dimx))
}
betas <- mkDf(betas_temp)
rm(betas_temp)

# Re-order based on alphabetical. Should already by alphabetical, but just in
# case
matNames <- function(x, names_m) {
  rownames(x) <- names_m
  colnames(x) <- names_m
  x
}
matOrderFun <- function(x) {
  x[order(rownames(x)), order(colnames(x))]
}
betas <- matNames(betas, q_names)
betas <- matOrderFun(betas)


phi <- mcmc_median['phi']


# 95% HDP -----------------------------------------------------------------

int_hpd <- data.frame(HPDinterval(y, 0.95))
hi_hdp_tmp <- int_hpd$upper[1:81]
lo_hpd_tmp <- int_hpd$lower[1:81]
hi_hdp <- hi_hdp_tmp %>%
  mkDf() %>%
  matNames(q_names)%>%
  matOrderFun()%>%
  as.matrix()

lo_hpd <- lo_hpd_tmp %>%
  mkDf() %>%
  matNames(q_names)%>%
  matOrderFun() %>%
  as.matrix()
rm(lo_hpd_tmp, hi_hdp_tmp)

phi_hdp <- int_hpd %>%
  slice(82)

# INITIALIZE EMPTY DF -----------------------------------------------------


N_it <- matrix(NA, Nquarter, 1)
N_it[, 1] <- unique(combined$est.pop.1853)



# SAVE --------------------------------------------------------------------
# If in future we sample from posterior, keep "y" object that I remove below 
# rm(combined, dataList, N_i_daily, mcmc_median, q_names_old, y, matNames, matOrderFun, mkDf)
# gc()
# save(int_hpd, file = 'Data/Rdata/int_hpd.Rdata')
# rm(int_hpd)
# save(lo_hpd, hi_hdp, phi_hdp, file = "Data/Rdata/param_ci.Rdata")
rm(lo_hpd, hi_hdp, phi_hdp)
save(list = ls(), file = 'Data/Rdata/sim-model-1-data.Rdata' )
# write.csv(betas, file = "Data/Rdata/betas-matrix.csv")