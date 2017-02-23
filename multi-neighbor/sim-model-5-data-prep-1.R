
graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
source("Data-3-combine quarters.R")
load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
load(file = "Data/Rdata/jags_m5_ls-new.Rdata")


# MCMC PREP ---------------------------------------------------------------

# Combine chains into 1
y <- jags_m5_ls %>%
  combine.MCMC() %>%
  combine.MCMC()
rm(jags_m5_ls)
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

# SAVE --------------------------------------------------------------------
# If in future we sample from posterior, keep "y" object that I remove below 
rm(mcmc_median, y, matNames, matOrderFun, mkDf)
gc()
save(int_hpd, file = 'Data/Rdata/int_hpd.Rdata')
rm(int_hpd)
save(lo_hpd, hi_hdp, phi_hdp, file = "Data/Rdata/param_ci.Rdata")
rm(lo_hpd, hi_hdp, phi_hdp)
save(list = ls(), file = 'Data/Rdata/sim-model-5-data1.Rdata' )
write.csv(betas, file = "Data/Rdata/betas-matrix.csv")
