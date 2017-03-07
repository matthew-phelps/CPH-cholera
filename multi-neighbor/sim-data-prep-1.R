
graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
source("Data-3-combine quarters.R")
source("multi-neighbor/sim-data-prep-functions.R")
source("functions/CalculateRFun.R")

# MODEL 1 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m1_ls-new.Rdata")
x <- mcmcPrep(jags_m1_ls, q_names, testing = FALSE)
rm(jags_m1_ls)
gc()
mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model1 <- rbind(R_list$R_int, R_list$R_ext, R_list$R_tot)

save(mcmc_out, file = 'Data/Rdata/sim-model-1-data-1.Rdata' )
save(R_model1, file = 'Data/Rdata/r-values-model-1.Rdata')

# MODEL 2 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m2_ls-new.Rdata")
x <- mcmcPrep(jags_m2_ls, q_names, testing = FALSE)
rm(jags_m2_ls)
x$int_hpd$int_hpd
gc()
mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model2 <- rbind(R_list$R_int, R_list$R_ext, R_list$R_tot)
save(mcmc_out, file = 'Data/Rdata/sim-model-2-data-1.Rdata' )
save(R_model2, file = 'Data/Rdata/r-values-model-2.Rdata')


# MODEL 3 -----------------------------------------------------------------
load(file = "Data/Rdata/jags_m3_ls-new.Rdata")
x <- mcmcPrep(jags_m3_ls, q_names, testing = FALSE)
rm(jags_m3_ls)
x$int_hpd$int_hpd
gc()
mcmc_out <- smMcmc(x)
rm(x)
gc()

R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model3 <- rbind(R_list$R_int, R_list$R_ext, R_list$R_tot)


save(mcmc_out, file = 'Data/Rdata/sim-model-3-data-1.Rdata' )
save(R_model3, file = 'Data/Rdata/r-values-model-3.Rdata')

# MODEL 4 -----------------------------------------------------------------
load(file = "Data/Rdata/jags_m4_ls-new.Rdata")
x <- mcmcPrep(jags_m4_ls, q_names, testing = FALSE)
rm(jags_m4_ls)
x$int_hpd$int_hpd
gc()
mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model4 <- rbind(R_list$R_int, R_list$R_ext, R_list$R_tot)

save(mcmc_out, file = 'Data/Rdata/sim-model-4-data-1.Rdata' )
save(R_model4, file = 'Data/Rdata/r-values-model-4.Rdata')

# MODEL 5 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m5_ls-new.Rdata")
x <- mcmcPrep(jags_m5_ls, q_names, testing = FALSE)
rm(jags_m5_ls)
gc()
mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model4 <- rbind(R_list$R_int, R_list$R_ext, R_list$R_tot)

save(mcmc_out, file = 'Data/Rdata/sim-model-5-data-1.Rdata' )
save(R_model4, file = 'Data/Rdata/r-values-model-5.Rdata')