
graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
source("Data-3-combine quarters.R")
source("functions/sim-data-prep-functions.R")
source("functions/CalculateRFun.R")

# MODEL 1 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m1_ls-new.Rdata")
x <- mcmcPrep(jags_m1_ls, q_names, testing = FALSE)
rm(jags_m1_ls)

mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model1 <- list(R_median = R_list$R_median,
                 R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_tot))

save(mcmc_out, file = 'Data/Rdata/sim-model-1-data-1.Rdata' )
save(R_model1, file = 'Data/Rdata/r-values-model-1.Rdata')





# MODEL 2 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m2_ls-new.Rdata")
x <- mcmcPrep(jags_m2_ls, q_names, testing = FALSE)
rm(jags_m2_ls)

mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model2 <- list(R_median = R_list$R_median,
                 R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_tot))
save(mcmc_out, file = 'Data/Rdata/sim-model-2-data-1.Rdata' )
save(R_model2, file = 'Data/Rdata/r-values-model-2.Rdata')


# MODEL 2b ---------------------------------------------------------------
load(file = "data/Rdata/jags_m2b.Rdata")
x <- mcmcPrep(jags_m2_ls, q_names, testing = FALSE)
rm(jags_m2_ls)

mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model2b <- list(R_median = R_list$R_median,
                 R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_tot))
save(mcmc_out, file = 'Data/Rdata/sim-model-2b-data-1.Rdata' )
save(R_model2b, file = 'Data/Rdata/r-values-model-2b.Rdata')



# MODEL 2c ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m2c.Rdata")
x <- mcmcPrep(fit_model, q_names, testing = FALSE)
rm(fit_model)

mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model2c <- list(R_median = R_list$R_median,
                 R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_tot))
save(mcmc_out, file = 'Data/Rdata/sim-model-2c-data-1.Rdata' )
save(R_model2c, file = 'Data/Rdata/r-values-model-2c.Rdata')



# MODEL 3 -----------------------------------------------------------------
load(file = "Data/Rdata/jags_m3_ls-new.Rdata")
x <- mcmcPrep(jags_m3_ls, q_names, testing = FALSE)
rm(jags_m3_ls)

mcmc_out <- smMcmc(x)
rm(x)

R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model3 <- list(R_median = R_list$R_median,
                 R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_tot))


save(mcmc_out, file = 'Data/Rdata/sim-model-3-data-1.Rdata' )
save(R_model3, file = 'Data/Rdata/r-values-model-3.Rdata')








# MODEL 4 -----------------------------------------------------------------
load(file = "Data/Rdata/jags_m4_ls-new.Rdata")
x <- mcmcPrep(jags_m4_ls, q_names, testing = FALSE)
rm(jags_m4_ls)
x$int_hpd$int_hpd

mcmc_out <- smMcmc(x)
rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model4 <- list(R_median = R_list$R_median,
                 R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_tot))

save(mcmc_out, file = 'Data/Rdata/sim-model-4-data-1.Rdata' )
save(R_model4, file = 'Data/Rdata/r-values-model-4.Rdata')








# MODEL 5 ---------------------------------------------------------------
load(file = "Data/Rdata/jags_m5_ls-new.Rdata")
x <- mcmcPrep(jags_m5_ls, q_names, testing = FALSE)
rm(jags_m5_ls)

mcmc_out <- smMcmc(x)

rm(x)
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R_model5 <- list(R_median = R_list$R_median,
                 R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_tot,
                                R_list$R_in))
                 

save(mcmc_out, file = 'Data/Rdata/sim-model-5-data-1.Rdata' )
save(R_model5, file = 'Data/Rdata/r-values-model-5.Rdata')



# MODEL 5 RECAPTURE ---------------------------------------------------------------
load(file = "Data/Rdata/jags_recapture_m5.Rdata")
x <- mcmcPrep(re_capture_m5, q_names, testing = FALSE)
rm(re_capture_m5)

mcmc_out <- smMcmc(x)
rm(x)

save(mcmc_out, file = 'Data/Rdata/sim-model-5recapture-data.Rdata' )

