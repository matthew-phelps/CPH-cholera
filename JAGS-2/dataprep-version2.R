
graphics.off()
rm(list = ls())
library(coda)
library(dplyr)
library(runjags)

# LOAD & PREP DATA ---------------------------------------------------------------
source("Data-3-combine quarters.R")
source("functions/sim-data-prep-functions.R")
source("functions/CalculateRFun.R")

# ALL MODELS ---------------------------------------------------------------
model_num <- c(1:2)
models_data <- lapply(model_num, function(x){
  # Load all models, make nice mcmc output, and calculate R values
  file_name <- paste("Data/Rdata/model_", x, "_jags.Rdata", sep="")
  load(file_name)
  out1 <- mcmcPrep(fit_model, q_names, testing = FALSE)
  mcmc_out <- smMcmc(out1)
  R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                  lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                  q_names = q_names, order = TRUE)
  
  R_model <- list(R_median = R_list$R_median,
                   R_vals = rbind(R_list$R_int, R_list$R_ext, R_list$R_in, R_list$R_tot))
  return(list(mcmc_out = mcmc_out,
              R_model = R_model))
})

save(models_data, file = 'Data/Rdata/models-version2-data.Rdata' )