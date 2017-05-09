# Author: Matthew Phelps
# Desc: DIC sampling for all model runs
library(runjags)
library(rjags)
library(parallel)

# Function ----------------------------------------------------------------
SampleDIC_rep <- function(x, cl_num){
  # Sample DIC from all epidemic relizations using multiple cores
  cl <- makeCluster(cl_num)
  dic_samp <- parLapply(cl, x, extract.runjags, "dic")
  stopCluster(cl)
  dic_samp
}

# RUN FUNCTION --------------------------------------------------------------------





load("data/Rdata/jags_m2b.Rdata")
DIC_m2b_ls <- SampleDIC_rep(jags_m2_ls, 7)
rm(jags_m2_ls)

load("data/Rdata/jags_m2c.Rdata")
DIC_m2c_ls <- SampleDIC_rep(fit_model, 7)
rm(fit_model)


# SAVE --------------------------------------------------------------------

save(DIC_m2b_ls, file="Data/Rdata/DIC_m2b_tmp.Rdata")
save(DIC_m2c_ls, file="Data/Rdata/DIC_m2c_tmp.Rdata")