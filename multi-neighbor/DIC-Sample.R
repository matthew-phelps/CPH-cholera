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





load(file = "Data/Rdata/jags_m1_ls-new-inits.Rdata")
DIC_m1_ls <- SampleDIC_rep(jags_m1_ls, 6)
rm(jags_m1_ls)

load(file = "Data/Rdata/jags_m2_ls-new.Rdata")
DIC_m2_ls <- SampleDIC_rep(jags_m2_ls, 5)
rm(jags_m2_ls)

load("data/Rdata/jags_m2b.Rdata")
DIC_m2b_ls <- SampleDIC_rep(jags_m2_ls, 4)
rm(jags_m2_ls)

load("data/Rdata/jags_m2c.Rdata")
DIC_m2c_ls <- SampleDIC_rep(fit_model, 4)
rm(jags_m2_ls)


load(file = "Data/Rdata/jags_m3_ls-new.Rdata")
DIC_m3_ls <- SampleDIC_rep(jags_m3_ls, 6)
rm(jags_m3_ls)

load(file = "Data/Rdata/jags_m4_ls-new.Rdata")
DIC_m4_ls <- SampleDIC_rep(jags_m4_ls, 4)
rm(jags_m4_ls)

load(file = "Data/Rdata/jags_m5_ls-new.Rdata")
DIC_m5_ls <- SampleDIC_rep(jags_m5_ls, 6)
rm(jags_m5_ls)



DIC_list <- list(m1 = DIC_m1_ls, m2 = DIC_m2_ls,
                 m2b =DIC_m2b_ls, m2c = DIC_m2c_ls,
                 m3 = DIC_m3_ls, m4 = DIC_m4_ls,
                 m5 = DIC_m5_ls, m6 = DIC_m6_ls,
                 m7 = DIC_m7_ls)

# SAVE --------------------------------------------------------------------
save(DIC_list, file = "Data/Rdata/DIC_ls.Rdata")

