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

SampleDIC_non_parallel <- function(x){
  # Sample DIC from all epidemic relizations using multiple cores
  browser()
  dic_samp <- lapply(x, extract.runjags, "dic")
  dic_samp
}

# RUN FUNCTION --------------------------------------------------------------------
model_num <- c(1:2)

file_name <- paste("Data/Rdata/model_", model_num[1], "_jags.Rdata", sep="")

x <- lapply(model_num, function(x){
  file_name <- paste("Data/Rdata/model_", x, "_jags.Rdata", sep="")
  load(file_name)
  DIC <- SampleDIC_non_parallel(fit_model)
  return(DIC)
})


load(file = "Data/Rdata/jags_m1_ls-new-inits.Rdata")
DIC_m1_ls <- SampleDIC_rep(jags_m1_ls, 6)
rm(jags_m1_ls)

load(file = "Data/Rdata/jags_m2_ls-new.Rdata")
DIC_m2_ls <- SampleDIC_rep(jags_m2_ls, 5)
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

load(file = "Data/Rdata/jags_m6_ls-new.Rdata")
DIC_m6_ls <- SampleDIC_rep(jags_m6_ls, 6)
rm(jags_m6_ls)

load(file = "Data/Rdata/jags_m7_ls-new.Rdata")
DIC_m7_ls <- SampleDIC_rep(jags_m7_ls, 4)
rm(jags_m7_ls)


DIC_list <- list(DIC_m1_ls, DIC_m2_ls, DIC_m3_ls,
                 DIC_m4_ls, DIC_m5_ls, DIC_m6_ls)


# SAVE --------------------------------------------------------------------
save(DIC_list, file = "Data/Rdata/DIC_ls.Rdata")
