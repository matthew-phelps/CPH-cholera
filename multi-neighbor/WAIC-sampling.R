# Author: Matthew Phelps - using code from Matt Denwood (WAIC function)
# Desc: WAIC sampling for all model runs
rm(list=ls())
library(runjags)
library(rjags)
source("Functions/WAIC-function.R")


# Function ----------------------------------------------------------------

waicRep <- function(x) {
  waic_list_tmp <- list()
  for(i in 1:length(x)){
    ll <- jags.samples(as.jags(x[[i]]), c('lik', 'llsim'), type=c('mean','variance'), 10000)
    
    mean_lik <- apply(ll$mean$lik,c(1,2),mean)
    
    var_loglik <- apply(ll$variance$llsim, c(1,2),mean)
    # Remove first row because we start at t + 1
    mean_lik <- mean_lik[2:nrow(mean_lik), ]
    var_loglik <- var_loglik[2:nrow(var_loglik), ]
    waic_list_tmp[[i]] <-  get_waic(mean_lik, var_loglik)
  }
  waic_list_tmp
}



# RUN FUNCTION --------------------------------------------------------------------

load(file = "Data/Rdata/jags_m1_ls-new-inits.Rdata")
waic_m1_ls <- waicRep(jags_m1_ls)
rm(jags_m1_ls)

load(file = "Data/Rdata/jags_m2_ls-new.Rdata")
waic_m2_ls <- waicRep(jags_m2_ls)
rm(jags_m2_ls)

load(file = "Data/Rdata/jags_m3_ls-new.Rdata")
waic_m3_ls <- waicRep(jags_m3_ls)
rm(jags_m3_ls)

load(file = "Data/Rdata/jags_m4_ls-new.Rdata")
waic_m4_ls <- waicRep(jags_m4_ls)
rm(jags_m4_ls)

load(file = "Data/Rdata/jags_m5_ls-new.Rdata")
waic_m5_ls <- waicRep(jags_m5_ls)
rm(jags_m5_ls)

load(file = "Data/Rdata/jags_m6_ls-new.Rdata")
waic_m6_ls <- waicRep(jags_m6_ls)
rm(jags_m6_ls)

waic_list <- list(waic_m1_ls = waic_m1_ls, waic_m2_ls = waic_m2_ls,
                  waic_m3_ls = waic_m3_ls, waic_m4_ls = waic_m4_ls,
                  waic_m5_ls = waic_m5_ls, waic_m6_ls = waic_m6_ls)


# SAVE --------------------------------------------------------------------
save(waic_list, file = "Data/Rdata/waic_ls.Rdata")
