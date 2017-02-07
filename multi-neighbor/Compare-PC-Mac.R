# Author: Matthew Phelps
# Compare JAGs from Mac and PC to check if they produce the same results
# 
library(parallel)
library(runjags)
library(rjags)
library(tidyverse)
rm(list=ls())
ifelse(grepl("C:", getwd()), 
       g_drive <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata",
       g_drive <- "d")
old_wd <- getwd()

# PC file
load(file = "Data/Rdata/jags_m2_ls-new-inits.Rdata")
jags_m2_pc <- jags_m2_ls
rm(jags_m2_ls)

# Mac file
setwd(g_drive)
load("jags_m2_ls-new-inits-mac.Rdata")
jags_m2_mac <- jags_m2_ls
rm(jags_m2_ls)
setwd(old_wd)

# Function Def ------------------------------------------------------------
c_jags <- function(x){
  x %>%
    combine.MCMC() %>% # Combine different reps into one
    combine.MCMC() # Combine different chains into 1 chain
}


# MCMC PREP ---------------------------------------------------------------
# PC
y <- c_jags(jags_m2_pc)
pc_median <- apply(y, MARGIN = 2, FUN = median)
# Get median values for each parameter
mcmc_names <- names(y[1, ]) # name the rows

# MAC
z <- c_jags(jags_m2_mac)
mac_median <- apply(z, MARGIN = 2, FUN = median)
mac_median - pc_median
rm(y,z)
gc()

# DIC ---------------------------------------------------------------------

cl <- makeCluster(5)
dic_m2_pc <- parLapply(cl, jags_m2_pc, extract.runjags, "dic")
dic_m2_mac <- parLapply(cl, jags_m2_mac, extract.runjags, "dic")
stopCluster(cl)

dic_m2_compare <- list(dic_m2_pc, dic_m2_pc)

save(dic_m2_compare, file = "Data/Rdata/dic_m2_compare.Rdata")
