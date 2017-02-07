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


# PC file
load(file = "Data/Rdata/jags_m2_ls-new-inits.Rdata")
jags_m2_pc <- jags_m2_ls
rm(jags_m2_ls)

# Mac file
setwd(g_drive)
load("jags_m2_ls-new-inits-mac.Rdata")
jags_m2_mac <- jags_m2_ls
rm(jags_m2_ls)

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
# DIC ---------------------------------------------------------------------


dic_m2_pc <- mclapply(jags_m2_pc, extract.runjags, "dic")
dic_m2_mac <- mclapply(jags_m2_mac, extract.runjags, "dic")



