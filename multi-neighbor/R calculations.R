# Author: Matthew Phelps 
# Desc: Calculate the R-internal and R-external for each neighborhood using
# beta values
# 
# 
# Intro -------------------------------------------------------------------
library(tidyverse)
require(grid)
library(coda)
library(CholeraDataDK)

# LOAD data ---------------------------------------------------------------
load(file = "Data/Rdata/int_hpd.Rdata")
load(file = "Data/Rdata/param_ci.Rdata")
gamma <- 1/5
#load(file = "sim-model-5-data.Rdata") # update as more MCMCs are run
load(file = "Data/Rdata/sim-model-5-data.Rdata")
rm(I_it_daily, N_it, weekly_avg, phi, Nsteps)



# R-INTERNAL --------------------------------------------------------------
B_int_low <- diag(lo_hpd)
B_int_hi <- diag(hi_hdp)


B_int <- betas %>%
  as.matrix() %>% # convert to matrix for diag() function to work
  diag()

R_int <- data.frame(B_int / gamma )
colnames(R_int) <- "R_value"
R_int$lower <- B_int_low / gamma
R_int$upper <- B_int_hi /gamma
R_int$quarter <- q_names
R_int$R_type <- "int"
R_int$quarter <- factor(R_int$quarter, levels = R_int$quarter[order(R_int$R_value)])


# R-EXTERNAL -------------------------------------------------------------- 
# Defined as: The estimated number of infectious cases casused in all other
# quarters by a single infecious case in the target quarter

diag(lo_hpd) <- NA
diag(hi_hdp) <- NA
B_ext_low <- rowSums(lo_hpd, na.rm = T)
B_ext_hi <- rowSums(hi_hdp, na.rm = T)


x <- as.matrix(betas)
diag(x) <- NA
B_ext <- rowSums(x, na.rm = T)

R_ext <- data.frame(B_ext / gamma)
colnames(R_ext) <- "R_value"
R_ext$lower <- B_ext_low / gamma
R_ext$upper <- B_ext_hi / gamma
R_ext$quarter <- q_names
R_ext$R_type <- "ext"
R_ext$quarter <- factor(R_ext$quarter, levels = R_ext$quarter[order(R_int$R_value)])

# DATA SHAPING ------------------------------------------------------------
R <- rbind(R_int, R_ext)
rm_list <-ls()

R_ext[which.min(R_ext$R_value), ]
R_ext[which.max(R_ext$R_value), ]

R_int[which.min(R_int$R_value), ]
R_ext[which.max(R_int$R_value), ]
