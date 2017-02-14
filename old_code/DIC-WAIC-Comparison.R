# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.

# Intro -------------------------------------------------------------------
library(runjags)
library(rjags)

# LOAD --------------------------------------------------------------------
load(file = "Data/Rdata/DIC_ls.Rdata")
load(file = "Data/Rdata/waic_ls.Rdata")


# DIC SUMMARY -------------------------------------------------------------
# Produce DIC summary - output is penalized deviance
getPenDic <- function(x) sum(x$deviance) + sum(x$penalty)
modelDic <- function(z) mean(sapply(z, getPenDic))
summaryDic <- function(y) sapply(y, modelDic)
summaryDic(DIC_list)




# WAIC SUMMARY ------------------------------------------------------------
mean_waic_model <- function(x) mean(sapply(x, function(x) x$waic))
waic_summary <- function(x) sapply(x, mean_waic_model)
waic_summary(waic_list)





# OLD ---------------------------------------------------------------------
# 
# dic <- data.frame(pd_1 = c(3941, 3844, 3977, 4086, 3988, 3994,
#                            4049, 3925, 4016, 3994),
#                   pd_2 = c(3637, 3552, 3648, 3753, 3666, 3671,
#                            3728, 3622, 3718, 3659),
#                   pd_3 = c(3605, 3552, 3613, 3705, 3632, 3629,
#                            3682, 3558, 3685, 3622),
#                   pd_5 = c(3507, 3417, 3538, 3660, 3525, 3498,
#                            3597, 3526, 3542, 3545),
#                   pd_5_b = c(3511, 3423, 3530, 3650, 3521,
#                              3506, 3589, 3532, 3543, 3546),
#                   pd_6 = c(3522, 3521, 3522, 3521, 3521, 3521,
#                            3521, 3521, 3521, 3521))
# 
# range(dic$pd_5_b)
# range(dic$pd_6)
# colSums(dic)
# colMeans(dic, na.rm = T)
