# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.

# Intro -------------------------------------------------------------------
rm(list = ls())
graphics.off()

library(runjags)
library(rjags)

# LOAD --------------------------------------------------------------------

load(file = "Data/Rdata/dic_m1.Rdata")
load(file = "Data/Rdata/dic_m2.Rdata")
load(file = "Data/Rdata/dic_m3.Rdata")
load(file = "Data/Rdata/dic_m5.Rdata")
load(file = "Data/Rdata/waic_m1_ls.Rdata")
load(file = "Data/Rdata/waic_m2_ls.Rdata")
load(file = "Data/Rdata/waic_m3_ls.Rdata")
load(file = "Data/Rdata/waic_m5_ls.Rdata")

dic_list <- list(dic_m1, dic_m2, dic_m3, dic_m5)
waic_list <- list(waic_m1_ls, waic_m2_ls, waic_m3_ls, waic_m5_ls)

print(dic_m5[[1]])

getPenDic <- function(x) sum(x$deviance) + sum(x$penalty)
modelDic <- function(z) mean(sapply(z, getPenDic))
summaryDic <- function(y) sapply(y, modelDic)
summaryDic(dic_list)


mean(dic_m1[[1]]$penalty)

extDic <- function(x)

exFun <- function(z)sapply(z, function(x) x[[1]])

sapply(waic_list, exFun)
print()

waic <- data.frame(waic_m1_ls$waic)

dic_m1
dic_m2
dic_m3
dic_m5
dic_m5_b
dic <- data.frame(pd_1 = c(3941, 3844, 3977, 4086, 3988, 3994,
                           4049, 3925, 4016, 3994),
                  pd_2 = c(3637, 3552, 3648, 3753, 3666, 3671,
                           3728, 3622, 3718, 3659),
                  pd_3 = c(3605, 3552, 3613, 3705, 3632, 3629,
                           3682, 3558, 3685, 3622),
                  pd_5 = c(3507, 3417, 3538, 3660, 3525, 3498,
                           3597, 3526, 3542, 3545),
                  pd_5_b = c(3511, 3423, 3530, 3650, 3521,
                             3506, 3589, 3532, 3543, 3546),
                  pd_6 = c(3522, 3521, 3522, 3521, 3521, 3521,
                           3521, 3521, 3521, 3521))

range(dic$pd_5_b)
range(dic$pd_6)
colSums(dic)
colMeans(dic, na.rm = T)
