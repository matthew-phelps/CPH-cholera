# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
rm(list=ls())
gc()
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")


amazon <- F

ifelse(amazon == T,
       data.path <- "~/Dropbox/AWS-Rstudio",
       data.path <- data.path)
setwd(data.path)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
# library(ggmcmc)
# library(ggplot2)
options(mc.cores = 4)
rm(amazon)

# LOAD -------------------------------------------------------
water_url <- RCurl::getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/water-matrix.csv")
border_url <- RCurl::getURL("https://raw.githubusercontent.com/matthew-phelps/CPH-cholera/master/online-data/border-matrix.csv")
load(file = "multi-model1-data-prep.Rdata")

water_temp <- read.csv(text = water_url)
border_temp <- read.csv(text = border_url)

water_temp[is.na(water_temp)] <- 0
water <- as.matrix(water_temp)

border_temp[is.na(border_temp)] <- 0
border <- as.matrix(border_temp)

rm(water_temp, border_temp)

# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS

dataList <- list()
num_reps <- length(I_reps)
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[5]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter,
                           border = border)
}

# Supply initial parameter values
inits1 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_chi = 0,
               logit_phi = 0,
               log_beta = 0)
inits2 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_chi = 1,
               logit_phi = 0,
               log_beta = -1)

inits3 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_chi = -1,
               logit_phi = 0,
               log_beta = 0)

inits4 <- list(mu = 0,
               tau = 0.01,
               mu_2 = 0,
               tau_2 = 0.01,
               log_chi = 1,
               logit_phi = 0,
               log_beta = 0)

inits_list <- list(inits1, inits2, inits3, inits4)
# Model 1 -----------------------------------------------------------------

# JAGS Run the JAGS models for each iteration in a separate instance on AWS. Run
# 4 chains in each
setwd(model.path)
jags_m7_ls_waic <- list()
system.time(for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m7_ls_waic[[reps]] <- run.jags(model = 'JAGS-multi-quarter-7-WAIC.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'gamma_b', "chi", "llsim"),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 inits = inits_list,
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 2e4,
                                 sample = 3e4,
                                 thin = 2,
                                 plots = T)
}
)

# SAVE
setwd(data.path)
save(jags_m7_ls_waic, file = "jags_m7_ls_waic.Rdata")

add.summary(jags_m7_ls_waic[[reps]])


#################################################
#################################################
#################################################
#################################################


# WAIC --------------------------------------------------------------------

# Need the ll to be in a n X S dimension matrix, where n = number of observations and S = number of iterations. See pg 2, eq (3) of https://goo.gl/a1GrwT
m7_mcmc <- combine.mcmc(jags_m7_ls_waic[[reps]], collapse.chains = T)

# Turn mcmc object into matrix (or arrary if not collapsing chains)
ll <- coda::as.array.mcmc.list(m7_mcmc)
# Get number of variables for y
n_var <- ncol(ll)

# Remove non-LL variables
ll <- ll[, 85:n_var]

# Transpose to get in n X S form & turn to df
ll <- t(ll)
#ll <- as.data.frame(ll)

# Function from footnote on pg 14 of https://goo.gl/a1GrwT
colVars <- function(a) {
  n <- dim(a)[[1]]
  c <- dim(a)[[2]]
  return(.colMeans(((a - matrix(.colMeans(a, n, c),
                                nrow = n, ncol = c,
                                byrow = TRUE)) ^2),
                   n, c) * n / (n - 1))
}

S <- nrow(ll)
n <- ncol(ll)
lpd <- log(colMeans(exp(ll)))
p_waic <- colVars(ll)
elpd_waic <- lpd - p_waic
waic <- -2 * elpd_waic
sum(waic)

str(lpd)
# DIC ---------------------------------------------------------------------


load(file = "jags_m7_ls_waic.Rdata")
dic_m7_waic <- list()
for (i in 1:length(jags_m7_ls_waic)){
  dic_m7_waic[[i]] <- extract.runjags(jags_m7_ls_waic[[i]], what = "dic")
}
save(dic_m7_waic, file = "dic_m7_waic.Rdata")
dic_m7_waic
