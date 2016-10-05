# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
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
library(plyr)
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

load(file = "multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m5_ls <- list()
dataList <- list()
num_reps <- length(I_reps)
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m5_ls[[reps]] <- run.jags(model = 'JAGS-multi-quarter-5.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi', 'llsim'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 1e4,
                                 sample = 1e4,
                                 thin = 1,
                                 plots = T)
}

setwd(data.path)
save(jags_m5_ls, file = "jags_m5_ls.Rdata")
add.summary(jags_m5_ls[[reps]])
m5_mcmc <- combine.mcmc(jags_m5_ls[[reps]], collapse.chains = F)
mcmcplot(m5_mcmc)



# WAIC --------------------------------------------------------------------
str()
m5_mcmc <- combine.mcmc(jags_m5_ls[[1]], collapse.chains = T)
m5_mcmc <- jags_m5_ls[[1]]$mcmc[1:4][, 85:1081]
x <- combine.mcmc(jags_m5_ls[[1]]$mcmc[1][, 85:1081])
str(combine.mcmc(jags_m5_ls[[1]]$mcmc[1][, 1:5]))
y <- jags_m5_ls[[2]]
rm(jags_m5_ls_waic)
gc()
x <- coda::as.array.mcmc.list(y$mcmc, chains = F)
# Remove "foi" vars
x <- x[, 1:1081, ]




# Function from footnote on pg 14 of https://goo.gl/a1GrwT
colVars <- function(a) {
  n <- dim(a)[[1]]
  c <- dim(a)[[2]]
  return(.colMeans(((a - matrix(.colMeans(a, n, c),
                                nrow = n, ncol = c,
                                byrow = TRUE)) ^2),
                   n, c) * n / (n - 1))
}

# WAIC Function
waic <- function(x) {
  # Turn mcmc object into matrix (or arrary if not collapsing chains)
  #ll <- coda::as.array.mcmc.list(x)
  ll <- x
  # Get number of variables for y
  n_var <- ncol(ll)
  # Remove non-LL variables
  ll <- ll[, 85:n_var]
  # Transpose to get in n X S form & turn to df
  ll <- t(ll)
  # S <- nrow(ll)
  # n <- ncol(ll)
  lpd <- log(colMeans(exp(ll)))
  p_waic <- colVars(ll)
  elpd_waic <- lpd - p_waic
  waic <- -2 * elpd_waic
  return(sum(waic))
}

for (i in 1:4){
  print(waic(x[, , i]))
}

#################################################
#################################################
#################################################
#################################################

load(file = "jags_m5_ls.Rdata")
dic_m5 <- list()
for (i in 1:length(jags_m5_ls)){
  dic_m5[[i]] <- extract.runjags(jags_m5_ls[[i]], what = "dic")
}
save(dic_m5, file = "dic_m5.Rdata")
dic_m5
