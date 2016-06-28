# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.

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
options(mc.cores = (parallel::detectCores() -3 ))
rm(amazon)

# LOAD -------------------------------------------------------

load(file = "multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
model_jags_list_1 <- list()
dataList <- list()
num_reps <- length(I_reps)
rep_num <- 8
dataList<- list(N_i_daily = N_pop[, 2],
                I_incidence=I_reps[[rep_num]],
                Nsteps=Nsteps,
                Nquarter = Nquarter)



# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 4 chains in each
setwd(model.path)
set.seed(13) # Not sure if this does anything in current set-up
jags_m1_rep_8 <- autorun.jags(model = 'JAGS-multi-quarter-1.stan',
                          method = 'parallel',
                          monitor = c('beta_1', "beta_2", 'phi'),
                          modules = "glm",
                          data = dataList,
                          n.chains = 4,
                          adapt = 1e3,
                          startburnin = 5e4,
                          startsample = 4e3,
                          thin = 1,
                          plots = T)


setwd(data.path)
save(jags_m1_rep_8, file = "jags_m1_rep_8_ext_auto.Rdata")

add.summary(jags_m1_rep_8)

mcmcplot(combine.mcmc(jags_m1_rep_8, collapse.chains = F))
