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
options(mc.cores = 4)
rm(amazon)

# LOAD -------------------------------------------------------

load(file = "multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m1_ls <- list()
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
jags_m1_ls[[reps]] <- run.jags(model = 'JAGS-multi-quarter-1-b.stan',
                          method = 'parallel',
                          monitor = c('beta_1', "beta_2", 'phi'),
                          modules = "glm",
                          data = dataList[[reps]],
                          n.chains = 4,
                          adapt = 1e3,
                          burnin = 1e4,
                          sample = 1e4,
                          thin = 1,
                          plots = T)
}


add.summary(jags_m1_ls[[reps]])
mcmcplot(combine.mcmc(jags_m1_ls[[reps]], collapse.chains = F))


setwd(data.path)
save(jags_m1_ls, file = "jags_m1_ls.Rdata")

################################################################################
################################################################################
################################################################################

load(file = "jags_m1_ls.Rdata")
dic_m1 <- list()
for (i in 1:length(jags_m1_ls)){
  dic_m1[[i]] <- extract.runjags(jags_m1_ls[[i]], what = "dic")
}
save(dic_m1, file = "dic_m1.Rdata")
