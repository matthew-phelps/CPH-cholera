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
options(mc.cores = (parallel::detectCores() -4 ))
rm(amazon)

# LOAD -------------------------------------------------------

load(file = "multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
model_jags_list_1 <- list()
dataList <- list()
num_reps <- length(I_reps)
rep_num <- 4
dataList<- list(N_i_daily = N_pop[, 2],
                I_incidence=I_reps[[rep_num]],
                Nsteps=Nsteps,
                Nquarter = Nquarter)



# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)
set.seed(13) # Not sure if this does anything in current set-up
jags_m1_rep_4 <- run.jags(model = 'JAGS-multi-quarter-1.stan',
                       method = 'parallel',
                       monitor = c('beta_1', "beta_2", 'phi'),
                       modules = "glm",
                       data = dataList,
                       n.chains = 4,
                       adapt = 1e3,
                       burnin = 5e4,
                       sample = 1e2,
                       thin = 35,
                       plots = T)



add.summary(jags_m1_rep_4)

mcmcplot(combine.mcmc(jags_m1_rep_4, collapse.chains = F))

setwd(data.path)
save(jags_m1_rep_4, file = "jags_m1_rep_4_ext_1.Rdata")

jags_m1_rep_4_ext_2 <- extend.jags(jags_m1_rep_4,
                               method = "parallel",
                               adapt = 1e3,
                               sample = 1e3,
                               thin = 35)

save(jags_m1_rep_4_ext_2, file = "jags_m1_rep_4_ext_2.Rdata")
add.summary(jags_m1_rep_4_ext_2)
extract(jags_m1_rep_4_ext_2, what = "dic")

mcmcplot(combine.mcmc(jags_m1_rep_4_ext_2, collapse.chains = F))


save(jags_rep_4, file="jags_rep_4_ext1.Rdata")


jags_rep_4_ext2 <- extend.jags(jags_rep_4,
                               method = "parallel",
                               adapt = 500,
                               sample = 1000,
                               thin = 35,
                               drop.chain = c(1,3,5,7))


save(jags_rep_4_ext2, file="jags-rep-4-ext2.Rdata")


jags_rep_4_ext3 <- extend.jags(jags_rep_4_ext2,
                               method = "parallel",
                               adapt = 500,
                               sample = 1000,
                               thin = 35)

save(jags_rep_4_ext3, file="jags-rep-4-ext3.Rdata")

# S -----------------------------------------------------------------------

jags_rep_4_ext4 <- extend.jags(jags_rep_4_ext3,
                               method = "parallel",
                               adapt = 500,
                               sample = 1000,
                               thin = 35)

save(jags_rep_4_ext4, file="jags-rep-4-ext-4.Rdata")
