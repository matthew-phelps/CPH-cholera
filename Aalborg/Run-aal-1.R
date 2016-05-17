# Author: Matthew Phelps
#Desc: JAGS model of Aalborg 1853

# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/Aalborg",
       model.path <-"/Users/Matthew/GitClones/RCodes/Aalborg")


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
options(mc.cores = (parallel::detectCores()))
rm(amazon)

# LOAD -------------------------------------------------------

load(file = "Data-prep-jags-aal.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
model_jags_list_1 <- list()
dataList <- list()
rep_num <- 4
dataList<- list(N_i_daily = N_pop,
                I_incidence=cases,
                Nsteps=Nsteps,
                Nquarter = Nquarter)



# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)
set.seed(13) # Not sure if this does anything in current set-up
jags_aal_1 <- run.jags(model = 'JAGS-aalborg-1.stan',
                          method = 'parallel',
                          monitor = c('beta', 'phi'),
                          modules = "glm",
                          data = dataList,
                          n.chains = 4,
                          adapt = 1000,
                          burnin = 50000,
                          sample = 5000,
                          thin = 40,
                          plots = T)



add.summary(jags_aal_1)

mcmcplot(combine.mcmc(jags_aal_1, collapse.chains = F))





# SAVE --------------------------------------------------------------------

save(jags_aal_1, file = "aalborg-jags-1.Rdata")

