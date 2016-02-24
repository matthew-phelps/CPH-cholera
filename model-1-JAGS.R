# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853. Using interpolated incidence
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, Data-3
# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
rm(list = ls())
library(plyr)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
library(ggmcmc)
options(mc.cores = (parallel::detectCores()-1 ))


# LOAD -------------------------------------------------------
rm(list = ls())
load(file = "data\\Rdata\\model-1-data-prep.Rdata")




# REPLICATE 1 -------------------------------------------------------------
# Save in list form to pass to JAGS
dataList <- list(N_i_daily = N_i_daily,betydning
                 I_incidence=I_rep1,
                 Nsteps=Nsteps)

# JAGS 1 
# jags <- jags.model('Rcodes\\stan-model_Fitting-one-replicate.stan',
#                    data = dataList,
#                    n.chains = 1,
#                    n.adapt = 1000)

# JAGS 2 
set.seed(13) # Not sure if this does anything in current set-up
model_1_jags_rep1 <- run.jags(model = 'Rcodes\\stan-model_Fitting-one-replicate.stan',
                           method = 'parallel',
                           monitor = c('beta', 'phi'),
                           data = dataList,
                           n.chains = 5,
                           adapt = 1000,
                           burnin = 100000,
                           sample = 100000,
                           thin = 3,
                           plots = T)

model_1_coda_rep1 = as.mcmc.list( model_1_jags_rep1 )

# JAGS DIAGNOSTICS -
print(model_1_jags_rep1)

model_1_ggs_rep1 <- ggs(model_1_coda_rep1)
trace_beta <- ggs_traceplot(model_1_ggs_rep1, family = 'beta', simplify = .3) +
  theme(legend.position = 'none')
density_beta <- ggs_density(model_1_ggs_rep1, family = 'beta') +
  theme(legend.position = 'none')
running_beta <- ggs_running(model_1_ggs_rep1, family = 'beta') +
  theme(legend.position = 'none')

trace_phi <- ggs_traceplot(model_1_ggs_rep1, family = 'phi', simplify = .3) +
  theme(legend.position = 'none')
density_phi <- ggs_density(model_1_ggs_rep1, family = 'phi') +
  theme(legend.position = 'none')
running_phi <- ggs_running(model_1_ggs_rep1, family = 'phi') +
  theme(legend.position = 'none')


 # Get beta PE into human readible matrix 
model_1_out <-as.data.frame(print(model_1_jags_rep1))
beta_summary_1 <- model_1_out[1, ]
phi_summary_1 <- model_1_out[2, ]

# SAVE
# Save the specific replicate that was used to fit model
# save(I_incidence, file = "data\\Rdata\\I_incidence.Rdata")

save(model_1_coda_rep1, file = "data\\Rdata\\model-1-rep1-mcmc-output.Rdata")
save(model_1_jags_rep1, file = 'data\\Rdata\\model-1-rep1-jags.Rdata')

save(beta_summary_1, file = 'data\\Rdata\\beta-summary-rep1-1.Rdata')
save(phi_summary_1, file = 'data\\Rdata\\phi-summary-rep1-1.Rdata')
save(dataList, file = 'data\\Rdata\\model-1-rep1-dataList.Rdata')

ggsave(trace_beta, filename = "Output\\MCMC\\trace-beta-model-1-rep1.jpg")
ggsave(density_beta, filename = 'Output\\MCMC\\density-beta-model-1-rep1.jpg')
ggsave(running_beta, filename = 'Output\\MCMC\\running-beta-model-1-rep1.jpg')

ggsave(trace_phi, filename = "Output\\MCMC\\trace-phi-1-rep1.jpg")
ggsave(density_phi, filename = 'Output\\MCMC\\density-phi-1-rep1.jpg')
ggsave(running_phi, filename = 'Output\\MCMC\\running-phi-1-rep1.png')



################################################################################
################################################################################
################################################################################
################################################################################

# REPLICATE 2 -------------------------------------------------------------
rm(list = ls())
load(file = "data\\Rdata\\model-1-data-prep.Rdata")
# Save in list form to pass to JAGS
dataList <- list(N_i_daily = N_i_daily,
                 I_incidence=I_rep2,
                 Nsteps=Nsteps)

# JAGS 1 
# jags <- jags.model('Rcodes\\stan-model_Fitting-one-replicate.stan',
#                    data = dataList,
#                    n.chains = 1,
#                    n.adapt = 1000)

# JAGS 2 
set.seed(13) # Not sure if this does anything in current set-up
model_1_jags_rep2 <- run.jags(model = 'Rcodes\\stan-model_Fitting-one-replicate.stan',
                              method = 'parallel',
                              monitor = c('beta', 'phi'),
                              data = dataList,
                              n.chains = 5,
                              adapt = 1000,
                              burnin = 100000,
                              sample = 100000,
                              thin = 3,
                              plots = T)

model_1_coda_rep2 = as.mcmc.list( model_1_jags_rep2 )

# JAGS DIAGNOSTICS -
print(model_1_jags_rep2)

model_1_ggs_rep2 <- ggs(model_1_coda_rep2)
trace_beta <- ggs_traceplot(model_1_ggs_rep2, family = 'beta', simplify = .3) +
  theme(legend.position = 'none')
density_beta <- ggs_density(model_1_ggs_rep2, family = 'beta') +
  theme(legend.position = 'none')
running_beta <- ggs_running(model_1_ggs_rep2, family = 'beta') +
  theme(legend.position = 'none')

trace_phi <- ggs_traceplot(model_1_ggs_rep2, family = 'phi', simplify = .3) +
  theme(legend.position = 'none')
density_phi <- ggs_density(model_1_ggs_rep2, family = 'phi') +
  theme(legend.position = 'none')
running_phi <- ggs_running(model_1_ggs_rep2, family = 'phi') +
  theme(legend.position = 'none')


# Get beta PE into human readible matrix 
model_1_out <-as.data.frame(print(model_1_jags_rep2))
beta_summary_1 <- model_1_out[1, ]
phi_summary_1 <- model_1_out[2, ]

# SAVE
# Save the specific replicate that was used to fit model
save(I_incidence, file = "data\\Rdata\\I_incidence.Rdata")

save(model_1_coda_rep2, file = "data\\Rdata\\model-1-rep2-mcmc-output.Rdata")
save(model_1_jags_rep2, file = 'data\\Rdata\\model-1-rep2-jags.Rdata')

save(beta_summary_1, file = 'data\\Rdata\\beta-summary-rep2-1.Rdata')
save(phi_summary_1, file = 'data\\Rdata\\phi-summary-rep2-1.Rdata')
save(dataList, file = 'data\\Rdata\\model-1-rep2-dataList.Rdata')

ggsave(trace_beta, filename = "Output\\MCMC\\trace-beta-model-1-rep2.jpg")
ggsave(density_beta, filename = 'Output\\MCMC\\density-beta-model-1-rep2.jpg')
ggsave(running_beta, filename = 'Output\\MCMC\\running-beta-model-1-rep2.jpg')

ggsave(trace_phi, filename = "Output\\MCMC\\trace-phi-1-rep2.jpg")
ggsave(density_phi, filename = 'Output\\MCMC\\density-phi-1-rep2.jpg')
ggsave(running_phi, filename = 'Output\\MCMC\\running-phi-1-rep2.png')