# Author: Matthew Phelps
#Desc: Used for CHRISTIANSHAVN. ONLY for PLOTTING purposes
#
## Dependicies: Data 1, Data 2, 5_GLM_data_reshape, Data-3
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
options(mc.cores = (parallel::detectCores()-2 ))


# LOAD -------------------------------------------------------
rm(list = ls())
load(file = "data\\Rdata\\model-1-data-prep_CHRIST.Rdata")




# REPLICATE 1 -------------------------------------------------------------
# Save in list form to pass to JAGS
dataList <- list()
for (i in 1:num_replicates) {
  dataList[[i]] <- list(N_i_daily = N_i_daily,
                   I_incidence=as.vector(I_rep[, i]),
                   Nsteps=Nsteps)
}


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
                              data = dataList[[1]],
                              n.chains = 5,
                              adapt = 100,
                              burnin = 10,
                              sample = 10,
                              thin = 3,
                              plots = T)



model_1_coda = as.mcmc.list( model_1_jags )
model_1_coda <- model_1_coda



# JAGS DIAGNOSTICS --------------------------------------------------------

print(model_1_jags)


model_1_ggs <- ggs(model_1_coda)
trace_beta <- ggs_traceplot(model_1_ggs, family = 'beta', simplify = .3) +
  theme(legend.position = 'none')
density_beta <- ggs_density(model_1_ggs, family = 'beta') +
  theme(legend.position = 'none')
running_beta <- ggs_running(model_1_ggs, family = 'beta') +
  theme(legend.position = 'none')

trace_phi <- ggs_traceplot(model_1_ggs, family = 'phi', simplify = .3) +
  theme(legend.position = 'none')
density_phi <- ggs_density(model_1_ggs, family = 'phi') +
  theme(legend.position = 'none')
running_phi <- ggs_running(model_1_ggs, family = 'phi') +
  theme(legend.position = 'none')



# Get beta PE into human readible matrix --------------------------------

model_1_out <-as.data.frame(print(model_1_jags))
nrow(model_1_out)
beta_summary_1 <- model_1_out[1, ]
phi_summary_1 <- model_1_out[2, ]

# SAVE --------------------------------------------------------------------

save(model_1_coda, file = "data\\Rdata\\model-1-mcmc-output-CHRIST.Rdata")
save(model_1_jags, file = 'data\\Rdata\\model-1-jags-CHRIST.Rdata')

save(beta_summary_1, file = 'data\\Rdata\\beta-summary-1-CHRIST.Rdata')
save(phi_summary_1, file = 'data\\Rdata\\phi-summary-1-CHRIST.Rdata')
save(dataList, file = 'data\\Rdata\\model-1-dataList-CHRIST.Rdata')

ggsave(trace_beta, filename = "Output\\MCMC\\trace-beta-model-1-rep1.jpg")
ggsave(density_beta, filename = 'Output\\MCMC\\density-beta-model-1-rep1.jpg')

ggsave(trace_phi, filename = "Output\\MCMC\\trace-phi-1-rep1.jpg")
ggsave(density_phi, filename = 'Output\\MCMC\\density-phi-1-rep1.jpg')
