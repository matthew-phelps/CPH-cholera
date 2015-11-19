# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853. Using interpolated incidence 
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, Data-3



# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())
library(plyr)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
library(ggmcmc)
options(mc.cores = (parallel::detectCores()-1 ))


# COMBINED quarters -------------------------------------------------------
rm(list = ls())
load(file = "data\\Rdata\\Data_4.Rdata")


# DATA SHAPE --------------------------------------------------------------
# Restrict to only Christianshavn
I_it_daily <- I_it_daily[1,]
S_it_daily <- S_it_daily[1,]
N_i_daily <- N_i_daily[1, ]



dataList <- list(N_i_daily = N_i_daily,
                 I_it_daily=I_it_daily,
                 Nsteps=Nsteps)




jags <- jags.model('Rcodes\\stan_model_0_3.stan',
                   data = dataList,
                   n.chains = 1,
                   n.adapt = 1000)


# JAGS model 0.1 - quarter phi's --------------------------------------------------------------------

model_0_3_jags <- run.jags(model = 'Rcodes\\stan_model_0_3.stan', method = 'parallel',
                           monitor = c('beta', 'phi', 'lambda'),
                           data = dataList,
                           n.chains = 3,
                           adapt = 1000,
                           burnin = 10000,
                           sample = 20000,
                           thin = 3,
                           plots = T)

model_0_3_coda = as.mcmc.list( model_0_3_jags )



# JAGS DIAGNOSTICS --------------------------------------------------------

print(model_0_3_jags)
# gelman.diag(model_0_3_jags)
# 
# 
# 
# plot(model_0_3_coda[,1])
# rmeanplot(model_0_3_coda[, 1])
# 
# phi1 <- model_0_3_coda[, 1]
# gelman.diag(phi1)

model_0_1_ggs <- ggs(model_0_3_coda)



trace_beta <- ggs_traceplot(model_0_1_ggs, family = 'beta', simplify = .3)
density_beta <- ggs_density(model_0_1_ggs, family = 'beta')
running_beta <- ggs_running(model_0_1_ggs, family = 'beta')


ggsave(trace_beta, filename = "Output\\MCMC\\trace_beta.jpg")
ggsave(density_beta, filename = 'Output\\MCMC\\density_beta.jpg')
ggsave(running_beta, filename = 'Output\\MCMC\\running_beta.png')


trace_phi <- ggs_traceplot(model_0_1_ggs, family = 'phi', simplify = .3)
density_phi <- ggs_density(model_0_1_ggs, family = 'phi')
running_phi <- ggs_running(model_0_1_ggs, family = 'phi')


ggsave(trace_phi, filename = "Output\\MCMC\\trace_phi.jpg")
ggsave(density_phi, filename = 'Output\\MCMC\\density_phi.jpg')
ggsave(running_phi, filename = 'Output\\MCMC\\running_phi.png')

