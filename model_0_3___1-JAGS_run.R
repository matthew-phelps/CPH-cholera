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
I_it_daily <- I_it_daily[1, 13:Nsteps]
S_it_daily <- S_it_daily[1,13:Nsteps]
N_i_daily <- N_i_daily[1, 13:Nsteps]
Nsteps <- length(I_it_daily)


dataList <- list(N_i_daily = N_i_daily,
                 I_it_daily=I_it_daily,
                 Nsteps=Nsteps)




jags <- jags.model('Rcodes\\stan_model_0_3.stan',
                   data = dataList,
                   n.chains = 1,
                   n.adapt = 1000)


# JAGS model 0.1 - quarter phi's --------------------------------------------------------------------

model_0_3_jags <- run.jags(model = 'Rcodes\\stan_model_0_3.stan', method = 'parallel',
                           monitor = c('beta', 'phi'),
                           data = dataList,
                           n.chains = 4,
                           adapt = 1000,
                           burnin = 10000,
                           sample = 60000,
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

model_0_3_ggs <- ggs(model_0_3_coda)



trace_beta <- ggs_traceplot(model_0_3_ggs, family = 'beta', simplify = .3)
density_beta <- ggs_density(model_0_3_ggs, family = 'beta')
running_beta <- ggs_running(model_0_3_ggs, family = 'beta')


ggsave(trace_beta, filename = "Output\\MCMC\\trace_beta_0_3.jpg")
ggsave(density_beta, filename = 'Output\\MCMC\\density_beta_0_3.jpg')
ggsave(running_beta, filename = 'Output\\MCMC\\running_beta_0_3.png')


trace_phi <- ggs_traceplot(model_0_3_ggs, family = 'phi', simplify = .3)
density_phi <- ggs_density(model_0_3_ggs, family = 'phi')
running_phi <- ggs_running(model_0_3_ggs, family = 'phi')


ggsave(trace_phi, filename = "Output\\MCMC\\trace_phi_0_3.jpg")
ggsave(density_phi, filename = 'Output\\MCMC\\density_phi_0_3.jpg')
ggsave(running_phi, filename = 'Output\\MCMC\\running_phi_0_3.png')




# # Get beta PE into human readible matrix --------------------------------

model_0_3_out <-as.data.frame(print(model_0_3_jags))
nrow(model_0_3_out)
beta_summary_0_3 <- model_0_3_out[1, ]
phi_summary_0_3 <- model_0_3_out[2, ]



# SAVE --------------------------------------------------------------------

save(model_0_3_jags, file = 'data\\Rdata\\model_0_3_jags.Rdata')
save(Nsteps, file = "data\\Rdata\\model_0_3_Nsteps")

save(beta_summary_0_3, file = 'data\\Rdata\\beta_summary_0_3.Rdata')
save(phi_summary_0_3, file = 'data\\Rdata\\phi_summary_0_3.Rdata')
save(dataList, file = 'data\\Rdata\\model_0_3_dataList.Rdata')
