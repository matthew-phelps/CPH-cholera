# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853. Currently uses combined quarters 
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
load(file = "data\\Rdata\\Data_3.Rdata")


# DATA SHAPE --------------------------------------------------------------
# Restrict to only Christianshavn
I_it <- I_it[1,]
S_it <- S_it[1,]
N_i <- N_i[1, ]



dataList <- list(N_i = N_i,
                I_it=I_it,
                Nsteps=Nsteps)




jags <- jags.model('Rcodes\\model_0_1.stan',
                   data = dataList,
                   n.chains = 1,
                   n.adapt = 1000)


# JAGS model 0.1 - quarter phi's --------------------------------------------------------------------

model_0_1_jags <- run.jags(model = 'Rcodes\\model_0_1.stan', method = 'parallel',
                           monitor = c('beta', 'phi', 'mu', 'tau', 'sigma', 'logit_phi', 'lambda'),
                           data = dataList,
                           n.chains = 3,
                           adapt = 5000,
                           burnin = 10000,
                           sample = 100000,
                           thin = 3,
                           plots = F)

model_0_1_coda = as.mcmc.list( model_0_1_jags )



# JAGS DIAGNOSTICS --------------------------------------------------------

print(model_0_1_jags)
# gelman.diag(model_0_1_jags)
# 
# 
# 
# plot(model_0_1_coda[,1])
# rmeanplot(model_0_1_coda[, 1])
# 
# phi1 <- model_0_1_coda[, 1]
# gelman.diag(phi1)

model_0_1_ggs <- ggs(model_0_1_coda)

# ggmcmc(model_0_1_ggs, file = 'Output\\MCMC\\model_0_1.pdf',
#        family = 'beta')



# trace_beta <- ggs_traceplot(model_0_1_ggs, family = 'beta', simplify = .3)
# density_beta <- ggs_density(model_0_1_ggs, family = 'beta')
# running_beta <- ggs_running(model_0_1_ggs, family = 'beta')
# 
# 
# ggsave(trace_beta, filename = "Output\\MCMC\\trace_beta.jpg")
# ggsave(density_beta, filename = 'Output\\MCMC\\density_beta.jpg')
# ggsave(running_beta, filename = 'Output\\MCMC\\running_beta.png')
# 
# 
# trace_phi <- ggs_traceplot(model_0_1_ggs, family = 'phi', simplify = .3)
# density_phi <- ggs_density(model_0_1_ggs, family = 'phi')
# running_phi <- ggs_running(model_0_1_ggs, family = 'phi')
# 
# 
# ggsave(trace_phi, filename = "Output\\MCMC\\trace_phi.jpg")
# ggsave(density_phi, filename = 'Output\\MCMC\\density_phi.jpg')
# ggsave(running_phi, filename = 'Output\\MCMC\\running_phi.png')



# # Get beta PE into human readible matrix --------------------------------

model_0_1_out <-as.data.frame(print(model_0_1_jags))
nrow(model_0_1_out)
beta_summary_0_1 <- model_0_1_out[1, ]
phi_summary_0_1 <- model_0_1_out[2, ]



# SAVE --------------------------------------------------------------------

save(model_0_1_jags, file = 'data\\Rdata\\model_0_1_jags.Rdata')

save(beta_summary_0_1, file = 'data\\Rdata\\beta_summary_0_1.Rdata')
save(phi_summary_0_1, file = 'data\\Rdata\\phi_summary_0_1.Rdata')
