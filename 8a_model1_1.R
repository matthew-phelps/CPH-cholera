# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853. Currently uses combined quarters 
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape



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
load(file = "data\\Rdata\\quarter_combined.Rdata")


### Prepare data to send to Stan
Nsteps <- 16
quarterID <- as.numeric(combined$quarterID)
n <- as.numeric(length(quarterID))
Nquarter <- length(table(quarterID))
q_names <-as.data.frame(unique(combined$quarter))

S_it <- matrix(0, Nquarter, Nsteps)
I_it <- matrix(0, Nquarter, Nsteps)
N_i <- matrix(0, Nquarter, Nsteps)
for (i in 1:Nquarter){
  for( t in 1:Nsteps){
    S_it[i, t] <- (combined$S[which(combined$quarterID==i)])[t]
    I_it[i, t] <- (combined$sick.total.week[which(combined$quarterID==i)])[t]
    N_i[i, t] <- (combined$pop1855[which(combined$quarterID==i)])[t]
  }
}
row.names(I_it) <- q_names[, 1]

# 
# frac_suseptible_it <- matrix(0, nrow = Nquarter, ncol = Nsteps)
# for (i in 1:Nquarter){
#   for (t in 1:Nsteps){
#     frac_suseptible_it[i, t] <- S_ti[i, t] / N_i[i, t]
#   }
# }



dataList <- list(Nquarter=Nquarter,
                 S_it = S_it,
                 N_i = N_i,
                 I_it=I_it,
                 Nsteps=Nsteps)



# JAGS --------------------------------------------------------------------

# Non-parallel run of JAGS model
# jags <- jags.model('Rcodes\\model1.1.stan',
#                    data = dataList,
#                    n.chains = 1,
#                    n.adapt = 1000)
# 
# update(object = jags, n.iter = 500)
# 
# model1.1_fit <-coda.samples(model = jags, 
#                             variable.names = 'beta',
#                             n.iter = 15000)
# # plot(model1.1_fit[, 1])
# gelman.plot(model1.1_fit[,1])

# JAGS with model 1.2 - quarter phi's

model1_1_jags <- run.jags(model = 'Rcodes\\model1.1.stan', method = 'parallel',
                     monitor = c('beta', 'phi'),
                     data = dataList,
                     n.chains = 3,
                     adapt = 5000,
                     burnin = 10000,
                     sample = 100000,
                     thin = 3,
                     plots = F)

model_1_1_coda = as.mcmc.list( model1_1_jags )



# JAGS DIAGNOSTICS --------------------------------------------------------

print(model1_1_jags)
gelman.diag(model1_1_jags)



plot(model_1_1_coda[,65])
rmeanplot(model_1_1_coda[, 65])

phi1 <- model_1_1_coda[, 65]
gelman.diag(phi1)

model1_2_ggs <- ggs(model_1_1_coda)

ggmcmc(model1_1_ggs, file = 'Output\\MCMC\\model1_2.pdf',
       family = 'phi')


ggmcmc(model1_1_ggs, file = 'Output\\MCMC\\model1_2_rm.pdf',
       family = 'phi', plot = c('ggs_running', 'ggs_uatocorelation, ggs_Rhat, ggs_caterpillar'))

ggs_traceplot(model1_1_ggs, family = 'phi')
ggs_density(model1_1_ggs, family = 'phi')
ggs_running(model1_1_ggs, family = 'phi')


# # Get beta PE into human readible matrix --------------------------------

model1_1_out <-as.data.frame(print(model1_1_jags))
nrow(model1_1_out)
beta_summary_1_2 <- model1_1_out[1:64, ]
phi_summary_1_2 <- model1_1_out[65, ]



# SAVE --------------------------------------------------------------------






# Quarter x Quarter matrix of force of infection
beta_pe <- as.data.frame(matrix(beta_vect, nrow = Nquarter, ncol = Nquarter))
row.names(beta_pe) <- q_names[,1]
colnames(beta_pe) <- q_names[,1]


lambda_pe <- as.data.frame(matrix(lambda_vect, nrow = Nquarter, ncol = Nsteps))
row.names(lambda_pe) <- q_names[,1]



# SAVE --------------------------------------------------------------------

save(I_it, file = 'data\\Rdata\\I_it.Rdata')
save(S_it, file = 'data\\Rdata\\S_it.Rdata')

save(model1_2_jags, file = 'data\\Rdata\\model1_1_jags.Rdata')

save(beta_summary_1_2, file = 'data\\Rdata\\beta_summary_1_1.Rdata')
save(phi_summary_1_2, file = 'data\\Rdata\\phi_summary_1_1.Rdata')

save(q_names, file = 'data\\Rdata\\q_names.Rdata')

# Manual model ------------------------------------------------------------

# # Initialize variables
# log_beta <- matrix(data = 0, nrow = Nquarter, ncol = Nquarter)
# beta <- matrix(data = 0, nrow = Nquarter, ncol = Nquarter)
# lambda <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)
# 
# 
# 
# 
# # Populate log priors
# for (i in 1:Nquarter){
#   for (j in 1:Nquarter){
#     log_beta[i, j] <- rnorm(1, 0, 10)
#   }
# }
# 
# logit_phi <- rnorm(n = 1, mean = 0, sd = 10)
# phi <- exp(logit_phi)/(1+exp(logit_phi))
# 
# # Transform log priors into priors
# for (i in 1:Nquarter){
#   for (j in 1:Nquarter){
#     beta[i, j] <-exp(log_beta[i, j])
#     
#   }
# }
# 
# # Find lambda for each time/neighborhood
# for (t in 1:Nsteps){
#   for (i in 1:Nquarter){
#     lambda[i, t] <- frac_suseptible_it[i, t] * phi * sum(beta[i, ]*I_it[, t])
#   }
# }
# 
# # Draw from possion for each lambda value to estimate I_t+1
# for (t in 1:Nsteps-1){
#   for (i in 1:Nquarter){
#     I_itplus1[i, t+1] <- rpois(1, lambda = lambda[i, t])
#   }
# }
# 

