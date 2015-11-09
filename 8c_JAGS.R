# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853. Currently uses combined quarters 
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())
library(MASS)
library(deSolve)
library(xlsx)
library(plyr)
library(ggplot2)
library(reshape2)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
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

S_ti <- matrix(0, Nquarter, Nsteps)
I_it <- matrix(0, Nquarter, Nsteps)
I_it_sampled <- matrix(0, Nquarter, Nsteps)
R_t <- matrix(0, Nquarter, Nsteps)
N_i <- matrix(0, Nquarter, Nsteps)
for (i in 1:Nquarter){
  for( t in 1:Nsteps){
    S_ti[i, t] <- (combined$S[which(combined$quarterID==i)])[t]
    I_it[i, t] <- (combined$sick.total.week[which(combined$quarterID==i)])[t]
    N_i[i, t] <- (combined$pop1855[which(combined$quarterID==i)])[t]
  }
}
row.names(I_it) <- q_names[, 1]


frac_suseptible_it <- matrix(0, nrow = Nquarter, ncol = Nsteps)
for (i in 1:Nquarter){
  for (t in 1:Nsteps){
    frac_suseptible_it[i, t] <- S_ti[i, t] / N_i[i, t]
  }
}



dataList <- list(Nquarter=Nquarter,
                 frac_suseptible_it = frac_suseptible_it, 
                 I_it=I_it,
                 Nsteps=Nsteps)



# Initialize variables
log_beta <- matrix(data = 0, nrow = Nquarter, ncol = Nquarter)
beta <- matrix(data = 0, nrow = Nquarter, ncol = Nquarter)
lambda <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)




# Populate log priors
for (i in 1:Nquarter){
  for (j in 1:Nquarter){
    log_beta[i, j] <- rnorm(1, 0, 10)
  }
}

logit_phi <- rnorm(n = 1, mean = 0, sd = 10)
phi <- exp(logit_phi)/(1+exp(logit_phi))

# Transform log priors into priors
for (i in 1:Nquarter){
  for (j in 1:Nquarter){
    beta[i, j] <-exp(log_beta[i, j])
    
  }
}

# Find lambda for each time/neighborhood
for (t in 1:Nsteps){
  for (i in 1:Nquarter){
    lambda[i, t] <- frac_suseptible_it[i, t] * phi * sum(beta[i, ]*I_it[, t])
  }
}

# Draw from possion for each lambda value to estimate I_t+1
for (t in 1:Nsteps-1){
  for (i in 1:Nquarter){
    I_itplus1[i, t+1] <- rpois(1, lambda = lambda[i, t])
  }
}


# JAGS --------------------------------------------------------------------

# Non-parallel run of JAGS model
# jags <- jags.model('Rcodes\\model1.1.bug',
#                    data = list(Nquarter=Nquarter,
#                                frac_suseptible_it = frac_suseptible_it, 
#                                I_it=I_it,
#                                Nsteps=Nsteps),
#                    n.chains = 3,
#                    n.adapt = 1000)

# update(object = jags, n.iter = 10000)
# 
# model1.1_fit <-coda.samples(model = jags, 
#                             variable.names = 'beta',
#                             n.iter = 15000)
# plot(model1.1_fit[, 1])
# gelman.plot(model1.1_fit[,1])

# Run jags using parallel
par.jags <- run.jags(model = 'Rcodes\\model1.1.bug',method = 'parallel',
                     monitor = c('beta', 'phi'),
                     data = dataList,
                     n.chains = 3,
                     adapt = 5000,
                     burnin = 10000,
                     sample = 550000,
                     thin = 2,
                     summarise = F,
                     plots = F)

par.jags.2 <- add.summary(par.jags)
model1.1_coda = as.mcmc.list( par.jags )

save(par.jags.2, file = 'data\\Rdata\\par_jags_2.Rdata')

# JAGS DIAGNOSTICS --------------------------------------------------------

print(par.jags.2)
gelman.diag(par.jags.2)
plot(par.jags.2, layout=c(4, 3))


plot(model1.1_coda[,1:2])
rmeanplot(model1.1_coda[, 1:10])




# # Get beta PE into human readible matrix --------------------------------

model1_out <-as.data.frame(print(par.jags.2))
nrow(model1_out)
beta_summary <- model1_out[1:64, ]
phi_summary <- model1_out[193, ]
save(beta_summary, file = 'data\\Rdata\\beta_summary.Rdata')
save(phi_summary, file = 'data\\Rdata\\phi_sumphi_summary.Rdata')



beta_vect <-0
for (i in 1:(Nquarter*Nquarter)){
beta_vect[i] <- round(summary(model1.1_coda[,i])$statistics[1], digits = 3)
}
beta_pe <- as.data.frame(matrix(beta_vect, nrow = Nquarter, ncol = Nquarter))

row.names(beta.pe) <- q_names[,1]
colnames(beta.pe) <- q_names[,1]

# Get Lambda PE into human readible matrix
lambda_vect <- 0
for (i in (Nquarter*Nquarter + 1):(Nquarter*Nsteps + Nquarter*Nquarter)){
  lambda_vect[i  - (Nquarter*Nquarter)] <- round(summary(model1.1_coda[,i])$statistics[1], digits = 3)
}
lambda_pe <- as.data.frame(matrix(lambda_vect, nrow = Nquarter, ncol = Nsteps))
row.names(lambda_pe) <- q_names[,1]

I_est <- as.data.frame(matrix(data = 0, nrow = Nquarter, ncol = Nsteps))
for (i in 1:Nquarter){
  for (t in 1:Nsteps){
    I_est[i, t] <- rpois(1, lambda_pe[i, t])
  }
}

row.names(I_est) <- q_names[,1]

