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



# JAGS model 1.2 - quarter phi's --------------------------------------------------------------------

model1_2_jags <- run.jags(model = 'Rcodes\\model1.2.stan', method = 'parallel',
                     monitor = c('beta', 'phi'),
                     data = dataList,
                     n.chains = 3,
                     adapt = 5000,
                     burnin = 10000,
                     sample = 100000,
                     thin = 3,
                     plots = F)

model_1_2_coda = as.mcmc.list( model1_2_jags )



# JAGS DIAGNOSTICS --------------------------------------------------------

print(model1_2_jags)
gelman.diag(model1_2_jags)



plot(model_1_2_coda[,65])
rmeanplot(model_1_2_coda[, 65])

phi1 <- model_1_2_coda[, 65]
gelman.diag(phi1)

model1_2_ggs <- ggs(model_1_2_coda)

ggmcmc(model1_2_ggs, file = 'Output\\MCMC\\model1_2.pdf',
       family = 'phi')


ggmcmc(model1_2_ggs, file = 'Output\\MCMC\\model1_2_rm.pdf',
       family = 'phi', plot = c('ggs_running', 'ggs_uatocorelation, ggs_Rhat, ggs_caterpillar'))

ggs_traceplot(model1_2_ggs, family = 'phi')
ggs_density(model1_2_ggs, family = 'phi')
ggs_running(model1_2_ggs, family = 'phi')


# # Get beta PE into human readible matrix --------------------------------

model1_2_out <-as.data.frame(print(model1_2_jags))
nrow(model1_2_out)
beta_summary_1_2 <- model1_2_out[1:64, ]
phi_summary_1_2 <- model1_2_out[65:72, ]



# SAVE --------------------------------------------------------------------

save(model1_2_jags, file = 'data\\Rdata\\model1_2_jags.Rdata')

save(beta_summary_1_2, file = 'data\\Rdata\\beta_summary_1_2.Rdata')
save(phi_summary, file = 'data\\Rdata\\phi_summary_1_2.Rdata')
