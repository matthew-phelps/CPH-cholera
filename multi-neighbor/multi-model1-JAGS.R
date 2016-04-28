# Author: Matthew Phelps
#Desc: JAGS model of CPH 1853 - multineighborhood.

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
library(ggplot2)
options(mc.cores = (parallel::detectCores()))


# LOAD -------------------------------------------------------
rm(list = ls())
load(file = "data/Rdata/multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
model_jags_list_1 <- list()
dataList <- list()
num_reps <- length(I_reps)
num_reps <- 1
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[2]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}


# Model 1 -----------------------------------------------------------------

for (reps in 1:num_reps){
  # JAGS
  # Run the JAGS models 10 times. Each run fits all quarters together
  # Each [[reps]] is one JAGS model with 5 chains
  set.seed(13) # Not sure if this does anything in current set-up
  model_jags_list_1[[reps]] <- run.jags(model = '/Users/Matthew/GitClones/RCodes/multi-neighbor/JAGS-multi-quarter-1.stan',
                                        method = 'parallel',
                                        monitor = c('beta', 'phi'),
                                        data = dataList[[reps]],
                                        n.chains = 4,
                                        adapt = 1000,
                                        burnin = 1000,
                                        sample = 2000,
                                        thin = 4,
                                        plots = T)
  
}


# SAVE --------------------------------------------------------------------
save(model_jags_list_1, file = "Data/Rdata/multi-model-1-jags-list.Rdata")
save(dataList, file = "Data/Rdata/model-1-dataList.Rdata")
# # JAGS DIAGNOSTICS -
add.summary(model_jags_list_1[[1]])
mcmcplot(model_jags_list_1[[1]])

# View each chain individually
model_1_mcmc <- as.mcmc.list(model_jags_list_1[[1]])
model_1_ggs <- ggs(model_1_mcmc)

ggs_traceplot(model_1_ggs, family = 'beta', simplify = .3) +
    theme(legend.position = 'none')

ggs_density(model_1_ggs, family = 'beta') +
  theme(legend.position = 'none')

ggs_autocorrelation(model_1_ggs, family = "beta") +
  theme_minimal() +
  theme(legend.position = "none")

# POOL POSTERIORS -------------------
# Pool all 5 chains in each JAGS run:
mcmc_comb_chains <- list()
for (reps in 1:num_reps){
  mcmc_comb_chains[[reps]] <- combine.mcmc(model_jags_list_1[[reps]])
}


# Combine all JAGS run into one huge mcmc chain:

mcmc_total <- data.frame(combine.mcmc(mcmc_comb_chains))
plot(mcmc_total$phi)
mcmc_length <- as.character(nrow(mcmc_total))
rep_num <- length(model_jags_list_1)
sub_title <- paste("MCMC length = ", mcmc_length,
                   " / Num. realizations =", rep_num)


beta_posterior <- ggplot(data = mcmc_total, aes(x = beta.1.2.)) +
  geom_density(fill = "darkred", alpha = 0.7) +
  theme_minimal() +
  ggtitle(bquote(atop("Beta pooled posterior",
                      atop(italic(.(sub_title)), ""))))



phi_posteriors <- ggplot(data = mcmc_total, aes(x = phi)) +
  geom_density(fill = "darkblue", alpha = 0.55) +
  theme_minimal() +
  ggtitle(bquote(atop("Phi pooled posterior",
                      atop(italic(.(sub_title)), ""))))


# SAVE --------------------------------------------------------------------

save(mcmc_total, file = "Data\\Rdata\\mcmc_total.Rdata")
ggsave(beta_posterior, file = "Output\\MCMC\\beta_posteriors.png")
ggsave(phi_posteriors, file = "Output\\MCMC\\phi_posteriors.png")




# Model 2 -----------------------------------------------------------------


model_jags_list_2 <- list()
for (reps in 1:num_reps){
  # JAGS
  # Run the JAGS models 10 times. Each run fits all quarters together
  # Each [[reps]] is one JAGS model with 5 chains
  set.seed(13) # Not sure if this does anything in current set-up
  model_jags_list_2[[reps]] <- run.jags(model = '/Users/Matthew/GitClones/RCodes/multi-neighbor/JAGS-multi-quarter-2.stan',
                                        method = 'parallel',
                                        monitor = c('beta', 'phi'),
                                        data = dataList[[reps]],
                                        n.chains = 4,
                                        adapt = 1000,
                                        burnin = 1000,
                                        sample = 1000,
                                        thin = 3,
                                        plots = T)
  
}

add.summary(model_jags_list_2[[1]])

mcmcplot(model_jags_list_2[[1]])




# Model 3-----------------------------------------------------------------


model_jags_list_3 <- list()
for (reps in 1:num_reps){
  # JAGS
  # Run the JAGS models 10 times. Each run fits all quarters together
  # Each [[reps]] is one JAGS model with 5 chains
  set.seed(13) # Not sure if this does anything in current set-up
  model_jags_list_3[[reps]] <- run.jags(model = '/Users/Matthew/GitClones/RCodes/multi-neighbor/JAGS-multi-quarter-2.stan',
                                        method = 'parallel',
                                        monitor = c('beta', 'phi'),
                                        data = dataList[[reps]],
                                        n.chains = 4,
                                        adapt = 1000,
                                        burnin = 1000,
                                        sample = 1000,
                                        thin = 3,
                                        plots = T)
  
}

add.summary(model_jags_list_3[[1]])

mcmcplot(model_jags_list_3[[1]])
