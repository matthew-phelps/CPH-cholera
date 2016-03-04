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
library(ggplot2)
options(mc.cores = (parallel::detectCores() ))


# LOAD -------------------------------------------------------
rm(list = ls())
load(file = "data\\Rdata\\model-1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
model_1_jags_list <- list()
dataList <- list()
num_reps <- length(I_reps)
ptm <- proc.time()
for (reps in 1:num_reps){
dataList[[reps]] <- list(N_i_daily = N_i_daily,
                 I_incidence=I_reps[[reps]],
                 Nsteps=Nsteps)


# JAGS
set.seed(13) # Not sure if this does anything in current set-up
model_1_jags_list[[reps]] <- run.jags(model = 'Rcodes\\stan-model_Fitting-one-replicate.stan',
                           method = 'parallel',
                           monitor = c('beta', 'phi'),
                           data = dataList[[reps]],
                           n.chains = 5,
                           adapt = 1000,
                           burnin = 100000,
                           sample = 150000,
                           thin = 3,
                           plots = T)

}
proc.time() - ptm

# SAVE --------------------------------------------------------------------
save(model_1_jags_list, file = "Data\\Rdata\\model-1-jags-list")

# # JAGS DIAGNOSTICS -
# print(model_1_jags_list)

# POOL POSTERIORS ---------------------------------------------------------
# Pool all 5 chains in each JAGS run:
mcmc_comb_chains <- list()
for (reps in 1:num_reps){
  mcmc_comb_chains[[reps]] <- combine.mcmc(model_1_jags_list[[reps]])
}

# Combine all JAGS run into one huge mcmc chain:

mcmc_total <- data.frame(combine.mcmc(mcmc_comb_chains))

mcmc_length <- as.character(nrow(mcmc_total))
rep_num <- length(model_1_jags_list)
sub_title <- paste("MCMC length = ", mcmc_length,
                   " / Num. realizations =", rep_num)

beta_posterior <- ggplot(data = mcmc_total, aes(x = beta)) +
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



