# Author: Matthew Phelps
#Desc: JAGS diagnostics from model 5b

# Intro -------------------------------------------------------------------
rm(list = ls())
library(mcmcplots)
library(runjags)
library(coda)
library(ggmcmc)
source("Data-3-combine quarters.R")
source("multi-neighbor/sim-data-prep-functions.R")
# LOAD --------------------------------------------------------------------

load("Data/Rdata/jags_m5_ls-new.Rdata") # Load the JAGS output to do diagnostics on
load("Data/Rdata/sim-model-1-data-1.Rdata")
# load("sim-model-5-data-b.Rdata") # Not sure why this is loaded 
# rm(I_it_daily, N_it, weekly_avg, y)
# gc()

# Collapse chains within each imputation. Plot this to see if imputations are
# diff from each other
m5_mcmc_10 <- lapply(jags_m5_ls, combine.mcmc, collapse.chains = T)
mcmcplot((jags_m5_ls[[1]]$mcmc), parms = c("phi", "gamma_b"),
                filename = "phi-mcmc", extension = "html",
         heading = "Model 5-1: phi & gamma mcmc plots")
mcmcplot((jags_m5_ls[[3]]$mcmc), parms = c("phi", "gamma_b"),
         filename = "phi-mcmc", extension = "html",
         heading = "Model 5-3: phi & gamma mcmc plots")

x <- combChains(jags_m5_ls, testing = FALSE)

mcmcplot((x), parms = c("phi", "gamma_b"),
         filename = "phi-mcmc", extension = "html",
         heading = "Model 5-1: phi & gamma mcmc plots")
head(x)
plot((m5_mcmc_10[[1]][,1]))

rm(jags_m5_ls_b)

phi
# Collapse everything into one huge mcmc object
m5_mcmc <- combine.mcmc(m5_mcmc)

param_avg <- data.frame(colMeans(m5_mcmc))


model_5_mcmc <- lapply(jags_m5_ls_b, as.mcmc.list)
model_5_ggs <- lapply(ggs(model_5_mcmc)

model_4_mcmc <- as.mcmc.list(jags_rep_4_ext4)
model_4_ggs <- ggs(model_4_mcmc)

model_6_mcmc <- as.mcmc.list(jags_rep_6_ext4)
model_6_ggs <- ggs(model_6_mcmc)


x$density <- add.summary(jags_rep_5_ext3)
names(jags_rep_5_ext3)
jags_rep_5_ext3$density
# COMBINE MCMC ------------------------------------------------------------

# First divide into sets of 4 chains:

mcmc_tot_sep <- combine.mcmc(mcmc.objects =  jags_list,
                             add.mutate = F,
                             collapse.chains = F)

mcmc_tot <- combine.mcmc(mcmc.objects =  jags_list, 
                         add.mutate = F,
                         collapse.chains = T)

mcmc_tot <- combine.mcmc(mcmc.objects =  jags_list, 
                         add.mutate = F,
                         collapse.chains = T)


mcmc_tot_drop_6 <- combine.mcmc(mcmc.objects =  jags_list_drop_6, 
                         add.mutate = F,
                         collapse.chains = T)

total_ggs <- ggs(mcmc_tot_sep)



# 95% CREDIBLE INTERVAL ---------------------------------------------------
int_hpd <- data.frame(HPDinterval(mcmc_tot, 0.95))
int_hpd_drop_6 <- data.frame(HPDinterval(mcmc_tot_drop_6, 0.95))
rep_4hpd <- data.frame(HPDinterval(model_4_mcmc))
rep_5hpd <- data.frame(HPDinterval(model_5_mcmc))
rep_6hpd <- data.frame(HPDinterval(model_6_mcmc))
min(rep_6hpd[22, ])
min(rep_5hpd[22,])
min(rep_4hpd[22,])

mcmc_tot_df <- (t(mcmc_tot))
n_draw <- ncol(mcmc_tot_df)
n_par <- nrow(int_hpd)

post_draws_full <- (matrix(data = NA, nrow = n_par , ncol = n_draw))


# take 1000 draws from posterior range
for (i in 1:n_par) {
  # instead of looping over every element in df, capitalize on R's vectorized
  # operations: http://goo.gl/eiCciz
  post_draws_full[i, ] <- ifelse(mcmc_tot_df[i, ] > int_hpd[i, 1],
                                 mcmc_tot_df[i, ],
                                 NA)
  post_draws_full[i, ] <- ifelse(post_draws_full[i, ] < int_hpd[i, 2],
                                 mcmc_tot_df[i, ],
                                 NA)
}

# Remove any mcmc draws that had ANY parameter outside the 95% CI:
# http://goo.gl/aXi6Vl
post_draws <- post_draws_full[, colSums(is.na(post_draws_full)) == 0]

n_samples <- ncol(post_draws)

# SAVE MCMC 95% CI:
setwd(rdata.path)
save(post_draws, file = "post_draws.Rdata")
save(int_hpd, file = "int_hpd.Rdata")
save(int_hpd_drop_6, file = "int_hpd_drop_6.Rdata")

# 1 - HTML DIAGNOSTICS ---------------------------------------------------------
mcmcplot(mcmc_tot_sep)
add.summary(jags_rep_4_ext3 )

gelman.plot(model_4_mcmc)
gelman.diag(model_4_mcmc)
mcmcplot(combine.mcmc(jags_rep_6_ext3, collapse.chains = F))
mcmcplot(combine.mcmc(jags_rep_4_ext3, collapse.chains = F))
mcmcplot(combine.mcmc(jags_rep_5_ext3, collapse.chains = F))


# Log transform trace plots
log_rep4 <- jags_rep_4_ext3
log_rep5 <- jags_rep_5_ext3
log_rep6 <- jags_rep_6_ext3
nchains <- length(jags_rep_4_ext3$mcmc)
for (i in 1:nchains){
  log_rep4$mcmc[[i]] <- log(jags_rep_4_ext3$mcmc[[i]])
  log_rep5$mcmc[[i]] <- log(jags_rep_5_ext3$mcmc[[i]])
  log_rep6$mcmc[[i]] <- log(jags_rep_6_ext3$mcmc[[i]])
}
mcmcplot(combine.mcmc(log_rep4, collapse.chains = F))
mcmcplot(combine.mcmc(log_rep5, collapse.chains = F))
mcmcplot(combine.mcmc(log_rep6, collapse.chains = F))


# 2 - GGS DIAGS -----------------------------------------------------------
setwd(out.path)
mcmcplot(jags_rep_4_ext3)

den <- ggs_density(model_4_ggs, family = 'beta') +
  theme(legend.position = 'none')
ggsave(den, file = "betas-density-4.png",
       width = 26,
       height = 300,
       units = 'cm',
       dpi = 150,
       limitsize = F)


catp <- ggs_caterpillar(model_4_ggs, family = "beta") +
  theme_minimal() +
  theme(legend.position = 'none')
ggsave(catp, file = "betas-catapillar.png",
       width = 26,
       height = 60,
       units = 'cm',
       dpi = 150,
       limitsize = F)


beta_trace <- ggs_traceplot(model_4_ggs, family = 'beta') +
  theme(legend.position = 'none')
ggsave(beta_trace, file = "betas-trace.jpg",
       width = 20,
       height = 300,
       units = 'cm',
       dpi = 150,
       limitsize = F)

ggs_Rhat(model_4_ggs) + xlab("Rep 4 R_hat") + ggtitle("Rep 4")
ggs_Rhat(model_5_ggs) + xlab("Rep 5 R_hat") + ggtitle("Rep 5")
ggs_Rhat(model_6_ggs) + xlab("Rep 6 R_hat") + ggtitle("Rep 6")

ggsave(rhat, file = "rhat2.jpg",
       width = 26,
       height = 40,
       units = 'cm',
       dpi = 150,
       limitsize = F)

gelman.plot(model_6_mcmc)
gelman.diag(model_4_mcmc)


ggs_autocorrelation(model_4_ggs, family = "phi") +
  theme_minimal() +
  theme(legend.position = "none")
