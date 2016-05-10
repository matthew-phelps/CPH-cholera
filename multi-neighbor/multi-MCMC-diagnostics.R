# Author: Matthew Phelps
#Desc: JAGS diagnostics from AWS

# Intro -------------------------------------------------------------------
rm(list = ls())
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Dropbox (Personal)/AWS-Rstudio",
       wd.path <-"/Users/Matthew/Dropbox (Personal)/AWS-Rstudio")
ifelse(grepl("wrz741", getwd()),
       out.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Output/MCMC/",
       out.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/MCMC/")
ifelse(grepl("wrz741", getwd()),
       rdata.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/rdata",
       rdata.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/rdata")
setwd(wd.path)




library(mcmcplots)
library(runjags)
library(coda)
library(ggmcmc)
# LOAD --------------------------------------------------------------------

load("JAGS-rep-5_ext2.Rdata")
load("JAGS-rep-4-ext2.Rdata")
load("JAGS-rep-6-ext-2.Rdata")
setwd(rdata.path)
load("sim-multi-1-data.Rdata")
rm(I_it_daily, N_it, weekly_avg)


model_5_mcmc <- as.mcmc.list(jags_rep_5_ext2)
model_5_ggs <- ggs(model_5_mcmc)

model_4_mcmc <- as.mcmc.list(jags_rep_4_ext2)
model_4_ggs <- ggs(model_4_mcmc)

model_6_mcmc <- as.mcmc.list(jags_rep_6_ext2)
model_6_ggs <- ggs(model_6_mcmc)




# COMBINE MCMC ------------------------------------------------------------

# First divide into sets of 4 chains:

rep5_1 <- divide.jags(jags_rep_5_ext2,
                      which.chains = 1:4)
rep5_2 <- divide.jags(jags_rep_5_ext2,
                      which.chains = 5:8)

mcmc_tot_sep <- combine.mcmc(mcmc.objects =  list(jags_rep_6_ext2,
                                                  rep5_1,
                                                  rep5_2,
                                                  jags_rep_4_ext2), 
                             add.mutate = F,
                             collapse.chains = F)

mcmc_tot <- combine.mcmc(mcmc.objects =  list(jags_rep_6_ext2,
                                              rep5_1,
                                              rep5_2,
                                              jags_rep_4_ext2), 
                         add.mutate = F,
                         collapse.chains = T)

total_ggs <- ggs(mcmc_tot_sep)
# 1 - DIAGNOSTICS ---------------------------------------------------------
mcmcplot(mcmc_tot_sep)
add.summary(jags_rep_4_ext2 )
#mcmcplot(jags_rep_5_ext1)
gelman.plot(model_4_mcmc)
gelman.diag(model_4_mcmc)
mcmcplot(combine.mcmc(jags_rep_6_ext2, collapse.chains = F))
mcmcplot(combine.mcmc(jags_rep_4_ext2, collapse.chains = F))


# 95% CREDIBLE INTERVAL ---------------------------------------------------
int_hpd <- data.frame(HPDinterval(mcmc_tot, 0.95))

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

# 2 - GGS DIAGS -----------------------------------------------------------
setwd(out.path)

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


beta_trace <- ggs_traceplot(model_4_ggs, family = 'beta', simplify = 0) +
  theme(legend.position = 'none')
ggsave(beta_trace, file = "betas-trace.jpg",
       width = 20,
       height = 300,
       units = 'cm',
       dpi = 150,
       limitsize = F)

rhat <- ggs_Rhat(model_4_ggs) + xlab("R_hat")
ggsave(rhat, file = "rhat2.jpg",
       width = 26,
       height = 40,
       units = 'cm',
       dpi = 150,
       limitsize = F)

gelman.plot(model_4_mcmc)
gelman.diag(model_4_mcmc)


ggs_autocorrelation(model_4_ggs, family = "phi") +
  theme_minimal() +
  theme(legend.position = "none")
