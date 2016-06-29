# Author: Matthew Phelps
#Desc: JAGS model with independent internal betas and 1 shared external beta

# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")


amazon <- F

ifelse(amazon == T,
       data.path <- "~/Dropbox/AWS-Rstudio",
       data.path <- data.path)
setwd(data.path)
library(plyr)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
# library(ggmcmc)
# library(ggplot2)
options(mc.cores = 4)
rm(amazon)

# LOAD -------------------------------------------------------

load(file = "multi-model1-data-prep.Rdata")


# JAGS -------------------------------------------------------------
# Save in list form to pass to JAGS
jags_m5_ls <- list()
dataList <- list()
num_reps <- length(I_reps)
num_reps <- 1
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_reps[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}

# Model 1 -----------------------------------------------------------------

# JAGS
# Run the JAGS models for each iteration in a separate instance on AWS. Run 8 chains in each
setwd(model.path)
for (reps in 1:num_reps){
  set.seed(13) # Not sure if this does anything in current set-up
  jags_m5_ls[[reps]] <- run.jags(model = 'JAGS-multi-quarter-5.stan',
                                 method = 'parallel',
                                 monitor = c("beta", 'phi'),
                                 modules = "glm",
                                 data = dataList[[reps]],
                                 n.chains = 4,
                                 adapt = 1e3,
                                 burnin = 1e4,
                                 sample = 1e4,
                                 thin = 1,
                                 plots = T)
}


add.summary(jags_m5_ls[[reps]])
mcmcplot(combine.mcmc(jags_m5_ls[[reps]], collapse.chains = F))


setwd(data.path)
save(jags_m5_ls, file = "jags_m5_ls.Rdata")





#################################################
#################################################
#################################################
#################################################

y <- extend.jags(model_jags_list_1[[reps]],
            method = "parallel",
            adapt = 500,
            sample = 100,
            thin = 35)
add.summary(y)
mcmcplot(x)
# SAVE --------------------------------------------------------------------
load("/Users/Matthew/Dropbox (Personal)/AWS-Rstudio/multi-model-1-jags-list-large.Rdata")

save(x, file = "Data/Rdata/multi-model-1-jags-list.Rdata")
save(dataList, file = "Data/Rdata/multi-model-1-dataList.Rdata")
# # JAGS DIAGNOSTICS -
add.summary(model_jags_list_1[[1]])
mcmcplot(model_jags_list_1[[1]])

# View each chain individually
model_1_mcmc <- as.mcmc.list(model_jags_list_1[[1]])
model_1_mcmc <- as.mcmc.list(jags_rep_5)
# Label parameters for ggs object
param_names <- data.frame(
  Parameter = names((model_1_mcmc[[1]][1,])),
  Label = c("Nyb", "S.An.Oe->Nyb", "S.An.V->Nyb",
            "Nyb->S.An.Oe", "S.An.Oe", "S.An.V->S.An.Oe",
            "Nyb->S.An.V", "S.An.Oe->S.An.V", "S.An.V",
            "phi")
  )
model_1_ggs <- ggs(model_1_mcmc)
model_1_betas <- ggs(model_1_mcmc, family = "beta")

beta_trace <- ggs_traceplot(model_1_ggs, family = 'beta', simplify = .3) +
    theme(legend.position = 'none')
ggsave(beta_trace, file = "/Users/Matthew/Desktop/beta_trace.png",
       width = 126,
       height = 286,
       units = 'cm',
       dpi = 150,
       limitsize = F)
getwd()

x$density


den <- ggs_density(model_1_ggs, family = 'beta') +
  theme(legend.position = 'none')
ggsave(den, file = "/Users/Matthew/Desktop/beta_den.png",
       width = 26,
       height = 300,
       units = 'cm',
       dpi = 150,
       limitsize = F)

rhat <- ggs_Rhat(model_1_ggs) + xlab("R_hat")
ggsave(rhat, file = "/Users/Matthew/Desktop/rhat.png",
       width = 26,
       height = 300,
       units = 'cm',
       dpi = 150,
       limitsize = F)


ggs_autocorrelation(model_1_ggs, family = "phi") +
  theme_minimal() +
  theme(legend.position = "none")

ggs_caterpillar(model_1_ggs, family = "beta") +
  theme_minimal() +
  theme(legend.position = 'none')

ggs_caterpillar(model_1_ggs, family = "phi") +
  theme_minimal() +
  theme(legend.position = 'none')

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



