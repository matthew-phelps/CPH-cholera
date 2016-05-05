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
setwd(wd.path)



library(mcmcplots)
library(runjags)
library(coda)
library(ggmcmc)
# LOAD --------------------------------------------------------------------
load("multi-JAGS-rep-5.Rdata")
model_1_mcmc <- as.mcmc.list(jags_rep_5_ext1)
model_1_ggs <- ggs(model_1_mcmc)


# 1 - DIAGNOSTICS ---------------------------------------------------------
#add.summary(jags_rep_5_ext1)
#mcmcplot(jags_rep_5_ext1)



# 2 - GGS DIAGS -----------------------------------------------------------
setwd(out.path)

den <- ggs_density(model_1_ggs, family = 'beta') +
  theme(legend.position = 'none')
ggsave(den, file = "betas-density.png",
       width = 26,
       height = 300,
       units = 'cm',
       dpi = 150,
       limitsize = F)


catp <- ggs_caterpillar(model_1_ggs, family = "beta") +
  theme_minimal() +
  theme(legend.position = 'none')
ggsave(catp, file = "betas-catapillar.png",
       width = 26,
       height = 60,
       units = 'cm',
       dpi = 150,
       limitsize = F)


beta_trace <- ggs_traceplot(model_1_ggs, family = 'beta', simplify = .3) +
  theme(legend.position = 'none')
ggsave(beta_trace, file = "betas-trace.jpg",
       width = 126,
       height = 286,
       units = 'cm',
       dpi = 150,
       limitsize = F)

rhat <- ggs_Rhat(model_1_ggs) + xlab("R_hat")
ggsave(rhat, file = "rhat.jpg",
       width = 26,
       height = 40,
       units = 'cm',
       dpi = 150,
       limitsize = F)

ggs_autocorrelation(model_1_ggs, family = "phi") +
  theme_minimal() +
  theme(legend.position = "none")