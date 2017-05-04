# Author: Matthew Phelps
#Desc: See if we can re-caputre the parameters used to simulate an outbreak
#using the same fittign procedures

# Intro -------------------------------------------------------------------
graphics.off()
library(tidyr)
library(plyr)
library(coda)
library(parallel)
library(runjags)
library(rjags)
library(mcmcplots)
# library(ggmcmc)
# library(ggplot2)
options(mc.cores = 4)

# LOAD -------------------------------------------------------

load(file = "Data/Rdata/multi-model1-data-prep.Rdata")
load(file =  "data/Rdata/sim5_step_data.Rdata")

# DATA PREP -------------------------------------------------------------
# Save in list form to pass to JAGS

head(sim5_step_data)

I_reps[[1]]
spreadQuarters <- function(x){
  x[, 2:4] %>%
    spread(key=quarter, I_simulated) %>%
    dplyr::select(-day) %>%
    as.matrix()
}
I_sim <- sim5_step_data %>%
  # only doing 10 re-capture simulations, so only need data from first 10 sims
  dplyr::filter(sim_num <=10) %>%
  split(f=.$sim_num) %>%
  lapply(spreadQuarters)


checkDataMunging <- function(x) {
  # Not sure if I spread data properly, so check:
  if(all(colSums(x) - colSums(I_reps[[1]]) > 100)){
    return("warning: difference > 100")
  } else {
    return("checks out")
  }
}

lapply(I_sim, checkDataMunging)

dataList <- list()
num_reps <- length(I_reps)
for (reps in 1:num_reps){
  dataList[[reps]] <- list(N_i_daily = N_pop[, 2],
                           I_incidence=I_sim[[reps]],
                           Nsteps=Nsteps,
                           Nquarter = Nquarter)
}


# JAGS --------------------------------------------------------------------
# Run jags on the simulate data

re_capture_m5 <- list()
for(i in seq_len(reps)){
  set.seed(13) # Not sure if this does anything in current set-up
  print(reps)
  print(Sys.time())
  re_capture_m5[[i]] <- autorun.jags(model = 'JAGS/JAGS-multi-quarter-5.stan',
                                        method = 'parallel',
                                        monitor = c("beta", 'phi', 'gamma_b'),
                                        modules = "glm",
                                        data = dataList[[reps]],
                                        n.chains = 4,
                                        adapt = 1e3,
                                        startburnin = 2e4,
                                        startsample = 5e3,
                                        thin = 2,
                                        plots = T)
}
save(re_capture_m5, file = "Data/Rdata/jags_recapture_m5.Rdata")

# # Get summary table
# jags_summary <- data.frame(add.summary(re_capture_m5[[10]]$summaries))
# add.summary(re_capture_m5[[10]])
# # Check that no prsf is higher than our 1.02 cutoff value
# max(jags_summary$psrf)
# which.max(jags_summary$psrf)
