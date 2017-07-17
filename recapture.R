rm(list= ls())
library(coda)
library(dplyr)
library(runjags)
source("Data-3-combine quarters.R")
source("functions/sim-data-prep-functions.R")
source("functions/SimulationAndPlots.R")
source("Data-4-prepare-JAGS.R")
source("multi-neighbor/sim-model-5-data-prep.R")
load(file = "Data/Rdata/jags_m5_ls-new.Rdata")
#  ------------------------------------------------------------------------

# Treat each of the 10 realizations as a separate model for the mcmcPrep
# function. This will give a warning because we are not collapsing the 10
# epidemics together, but only the 4 chains within each epidemic are collapsed
epi10 <- lapply(jags_m5_ls, mcmcPrep, q_names = q_names)

epi10_sm <- lapply(epi10, smMcmc)


# For each realization, simulate from the MCMC created from data from that
# relization
n_loops <- 200
seed <- 13
sim_recap <- lapply(epi10_sm, function(x) {
  SimPlusOne(loops=n_loops, 
             I_reps = I_reps, N_it = N_it,
             betas_95hpd = x$betas_95hpd,
             phi_95hpd = x$phi_95hpd,
             gamma_95hpd = x$gamma_95hpd,
             seed =13)
})

sim_recap_data <- lapply(sim_recap, SimDataToPlot)

sim_recap_summary <- lapply(sim_recap_data, function(x){
  x %>%
    rmNonOutbreaks(min_cases = 0) %>%
    SimCI()
})

# Plot to make sure simulations are working
SimPlot(observed_data = I_reps_plot,
        ci = sim_recap_summary[[1]]$sim_summary,
        ribbon = TRUE)


spreadSimData <- function(x) {
  x$sim_summary %>%
    select(-`2.5%`, -`97.5%`, -median) %>%
    spread(., key = quarter, value = avg) %>%
    select(-day) %>%
    as.matrix()
}

I_sim <- lapply(sim_recap_summary, spreadSimData)

dataList <- list()
num_reps <- length(sim_recap_summary)
Nsteps <- 112
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


