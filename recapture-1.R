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
n_realizations <- length(epi10_sm)

# For each realization, simulate from the MCMC created from data from that
# relization
n_loops <- 2
seed <- 13
sim_recap <- lapply(1:n_realizations, function(i) {
  # browser()
  x <- epi10_sm[[i]]
  SimPlusOneRecapture(loops=n_loops, 
                      I_reps = I_reps, N_it = N_it,
                      betas_95hpd = x$betas_95hpd,
                      phi_95hpd = x$phi_95hpd,
                      gamma_95hpd = x$gamma_95hpd,
                      seed =13, realization_number = i)
})

# 
# # Plot to make sure simulations are working
# SimPlot(observed_data = I_reps_plot,
#         ci = sim_recap_summary[[3]]$sim_summary,
#         ribbon = TRUE)


# spreadSimData <- function(x) {
#   x$sim_summary %>%
#     select(-`2.5%`, -`97.5%`, -median) %>%
#     spread(., key = quarter, value = avg) %>%
#     select(-day) %>%
#     as.matrix() %>%
#     round()
# }
# 
# I_sim <- lapply(sim_recap_summary, spreadSimData)
# 



# JAGS --------------------------------------------------------------------
# Run jags on the simulate data
dataList <- list()
Nsteps <- 112
length(z)
options(mc.cores = 4)
re_capture_m5 <- list()
re_capture_m5 <- lapply(sim_recap, function(x){
  # browser()
  for (loop in seq_len(n_loops)){
    dataList[[loop]] <- list(N_i_daily = N_pop[, 2],
                             I_incidence=as.matrix(x$I_new_plus1[[loop]]),
                             Nsteps=Nsteps,
                             Nquarter = Nquarter)
  }
  x <- list()
  for(i in seq_len(n_loops)){
    set.seed(13) # Not sure if this does anything in current set-up
    print(i)
    print(Sys.time())
    x[[i]] <- autorun.jags(model = 'JAGS/JAGS-multi-quarter-5.stan',
                           method = 'parallel',
                           monitor = c("beta", 'phi', 'gamma_b'),
                           modules = "glm",
                           data = dataList[[i]],
                           n.chains = 4,
                           adapt = 1e3,
                           startburnin = 2e4,
                           startsample = 5e3,
                           thin = 2,
                           plots = T)
  }
  return(x)
})


# For each simulation collapse the 4 chains, then for each realizations,
# collapse the simulations together and take the HPD of the final collapses mcmc
# obj.
hpd10 <- lapply(re_capture_m5, function(x){
  combine.mcmc(x) %>%
    combine.mcmc() %>%
    HPDinterval()
})


re_capture_list <- list(re_capture_m5 = re_capture_m5,
                        epi10 = epi10,
                        hpd10 = hpd10)

save(re_capture_list, file = "Data/Rdata/re_capture_list.Rdata")