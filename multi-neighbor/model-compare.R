rm(list = ls())
library(magrittr)
load(file =  "data/Rdata/sim5_full_data.Rdata")
source("multi-neighbor/sim-model-5-data-prep.R")
source("multi-neighbor/SimulationAndPlots.R")
rm(mcmc_out)


min_day21 <- I_reps_plot %>%
  dplyr::filter(quarter == "St. Annae Vester" &
                  rep == 1 & day <21) %$%
  sum(I_new)
x <- spread(sim5_full_data, sim_num, I_simulated)

# Test simulations if they have taken off by day 21
col_inx <- 35 < x %>%
  dplyr::filter(quarter == "St. Annae Vester" &
                  day <21) %>%
  dplyr::select(-quarter, -day) %>%
  colSums()

sim_inx <- col_inx %>%
  names() %>%
  as.numeric() %>%
  as.data.frame()%>%
  mutate(inx = col_inx) %>%
  `colnames<-` (c("sim_num", "include"))
sim5_filtered <- dplyr::left_join(sim5_full_data, sim_inx, by = "sim_num")
