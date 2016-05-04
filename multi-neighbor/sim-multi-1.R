# Author: Matthew Phelps
# Desc: Simulations from t = 0 and for t + 1 for multi-neighborhood model



# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
rm(list = ls())

library(ggplot2)
library(tidyr)
require(grid)
library(coda)
library(CholeraDataDK)

# LOAD data ---------------------------------------------------------------
load(file = "data/Rdata/sim-multi-1-data.Rdata")
city_obs <- cholera_daily_data[cholera_daily_data$city == "copenhagen", ] # city-wide data
city_obs <- select(city_obs, c(day_index, cases)) # remove un-needed columns


duration <- 5 # In days. "1-2 weeks" from DOI:  10.1038/nrmicro2204
gamma <- 1/duration
loops <- 10 # Has to be the same for both full sum and t+1 sim




# T = 0: SIMULATION-----------------------------------------------------

Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_sim_vect <-     matrix(nrow = Nsteps, ncol = Nquarter)
S_it_est <-       matrix(nrow = Nsteps, ncol = Nquarter)
I_new_mat <- list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))

# Starting values
I_sim_vect[1, ] <- I_it_daily[1]
I_sim_vect[1, c(5, 8, 9)] <- 1 # Init Nyboder with 2 cases
S_it_est[1, ] <- N_it[, 1] # init all S

# Simulate:
ptm <- proc.time()
set.seed(13)
for (z in 1:loops){
  for (t in 1:(Nsteps-1)){
    for(i in 1:Nquarter){
      Lambda_est_pe[t, i] <- S_it_est[t, i] / N_it[i] * sum( betas[, i] * I_sim_vect[t, ] )
      LambdaR[t, i] <- I_sim_vect[t, i] * gamma
      R_new[t, i] <- rpois(1, LambdaR[t, i])
      I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] ) )
      I_sim_vect[t + 1, i] <- max(0, (I_new[t, i] + I_sim_vect[t, i] - R_new[t, i]))
      S_temp <- (S_it_est[t, i]) -    (I_new[t, i]) / (phi)
      S_it_est[t + 1, i] <- max(0, S_temp)
    }
  }
  I_new_mat[[z]] <- data.frame(I_new)
  I_new_mat[[z]]$sim_sum <- z
}
proc.time() - ptm




# T = 0 : DATA RESHAPE --------------------------------------------------------------

# Simulated daily incidence to long format:
x <- do.call(rbind.data.frame, I_new_mat) # merge all sims to 1 df
colnames(x) <- c(q_names, "sim_num")
x$day <- 1:112
I_simulated <- gather(x, quarter, I_simulated, 1:9) # wide to long
I_it_daily <- data.frame(I_it_daily)
colnames(I_it_daily) <- q_names
I_it_daily$day <- 1:112
I_obs_lng <- gather(I_it_daily, quarter, I_obs, 1:9)


# T = 0: PLOT PANEL QUARTERS-------------------------------------------------------------

sim1_plot <- ggplot() + 
  geom_line(data = I_simulated, 
            alpha = 0.01,
            aes(x = day, y = I_simulated,
                                    group = interaction(quarter, sim_num),
                                    color = quarter)) +
  geom_line(data = I_obs_lng, aes(x = day,
                                   y = I_obs, group = quarter)) +
  facet_wrap(~quarter) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Simulation from t = 0")
sim1_plot
ggsave(filename = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/multi/Sim-1-quarter.png',
       plot = sim1_plot,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)


# T = 0: Citywide ---------------------------------------------------------

city_sim <- data.frame(rowSums(x[, 1:9]))
colnames(city_sim) <- "I_simulated"
city_sim$day <- x$day
city_sim$sim_num <- x$sim_num

city1_plot <- ggplot() +
  geom_line(data = city_sim,
            aes(x = day, y = I_simulated, group = sim_num),
            color = "green",
            alpha = 0.01) +
  geom_line(data = city_obs,
            aes(x = day_index, y = cases)) +
  ylab("Daily incidence") +
  theme_minimal() +
  ggtitle("Simulated from t = 0 \naggregated to city level")


ggsave(filename = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/multi/Sim-1-citywide.png',
       plot = city1_plot,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)


###############################################################################
###############################################################################
###############################################################################


# T + 1: SIMULATION-----------------------------------------------------

Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_sim_vect <-     matrix(nrow = Nsteps, ncol = Nquarter)
S_plus1_mat <-       matrix(nrow = Nsteps, ncol = Nquarter)
I_new_plus1 <- list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))

# Starting values
I_sim_vect[1, ] <- I_it_daily[1]
I_sim_vect[1, c(5, 8, 9)] <- 1 # Init St.A.V & Ø + Nyb with cases
S_it_est[1, ] <- N_it[, 1] # init all S

# Simulate:
# "I_it_daily" is the daily "observed" incidence.
ptm <- proc.time()
set.seed(13)
for (z in 1:loops){
  for (t in 1:(Nsteps-1)){
    for(i in 1:Nquarter){
      Lambda_est_pe[t, i] <- S_it_est[t, i] / N_it[i] * sum( betas[, i] * I_sim_vect[t, ] )
      LambdaR[t, i] <- I_sim_vect[t, i] * gamma
      R_new[t, i] <- rpois(1, LambdaR[t, i])
      I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] ) )
      I_sim_vect[t + 1, i] <- max(0, (I_it_daily[t, i] + I_sim_vect[t, i] - R_new[t, i]))
      S_temp <- (S_plus1_mat[t, i]) -    (I_new[t, i]) / (phi)
      S_plus1_mat[t + 1, i] <- max(0, S_temp)
    }
  }
  I_new_plus1[[z]] <- data.frame(I_new)
  I_new_plus1[[z]]$sim_sum <- z
}
proc.time() - ptm

# T + 1 : DATA RESHAPE --------------------------------------------------------------

# Simulated daily incidence to long format:
z <- do.call(rbind.data.frame, I_new_plus1) # merge all sims to 1 df
colnames(z) <- c(q_names, "sim_num")
z$day <- 1:112
I_simulated_plus1 <- gather(z, quarter, I_simulated, 1:9) # wide to long
I_it_daily <- data.frame(I_it_daily)
colnames(I_it_daily) <- q_names
I_it_daily$day <- 1:112
I_obs_lng <- gather(I_it_daily, quarter, I_obs, 1:9)



# T + 1: PLOT PANEL QUARTERS-------------------------------------------------------------

sim1_plus1 <- ggplot() + 
  geom_line(data = I_simulated_plus1, 
            alpha = 0.5,
            aes(x = day, y = I_simulated,
                group = interaction(quarter, sim_num),
                color = quarter)) +
  geom_line(data = I_obs_lng, aes(x = day,
                                  y = I_obs, group = quarter)) +
  facet_wrap(~quarter) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Simulation t + 1")
sim1_plus1

ggsave(filename = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/multi/Sim-1-plus1-quarter.png',
       plot = sim1_plus1,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)


# T + 1: Citywide ---------------------------------------------------------

city_sim_plus1 <- data.frame(rowSums(z[, 1:9]))
colnames(city_sim_plus1) <- "I_simulated"
city_sim_plus1$day <- z$day
city_sim_plus1$sim_num <- z$sim_num

city_plus1_plot <- ggplot() +
  geom_line(data = city_sim_plus1,
            aes(x = day, y = I_simulated, group = sim_num),
            color = "green",
            alpha = 0.5) +
  geom_line(data = city_obs,
            aes(x = day_index, y = cases)) +
  ylab("Daily incidence") +
  theme_minimal() +
  ggtitle("Simulated t + 1\naggregated to city level")


ggsave(filename = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/multi/Sim-1-citywide.png',
       plot = city1_plot,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)
