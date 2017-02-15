
graphics.off()
rm(list = ls())

library(tidyverse)
require(grid)
library(coda)
library(CholeraDataDK)

# LOAD data ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
Nweeks <- Nsteps
load("Data/Rdata/sim-model-1-data.Rdata")


# CITY LEVEL DATA ---------------------------------------------------------
city_obs <- cholera_daily_data %>%
  dplyr::filter(city == "copenhagen") %>%
  dplyr::select(c(day_index, cases))


city_obs_2 <- data.frame(t(I_it))
colnames(city_obs_2) <- q_names
city_obs_2$day_index <- seq(from = 7, to = (Nweeks )* 7, length.out = Nweeks)
city_obs_2$I <- rowSums(city_obs_2[, 1:9]) / 7
quarter_sums <- dplyr::select(combined, c(quarter, week.id, cum.sick))
quarter_sums <- quarter_sums[quarter_sums$week.id == 15, c(1, 3)]



# GLOBAL VARIABLES -------------------------------------------------------------------

gamma <- 1/5


# T + 1: PREP -----------------------------------------------------


# T + 1: SIMULATION -----------------------------------------------------
# "I_it_daily" is the daily "observed" incidence.
ptm <- proc.time()

sim_t_plus_one <- function(loops){
  set.seed(13)
  #browser()
  Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
  LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
  R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_prev_vect <-     matrix(nrow = Nsteps, ncol = Nquarter)
  S_plus1_mat <-    matrix(nrow = Nsteps, ncol = Nquarter)
  I_new_plus1 <- list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))
  store_prev <- list()
  store_S <- list() 
  # Attributable infections variables:
  Lambda_quart<-lapply(1:Nquarter,
                       function(x) data.frame(matrix(NA,
                                                     nrow=Nsteps,
                                                     ncol=Nquarter)))
  I_attr <-lapply(1:loops,
                  function(x) data.frame(matrix(NA,
                                                nrow=Nquarter,
                                                ncol=Nquarter)))
  
  # Starting values
  I_prev_vect[1, ] <- I_it_daily[[1]][1,]
  I_prev_vect[1, c(5, 8, 9)] <- 1 # Init St.A.V & Ø + Nyb with cases
  S_plus1_mat[1, ] <- N_it[, 1] # init all S
  
  # To sample from a random realization of the epidemic for each simulation:
  
  rand_realization <- sample(1:10, loops, replace = TRUE)
  
  for (z in 1:loops){
    for (t in 1:(Nsteps-1)){
      for(i in 1:Nquarter){ Lambda_est_pe[t, i] <- S_plus1_mat[t, i] / N_it[i] * sum(betas[, i] * I_prev_vect[t, ])
      LambdaR[t, i] <- I_prev_vect[t, i] * gamma
      
      R_temp <- LambdaR[t, i]
      R_new[t, i] <- min(R_temp, I_prev_vect[t, i]) # no more recovereds than infected
      
      I_data <- I_it_daily[[rand_realization[z]]][t, i] # Observed data
      
      I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] * phi ) )
      I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_data / phi  - R_new[t, i]))
      
      S_temp <- S_plus1_mat[t, i] -    I_data / phi # Should be I_it_daily instead?
      S_plus1_mat[t + 1, i] <- max(0, S_temp)
      
      }
    }
    # For each quarter: store sum of infections attributed to each quarter over
    # all time-steps
    #  browser()
    store_prev[[z]] <- I_prev_vect
    store_S[[z]] <- S_plus1_mat
    I_new_plus1[[z]] <- data.frame(I_new)
    I_new_plus1[[z]]$sim_sum <- z
  }
  list(I_new_plus1 = I_new_plus1,
       store_prev = store_prev, store_S = store_S)
}
sim1 <- sim_t_plus_one(loops=500)





# Simulated daily incidence to long format:
y <- do.call(rbind.data.frame, sim1$I_new_plus1) # merge all sims to 1 df
y[is.na(y)] <- 0
colnames(y) <- c(q_names, "sim_num")
y$day <- 1:112
I_simulated_plus1 <- gather(y, quarter, I_simulated, 1:9) # wide to long
I_obs_df <- data.frame(I_it_daily)
colnames(I_obs_df) <- q_names
I_obs_df$day <- 1:112
I_obs_lng <- gather(I_obs_df, quarter, I_obs, 1:9)



# T + 1: PLOT PANEL QUARTERS-------------------------------------------------------------

sim1_plus1 <- ggplot() + 
  geom_line(data = I_simulated_plus1, 
            alpha = 0.01,
            aes(x = day, y = I_simulated,
                group = interaction(quarter, sim_num),
                color = quarter)) +
  geom_line(data = combined, aes(x = (week.id+1) * 7,
                                 y = sick.total.week/7, group = quarter)) +
  facet_wrap(~quarter) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Sim: t + 1")
sim1_plus1


# SSAVe -------------------------------------------------------------------
ggsave(filename = 'Plot-output/Sim-m1-tplus1.png',
       plot = sim1_plus1,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)
save(I_att, file = "Data/Rdata/Attributable-cases.Rdata")
save(I_proportion, file = "Data/Rdata/Proportion-attributable.Rdata")


# T = 0: Citywide ---------------------------------------------------------

city_sim <- data.frame(rowSums(x[, 1:9]))
colnames(city_sim) <- "I_simulated"
city_sim$day <- x$day
city_sim$sim_num <- x$sim_num

city1_plot <- ggplot() +
  geom_line(data = city_sim,
            aes(x = day, y = I_simulated, group = sim_num),
            color = "orange",
            alpha = 0.02) +
  geom_line(data = city_obs_2,
            aes(x = day_index, y = I)) +
  ylab("Daily incidence") +
  theme_minimal() +
  ggtitle("Simulated from t = 0 \naggregated to city level")
city1_plot


# T = 0: Attributable cases -----------------------------------------------
# Element-wise mean of list of matrices. From : http://goo.gl/VA7S66
I_att_mean <- data.frame(Reduce("+", I_attr) / length(I_attr), row.names = q_names)
colnames(I_att_mean) <- q_names

I_proportion <- data.frame(matrix(data = NA, nrow = Nquarter, ncol = Nquarter), row.names = q_names)
colnames(I_proportion) <- q_names
for (i in 1:Nquarter){
  for (j in 1:Nquarter){
    I_proportion[j, i] <- I_att_mean[j, i] / quarter_sums[i, 2]
    
  }
}


# T = 0: Save -------------------------------------------------------

setwd(save.plot.path)
ggsave(filename = 'Sim-1-quarter.png',
       plot = sim1_plot,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)

ggsave(filename = 'Sim-1-citywide.png',
       plot = city1_plot,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)

setwd(wd.path)
save(I_att_mean, file = "Attributable-cases-t0.Rdata")
save(I_proportion, file = "Proportion-attributable-t0.Rdata")




###############################################################################
###############################################################################
###############################################################################


# T + 1: PREP -----------------------------------------------------

Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_prev_vect <-     matrix(nrow = Nsteps, ncol = Nquarter)
S_plus1_mat <-    matrix(nrow = Nsteps, ncol = Nquarter)
I_new_plus1 <- list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))
# Attributable infections variables:
Lambda_quart<-lapply(1:Nquarter,
                     function(x) data.frame(matrix(NA,
                                                   nrow=Nsteps,
                                                   ncol=Nquarter)))
I_attr <-lapply(1:loops,
                function(x) data.frame(matrix(NA,
                                              nrow=Nquarter,
                                              ncol=Nquarter)))

# Starting values
I_prev_vect[1, ] <- I_it_daily[1]
I_prev_vect[1, c(5, 8, 9)] <- 1 # Init St.A.V & Ø + Nyb with cases
S_plus1_mat[1, ] <- N_it[, 1] # init all S

# T + 1: SIMULATION -----------------------------------------------------
# "I_it_daily" is the daily "observed" incidence.
ptm <- proc.time()
set.seed(13)
for (z in 1:loops){
  for (t in 1:(Nsteps-1)){
    for(i in 1:Nquarter){
      Lambda_est_pe[t, i] <- S_plus1_mat[t, i] / N_it[i] * sum(betas[, i] * I_prev_vect[t, ])
      LambdaR[t, i] <- I_prev_vect[t, i] * gamma
      
      R_temp <- LambdaR[t, i]
      R_new[t, i] <- min(R_temp, I_prev_vect[t, i])
      
      I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i]  * phi) )
      I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_it_daily[t, i] / phi - R_new[t, i]))
      
      S_temp <- (S_plus1_mat[t, i]) - (I_it_daily[t, i]) / (phi)
      S_plus1_mat[t + 1, i] <- max(0, S_temp)
      Lambda_quart[[i]][t, ] <- (S_plus1_mat[t, i] / N_it[i]) * (betas[, i] * I_prev_vect[t, ])
    }
  }
  # For each quarter: store sum of infections attributed to each quarter over
  # all time-steps
  I_attr[[z]] <- sapply(Lambda_quart, colSums, na.rm=T)
  I_new_plus1[[z]] <- data.frame(I_new)
  I_new_plus1[[z]]$sim_sum <- z
}
proc.time() - ptm

# T + 1 : DATA RESHAPE --------------------------------------------------------------
# Simulated daily incidence to long format:
y <- do.call(rbind.data.frame, I_new_plus1) # merge all sims to 1 df
y[is.na(y)] <- 0
colnames(y) <- c(q_names, "sim_num")
y$day <- 1:112
I_simulated_plus1 <- gather(y, quarter, I_simulated, 1:9) # wide to long
I_obs_df <- data.frame(I_it_daily)
colnames(I_obs_df) <- q_names
I_obs_df$day <- 1:112
I_obs_lng <- gather(I_obs_df, quarter, I_obs, 1:9)



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



# T + 1: Citywide ---------------------------------------------------------

city_sim_plus1 <- data.frame(rowSums(y[, 1:9]))
colnames(city_sim_plus1) <- "I_simulated"
city_sim_plus1$day <- y$day
city_sim_plus1$sim_num <- y$sim_num

city_plus1_plot <- ggplot() +
  geom_line(data = city_sim_plus1,
            aes(x = day, y = I_simulated, group = sim_num),
            color = "dark green",
            alpha = 0.01) +
  geom_line(data = city_obs_2,
            aes(x = day_index, y = I)) +
  ylab("Daily incidence") +
  theme_minimal() +
  ggtitle("Simulated t + 1\naggregated to city level")
city_plus1_plot



# T + 1: Attributable cases -----------------------------------------------
# Element-wise mean of list of matrices. From : http://goo.gl/VA7S66
I_att_mean_plus1 <- data.frame(Reduce("+", I_attr) / length(I_attr), row.names = q_names)
colnames(I_att_mean_plus1) <- q_names

I_proportion_plus1 <- data.frame(matrix(data = NA, nrow = Nquarter, ncol = Nquarter), row.names = q_names)
colnames(I_proportion_plus1) <- q_names
for (i in 1:Nquarter){
  for (j in 1:Nquarter){
    I_proportion_plus1[j, i] <- I_att_mean_plus1[j, i] / quarter_sums[i, 2]
    
  }
}



# T + 1: Save -------------------------------------------------------

setwd(save.plot.path)

ggsave(filename = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/multi/Sim-1-plus1-quarter.png',
       plot = sim1_plus1,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)

ggsave(filename = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/multi/Sim-1-plus1-citywide.png',
       plot = city_plus1_plot,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)

save(I_att_mean_plus1, file = "Attributable-cases-tplus1.Rdata")
save(I_proportion_plus1, file = "Proportion-attributable-tplus1.Rdata")




