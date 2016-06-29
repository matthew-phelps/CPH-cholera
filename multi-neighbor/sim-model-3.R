# Author: Matthew Phelps
# Desc: Simulate model 1 based on inferred MCMC parameter values

graphics.off()
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")
ifelse(grepl("wrz741", getwd()),
       save.plot.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/Multi",
       save.plot.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/Multi")

setwd(wd.path)


library(ggplot2)
library(tidyr)
library(dplyr)
require(grid)
library(coda)
library(CholeraDataDK)

# LOAD data ---------------------------------------------------------------
load(file = "Data_3.Rdata")
Nweeks <- Nsteps
load(file = "sim-model-3-data.Rdata")



# CITY LEVEL DATA ---------------------------------------------------------
city_obs <- cholera_daily_data[cholera_daily_data$city == "copenhagen", ] # city-wide data
city_obs <- select(city_obs, c(day_index, cases)) # remove un-needed columns

city_obs_2 <- data.frame(t(I_it))
colnames(city_obs_2) <- q_names
city_obs_2$day_index <- seq(from = 7, to = (Nweeks )* 7, length.out = Nweeks)
city_obs_2$I <- rowSums(city_obs_2[, 1:9]) / 7
quarter_sums <- select(combined, c(quarter, week.id, cum.sick))
quarter_sums <- quarter_sums[quarter_sums$week.id == 15, c(1, 3)]


# GLOBAL VARIABLES -------------------------------------------------------------------

loops <- 12 # Has to be the same for both full sum and t+1 sim
n_gam <- loops*Nsteps*Nquarter
gamma <- 1/5
###############################################################################
###############################################################################
###############################################################################


# T = 0: PREP-----------------------------------------------------

Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_prev_vect <-     matrix(nrow = Nsteps, ncol = Nquarter)
S_it_est <-       matrix(nrow = Nsteps, ncol = Nquarter)

Lambda_quart <-   list(data.frame(matrix(nrow = Nsteps, ncol = Nquarter)))
I_new_mat <- list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))
# Attributable infections variables:
Lambda_quart<-lapply(1:Nquarter,
                     function(x) data.frame(matrix(NA,
                                                   nrow=Nsteps,
                                                   ncol=Nquarter)))
I_attr <-list((matrix(data = NA, nrow = Nquarter, ncol = Nquarter)))
# Starting values
I_prev_vect[1, ] <- I_it_daily[1]
I_prev_vect[1, c(5, 8, 9)] <- 1 # Init Nyboder with 2 cases
S_it_est[1, ] <- N_it[, 1] # init all S

# T = 0: SIMULATION-----------------------------------------------------
ptm <- proc.time()
set.seed(13)
for (z in 1:loops){
  for (t in 1:(Nsteps-1)){
    for(i in 1:Nquarter){
      #if(i ==1) browser()
      Lambda_est_pe[t, i] <- S_it_est[t, i] / N_it[i] * sum( betas[, i] * I_prev_vect[t, ])
      LambdaR[t, i] <- I_prev_vect[t, i] * gamma
      
      R_temp <- LambdaR[t, i]
      R_new[t, i] <- min(R_temp, I_prev_vect[t, i]) # no more recovereds than infected
      
      I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] * phi) )
      I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_new[t, i] / phi - R_new[t, i]))
      
      S_temp <- (S_it_est[t, i]) -    (I_new[t, i]) / (phi)
      S_it_est[t + 1, i] <- max(0, S_temp)
      Lambda_quart[[i]][t, ] <- (S_it_est[t, i] / N_it[i]) * ( betas[, i] * I_prev_vect[t, ])
    }
  }
  # For each quarter: store sum of infections attributed to each quarter over
  # all time-steps
  I_attr[[z]] <- sapply(Lambda_quart, colSums, na.rm=T)
  # Store new infections at each timestep for each quarter for each simulation
  I_new_mat[[z]] <- data.frame(I_new)
  I_new_mat[[z]]$sim_num <- z
}
proc.time() - ptm





# T = 0 : DATA RESHAPE --------------------------------------------------------------

# Simulated daily for each quarter
x <- do.call(rbind.data.frame, I_new_mat) # merge all sims to 1 df
colnames(x) <- c(q_names, "sim_num")
x$day <- 1:Nsteps
I_simulated <- gather(x, quarter, I_simulated, 1:Nquarter) # wide to long

# Check cum sums for all simulations to see if total epi size is correct
x1 <- split(x, f = x$sim_num)
x2 <- lapply(x1, cumsum)
# extract nth row of each df http://goo.gl/wxujJK
x3 <- (lapply(x2, "[", 111, 1:9, drop = F)) 
x4 <-t(do.call(rbind.data.frame, x3))
x5 <- data.frame(matrix(x4, nrow = 9, ncol = loops))
rm(x1, x2, x3, x4)
# The last row reads as the percentage of total city that become infected
cumsum(x5) / cumsum(N_it)


# Observed data
I_obs_df <- data.frame(I_it_daily)
colnames(I_obs_df) <- q_names
I_obs_df$day <- 1:Nsteps
I_obs_lng <- gather(I_obs_df, quarter, I_obs, 1:Nquarter)
rm(x)

x1 <- cumsum(I_obs_df[, 1:9])
x1 <- (x1[111,])
x1 <- t(x1)
cumsum(x1) / cumsum(N_it)


# T = 0: PLOT PANEL QUARTERS-------------------------------------------------------------

sim1_plot <- ggplot() +
  geom_line(data = I_simulated,
            alpha = 0.5,
            aes(x = day, y = I_simulated,
                group = interaction(quarter, sim_num),
                color = quarter)) +
  geom_line(data = I_obs_lng, aes(x = day,
                                  y = I_obs, group = quarter)) +
  facet_wrap(~quarter) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("M3 Simulation from t = 0")
sim1_plot




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
ggsave(filename = 'Sim-3-quarter.png',
       plot = sim1_plot,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)
setwd(wd.path)
save(I_att_mean, file = "Attributable-cases-t0-m3.Rdata")
save(I_proportion, file = "Proportion-attributable-t0-m3.Rdata")




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
  ggtitle("M3 sim t + 1")
sim1_plus1



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

ggsave(filename = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/multi/Sim-3-plus1-quarter.png',
       plot = sim1_plus1,
       width = 26,
       height = 16,
       units = 'cm',
       dpi = 300)


save(I_att_mean_plus1, file = "Attributable-cases-tplus1-m3.Rdata")
save(I_proportion_plus1, file = "Proportion-attributable-tplus1-m3.Rdata")




