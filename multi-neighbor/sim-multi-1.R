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

# LOAD data ---------------------------------------------------------------
load(file = "data/Rdata/sim-multi-1-data.Rdata")



duration <- 5 # In days. "1-2 weeks" from DOI:  10.1038/nrmicro2204
gamma <- 1/duration
loops <- 10000 # Has to be the same for both full sum and t+1 sim




# T = 0: SIMULATION-----------------------------------------------------

Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
I_sim_vect <-     matrix(nrow = Nsteps, ncol = Nquarter)
S_it_est <-       matrix(nrow = Nsteps, ncol = Nquarter)
I_new_mat <- list(matrix(data = NA, nrow = Nsteps, ncol = Nquarter))

# Starting values
I_sim_vect[1, ] <- I_it_daily[1]
I_sim_vect[1, c(5, 8, 9)] <- 3 # Init Nyboder with 2 cases
S_it_est[1, ] <- N_it[, 1] # init all S

# Simulate:
ptm <- proc.time()
set.seed(13)
loops <- 1 ### TEMP for TESTING
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
  I_new_mat[[z]] <- I_new
}
proc.time() - ptm




# T = 0 : DATA RESHAPE --------------------------------------------------------------

# Simulated daily incidence to long format:
x <- data.frame(I_new_mat[[z]])
colnames(x) <- q_names
x$day <- 1:112
I_simulated <- gather(x, quarter, I_simulated, 1:9)


