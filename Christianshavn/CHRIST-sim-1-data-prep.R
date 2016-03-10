# Author: Matthew Phelps
#Desc: Prepare data from JAGS for simulations
# Dependicies: model-1-JAGS


# Intro -------------------------------------------------------------------

graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
rm(list = ls())




# LOAD & PREP DATA ---------------------------------------------------------------


load(file = "Data/Rdata/quarter_combined.Rdata")
load(file = "Data\\Rdata\\CHRIST-model-1-dataList.Rdata")

N_i_daily <- dataList[[1]]$N_i_daily
I_it_daily <- dataList[[4]]$I_incidence
Nsteps <- dataList[[1]]$Nsteps
plot(I_it_daily)

# INITIALIZE EMPTY DF -----------------------------------------------------

I_it <- matrix(NA, 1, Nsteps-1)
S_it <- matrix(NA, 1, Nsteps-1)

N_it <- matrix(NA, 1, Nsteps)

I_i_t1 <- matrix(0, nrow = 1, ncol = 1)
S_i_t1 <- matrix(0, nrow = 1, ncol = 1)
N_i_t1 <- matrix(0, nrow = 1, ncol = 1)
I_i_t1[1, 1] <- (combined$sick.total.week[which(combined$quarter== "Christianshavn")])[1]
S_i_t1[1, 1] <- (combined$S[which(combined$quarter== "Christianshavn")])[1]

for (t in 1:Nsteps){
  N_it[1, t] <- (combined$est.pop.1853[which(combined$quarter== "Christianshavn")])[t]
}

# Bind first time-step of infection data to block of NAs the size of the remaining
# timesteps. These NAs will be overwritten with simulated data 
I_it_est <- (cbind(I_i_t1, I_it))
S_it_est <- (cbind(S_i_t1, S_it))
I_it_est <- I_it_est[1, ]
S_it_est <- S_it_est[1, ]
I_plus1 <- I_it_est
S_plus1 <- S_it_est

rm(N_i_t1, S_i_t1 , I_i_t1, I_it, S_it)

# SAVE --------------------------------------------------------------------

save(list = ls(), file = 'data/Rdata/CHRIST-model-1-sim_data.Rdata' )

