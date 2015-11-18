# Author: Matthew Phelps
#Desc: Using the fake beta value, change time-step to daily value



# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())

library(ggplot2)
library(reshape)
require(grid)

# LOAD data ---------------------------------------------------------------

load(file = 'data\\Rdata\\model_0_1_sim_data.Rdata')
load(file = 'data\\Rdata\\Data_3.Rdata')
set.seed(12)



Ndays <- Nsteps * 7
I_it <- I_it[1,]
S_it <- S_it[1,]
N_i <- N_i[1, ]

R_i <- seq(from = 0, to = 0, length.out = length(I_it))

I_it_est <- matrix(data =  NA, nrow = 1, ncol = Ndays)
S_it_est <- matrix(data = NA, nrow = 1, ncol = Ndays)
R_new <- matrix(data =  NA, nrow = 1, ncol = Ndays)
# SPECIFY RECOVERY --------------------------------------------------------

duration <- 10 # In days. "1-2 weeks" from DOI:  10.1038/nrmicro2204
gamma <- 1/duration


# SET FAKE PARAMETERS ---------------------------------------------------------
beta_fake_vect <- 2.1
phi_fake_vect <- phi_pe

#  Point Eestimate MODEL FROM INITIAL STATE ------------------------------------------------------------

# 1st week no infectious to match observed I
I_it_est[1:13] <- 0
S_it_est[1:14] <- N_it[1]
R_new[1:14] <- 0

# Middle 2nd week, infectious start
I_it_est[14] <- .5/phi_fake_vect


loops <- 1000
I_est_pe_list <- list()
S_it_est_pe_list <- list()
for (z in 1:loops){
  
  Lambda_est_pe <- matrix(data = NA, nrow = 1, ncol = Ndays)
  LambdaR <- matrix(data = NA, nrow = 1, ncol = Ndays)
  for (t in 14:(Ndays-1)){
    Lambda_est_pe[t] <- S_it_est[t] / N_it[1] * (beta_fake_vect[1]/7 *(I_it_est[t]))
    LambdaR[t] <- I_it_est[t] * gamma
    R_new[t +1 ] <- rpois(1, LambdaR[t])
    I_new <- rpois(1, (Lambda_est_pe[t] ) )
    I_it_est[t + 1] <- max(0, (I_new + I_it_est[t] - R_new[t + 1]))
    S_temp <- (S_it_est[t]) -    (I_new) / (phi_fake_vect[1])
    S_it_est[t + 1] <- max(0, S_temp)
  }
  
  I_est_pe_list[[z]] <- I_it_est
  S_it_est_pe_list[[z]] <- S_it_est
}

I_it_est


# SAVE DATA FOR LIKELIHOOD CALCULATION ------------------------------------



# PE RESHAPE DATA ---------------------------------------------------------

# Infectious Data for all quarters (city_pe level). Flatten each matrix
christ_full <- as.data.frame(matrix(data = 0, nrow = Ndays, ncol = loops))
christ_full$day_index <- 1:Ndays
#christ_full$day_index <- christ_full$week_index * 7

for (z in 1:loops){
  christ_full[z] <- as.data.frame(colSums(I_est_pe_list[[z]]))
}
christ_full_melt <- melt(christ_full, id.vars = 'day_index')





# Prepare observed data aggregated to the week ----------------------------------------


chrit_obs <- as.data.frame(I_it)
chrit_obs$week_index <- 1:Nsteps
chrit_obs$day_index <- chrit_obs$week_index * 7
chrit_obs$week_index <- NULL



# INTERPOLATE BETWEEN WEEKLY OBSERVATIONS ---------------------------------



# # city_pe level Infectious
christ_full_sim_plot <- ggplot() +
  geom_line(data = christ_full_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.05) +
  geom_line (data = chrit_obs,
             aes(x = day_index, y = I_it),
             color = 'darkred', alpha = 0.5, size = 1.2) +
  theme_minimal()+
  ylab("People") +
  xlab("Day index") + 
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4))+
  ggtitle('Christianshavn infectious\n simulated n = 2000')
christ_full_sim_plot
