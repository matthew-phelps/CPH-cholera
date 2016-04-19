# Author: Matthew Phelps
#Desc: Simulations from t = 0 and for t + 1
# Dependicies: model-1-sim-prep


# Intro -------------------------------------------------------------------
graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")
setwd(wd.path)
rm(list = ls())

library(ggplot2)
library(reshape)
require(grid)

# LOAD data ---------------------------------------------------------------

load(file = "data/Rdata/Kj-model-1-sim_data.Rdata")
load(file = "data/Rdata/Kj-mcmc_total.Rdata")
load(file = "data/Rdata/Kj-model-1-data-prep.Rdata")
# rm(betas_matrix, beta_summary_1, phi_matrix, phi_summary_1, dataList, step1,
#    lower_sample, sample_size, t, upper_sample)

duration <- 5 # In days. "1-2 weeks" from DOI:  10.1038/nrmicro2204
gamma <- 1/duration
loops <- 10000 # Has to be the same for both full sum and t+1 sim



# 95% CI for BETA AND PHI -------------------------------------------------
beta_qts <- quantile(mcmc_total$beta, probs = c(0.05, 0.975))
phi_qts <-  quantile(mcmc_total$phi, probs = c(0.05, 0.975))
mcmc_total95 <- mcmc_total[which(mcmc_total$beta > beta_qts[1] &
                                   mcmc_total$beta < beta_qts[2] &
                                   mcmc_total$phi > phi_qts[1] &
                                   mcmc_total$phi < phi_qts[2]) , ]


set.seed(13)
mcmc_sample <- dplyr::sample_n(mcmc_total95, loops)
beta_pe <- mcmc_sample$beta
phi_pe <- mcmc_sample$phi
beta_pe <- median(mcmc_total95$beta)
phi_pe <- median(mcmc_total95$phi)
ggplot(data = mcmc_sample) +
  geom_density2d(aes(x = beta, y = phi))

#  Point Eestimate MODEL FROM INITIAL STATE ------------------------------------------------------------

Lambda_est_pe <-  vector(length = Nsteps)
LambdaR <-        vector(length = Nsteps)
R_new <-          vector(length = Nsteps)
I_new <-          vector(length = Nsteps)
I_sim_vect <-     vector(length = Nsteps)
S_it_est <-       vector(length = Nsteps)
I_new_mat <- matrix(data = NA, nrow = loops, ncol = Nsteps)

# Starting values

S_it_est[1] <- N_i_daily
N_it <- N_i_daily

# Simulate:
ptm <- proc.time()
set.seed(13)
for (z in 1:loops){
  I_sim_vect[1] <- sample(1:3, 1)
  for (t in 1:(Nsteps-1)){
    Lambda_est_pe[t] <- S_it_est[t] / N_it[1] * (beta_pe[1] *(I_sim_vect[t]))
    LambdaR[t] <- I_sim_vect[t] * gamma
    R_new[t] <- rpois(1, LambdaR[t])
    I_new[t] <- rpois(1, (Lambda_est_pe[t] ) )
    I_sim_vect[t + 1] <- max(0, (I_new[t] + I_sim_vect[t] - R_new[t]))
    S_temp <- (S_it_est[t]) -    (I_new[t]) / (phi_pe[1])
    S_it_est[t + 1] <- max(0, S_temp)
  }
  I_new_mat[z, ] <- I_new
}
proc.time() - ptm

# # SAVE for likelhood calculation
# I_fitted_phi <- I_new_mat
# save(I_fitted_phi, file = 'data\\Rdata\\I_fitted_phi.Rdata')
# 






# PLOTTING ----------------------------------------------------------------
# Data to long form for plotting
model_1_full <- t(I_new_mat)
model_1_full <- as.data.frame(model_1_full)
model_1_full$day_index <- 1:Nsteps
# model_1_full_melt <- melt(model_1_full, id.vars = 'day_index')
model_1_full_melt <- tidyr::gather(model_1_full, day_index, value)
colnames(model_1_full_melt) <- c("day_index", "variable", "value")

model_1_obs <- (as.data.frame(I_reps)) # for plotting "observed" data
colnames(model_1_obs ) <- c(1:10)
model_1_obs$day_index <- 1:Nsteps # for plotting "observed" data
model_1_obs <- melt(model_1_obs, id.vars = "day_index")



phi <- as.character(round(phi_pe, digits = 3))
beta <- as.character(round(beta_pe, digits = 3))
no_loops <- as.character(loops)
sub_title <- paste("phi =", phi,
                   "/ beta = ", beta, "")

model_1_full_sim_plot <- ggplot() +
  geom_line(data = model_1_full_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.006) +
  geom_line (data = model_1_obs,
             aes(x = day_index, y = value, group = variable),
             color = 'darkred', alpha = 0.5, size = 1.2) +
  theme_minimal()+
  ylab("People") +
  xlab("Day index") + 
  ylim(0, 50) +
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4))+
  ggtitle(bquote(atop("Kjoebmager. Incidence",
                      atop(italic(.(sub_title)), "")))) #http://goo.gl/QfFEI0
model_1_full_sim_plot

# system.time(
#   ggsave(model_1_full_sim_plot,
#          file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\Kj-short-Fig-1-model-1-full-sim.pdf',
#          width=15, height=9,
#          units = 'in')
# )

ggsave(model_1_full_sim_plot,
       file = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/Kj-1-full-sim.jpg',
       width=9, height=9,
       units = 'in')


# 


# STEP AHEAD SIMULATION ---------------------------------------------------


Lambda_est_pe <-  vector(length = Nsteps)
LambdaR <-        vector(length = Nsteps)
R_new <-          vector(length = Nsteps)
I_new <-          vector(length = Nsteps)
I_sim_vect <-     vector(length = Nsteps)
S_plus1_mat <-     vector(length = Nsteps)
I_new_plus1_mat <- matrix(data = NA, nrow = loops, ncol = Nsteps)

# Starting values
I_sim_vect[1] <- I_it_daily[1]
S_plus1_mat[1] <- N_i_daily

# Simulate:
ptm <- proc.time()
set.seed(13)
for (z in 1:loops){
  rep_num <- sample(1:10, 1)
  for (t in 1:(Nsteps-1)){
    
    
    Lambda_est_pe[t] <- S_plus1_mat[t] / N_i_daily * (beta_pe[1] *(I_sim_vect[t]))
    LambdaR[t] <- I_sim_vect[t] * gamma
    R_new[t] <- rpois(1, LambdaR[t])
    #if(z == 2 & t == 5){browser()}
    I_new[t] <- rpois(1, (Lambda_est_pe[t] ) )
    I_sim_vect[t + 1] <- max(0, (I_reps[[rep_num]][t] + I_sim_vect[t] - R_new[t]))
    S_temp <- (S_plus1_mat[t]) -    (I_new[t]) / (phi_pe[1])
    S_plus1_mat[t + 1] <- max(0, S_temp)
  }
  I_new_plus1_mat[z, ] <- I_new
}
proc.time() - ptm
# # SAVE for likelhood calculation
# I_fit_plus1_phi <- I_new_plus1_mat
# save(I_fit_plus1_phi, file = 'data\\Rdata\\I_fit_plus1_phi.Rdata')





# Plotting ----------------------------------------------------------------
model_1_tplus1 <- t(I_new_plus1_mat)
model_1_tplus1 <- as.data.frame(model_1_tplus1)
model_1_tplus1$day_index <- 1:Nsteps
# model_1_tplus1_melt <- melt(model_1_tplus1, id.vars = 'day_index')
model_1_tplus1_melt <- tidyr::gather(model_1_tplus1, day_index, value)
colnames(model_1_tplus1_melt) <- c("day_index", "variable", "value")
model_1_obs <- (as.data.frame(I_reps)) # for plotting "observed" data
colnames(model_1_obs ) <- c(1:10)
model_1_obs$day_index <- 1:Nsteps # for plotting "observed" data
model_1_obs <- melt(model_1_obs, id.vars = "day_index")

no_loops <- as.character(loops)
sub_title <- paste("phi =", phi,
                   "/ beta = ", beta, "")



model_1_tplus1_plot <- ggplot() +
  geom_line(data = model_1_tplus1_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.02) +
  geom_line(data = model_1_obs,
            aes(x = day_index, y = value, group = variable),
            color = 'darkred', alpha = 0.5, size = 1.3) +
  theme_minimal()+
  ylab("People") +
  xlab("Day index") + 
  theme(plot.title = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4))+
  ggtitle(bquote(atop("Kjoebmager t + 1",
                      atop(italic(.(sub_title)), "")))) #http://goo.gl/QfFEI0)
model_1_tplus1_plot

# system.time(ggsave(model_1_tplus1_plot, 
#                    file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\Fig-2-model_1_tplus1-I.pdf',
#                    width=15, height=9,
#                    units = 'in')
# )

ggsave(model_1_tplus1_plot, 
       file = '/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Simulations/Kj-1-tplus1-sim.jpg',
       width=9, height=9,
       units = 'in')
