# Author: Matthew Phelps
#Desc: Calculate the LL of the simulated data for model-1. Compare LL of fake
#     fake phi with that of the fitted phi. Ginny said fitted phi should be 
#     somewhat better LL than the fake phi
# Dependicies:

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

# load(file = 'data\\Rdata\\I_fitted_phi.Rdata')
# load(file = 'data\\Rdata\\I_fake_phi.Rdata')
# load(file = 'data\\Rdata\\I_phi_vect.Rdata')
 load(file = 'data\\Rdata\\I_fake_plus1_phi.Rdata')
load(file = 'data\\Rdata\\I_incidence.Rdata')
load(file = 'data\\Rdata\\phi_vect.Rdata')
load(file = 'data\\Rdata\\I_fit_plus1_phi.Rdata')
load(file = 'data\\Rdata\\I_phi_plus1_vect.Rdata')
load(file = 'data\\Rdata\\I_phi_plus1_vect_parallel.Rdata')

# Set some global variables to make life easier later
phi_rez <- length(I_phi_plus1_vect)
num_loops <- nrow(I_fit_plus1_phi)

# Just check to make sure we're dealing with the correct data set
plot(I_incidence)
plot(I_fit_plus1_phi[2, 1:112])


# Restrict likelihood calculation to first half of epidemic. Otherwise we see
# -inf values for some reason:
I_incidence_60 <- I_incidence[20:65]
I_fit_plus1_phi_60 <- I_fit_plus1_phi[, 20:65]

# I_fake_phi_60 <- list()
# I_fitted_phi_60 <- list()
# I_phi_vect_60 <- I_phi_vect
 I_fake_plus1_phi_60 <- list()
# for(loops in 1:length(I_fit_plus1_phi)){
#   #   I_fake_phi_60[[z]] <- I_fake_phi[[z]][20:65]
#   #   I_fitted_phi_60[[z]] <- I_fitted_phi[[z]][1 ,20:65]
#   
#   # I_fake_plus1_phi_60[[z]] <- I_fake_plus1_phi[[z]][20:65]
# } 


I_phi_plus1_vect_60 <- I_phi_plus1_vect
I_phi_plus1_vect_par_60 <- I_phi_plus1_vect_parallel
#Separate for-loop for phi-vector since it is a nested list
for(phi_value in 1:length(I_phi_plus1_vect_parallel)){
  # I_phi_plus1_vect_60[[phi_value]]<- I_phi_plus1_vect[[phi_value]][, 20:65]
  I_phi_plus1_vect_par_60[[phi_value]] <- I_phi_plus1_vect_parallel[[phi_value]][, 20:65]
  }

rm(I_incidence, I_fake_phi, I_fitted_phi, I_fake_plus1_phi,
   I_phi_plus1_vect, I_phi_vect, I_fit_plus1_phi)




# COMPARE PARALLEL w SERIAL -----------------------------------------------

plot(I_incidence_60)
# plot(I_fake_phi_60[[2]])
# plot(I_fitted_phi_60[[2]])
serial_1<-0
par_1 <- 0
for (i in 1:num_loops){
serial_1[i] <- max(I_phi_plus1_vect_60[[150]][i, ])
par_1[i] <- max(I_phi_plus1_vect_par_60[[150]][i, ])
}
serial_1 <- data.frame(serial_1)
par_1 <- data.frame(par_1)
serial_1$loop <- 1:num_loops
par_1$loop <- 1:num_loops
mean(serial_1$serial_1)
mean(par_1$par_1)
sd(serial_1$serial_1)
sd(par_1$par_1)
ggplot() +
  geom_point(data = serial_1, aes(x = loop, y = serial_1),
             color = "blue",
             alpha = 0.7) +
  geom_point(data = par_1, aes(x = loop, y = par_1),
             color = "red3",
             alpha = .7) +
  ylab("Mean max infected")

rm(serial_1, par_1)


######################################################
# FUL SIMULATION
# ####################################################
# # 
# # Sum log-likelihood across each timestep for FAKE phi: -------------------
# 
# ll_t <- list()
# ll_z <- 0
# for(z in 1:length(I_fake_phi_60)){
#   ll_t[[z]] <- dpois(I_incidence_60, I_fake_phi_60[[z]], log = T)
#   ll_z[z] <- exp(sum(ll_t[[z]]))
# }
# 
# model_ll_fake_phi <- sum(ll_z)
# 
# # Sum log-likelihood across each timestep for FITTED phi:
# ll_t <- list()
# ll_z <- 0
# for(z in 1:length(I_fitted_phi_60)){
#   ll_t[[z]] <- dpois(I_incidence_60, I_fitted_phi_60[[z]], log = T)
#   ll_z[z] <- exp(sum(ll_t[[z]]))
# }
# 
# model_ll_fitted_phi <- sum(ll_z)
# 
# 
# # Compare. If fitted is better 
# model_ll_fitted_phi > model_ll_fake_phi
# 
# # Why is the fitted phi much worse than the fake phi?
# 
# 


# MLE FULL SIM for vector of Phis --------------------------------------------------



# ll_t <- vector("list", length(I_phi_vect_60[[1]]))
# ll_z <- vector(length = length(I_phi_vect_60[[1]]))
# model_ll_phi_vect <- matrix(data = NA, nrow = 1, ncol = length(I_phi_vect_60))
# for(phi_value in 1:length(I_phi_vect_60)){
#   for(z in 1:length(I_incidence_60)){
#     ll_t[[z]] <- dpois(I_incidence_60, I_phi_vect_60[[phi_value]][[z]], log = T)
#     ll_z[z] <- exp(sum(ll_t[[z]]))
#   }
#   model_ll_phi_vect[1, phi_value] <- sum(ll_z)
#   }
# 
# model_ll_phi_vect <- rbind(model_ll_phi_vect, phi_pe)
# plot(model_ll_phi_vect[2,], model_ll_phi_vect[1,])
# 
# 
# # GGPLOTs
# model_ll <- data.frame(t(model_ll_phi_vect))
# colnames(model_ll) <- c("LL", "phi")
# 
# ll_plot <- ggplot() +
#   geom_point(data = model_ll,
#              aes(x = phi, y = LL),
#              size = 2) +
#   theme_minimal()
# 
# ggsave(filename = "Output\\Simulations\\LL-phi-1.png",
#        plot = ll_plot,
#        width = 23,
#        height = 15,
#        units = 'cm')


rm(list = setdiff(ls(), c("I_incidence_60",
                          "I_fit_plus1_phi_60",
                          "I_phi_plus1_vect_60",
                          "I_phi_plus1_vect_par_60",
                          "phi_rez",
                          "num_loops",
                          "phi_pe"))) #http://goo.gl/88L5C2

######################################################
# 1_STEP_AHEAD SIMULATION
# ####################################################

# 
# ll_t <- list()
# ll_z <- 0
# for(z in 1:length(I_fake_plus1_phi_60)){
#   ll_t[[z]] <- dpois(I_incidence_60, I_fake_plus1_phi_60[[z]], log = T)
#   ll_z[z] <- exp(sum(ll_t[[z]]))
# }
# 
# model_ll_fake_phi <- sum(ll_z)
# 
# # Sum log-likelihood across each timestep for FITTED phi:
# ll_t <- list()
# ll_z <- 0
# for(z in 1:length(I_fit_plus1_phi_60)){
#   ll_t[[z]] <- dpois(I_incidence_60, I_fit_plus1_phi_60[[z]], log = T)
#   ll_z[z] <- exp(sum(ll_t[[z]]))
# }
# 
# model_ll_fitted_phi <- sum(ll_z)
# 
# 
# # Compare. If fitted is better 
# model_ll_fitted_phi > model_ll_fake_phi


# MLE 1-STEP-AHEAD SERIAL-------------------------------------------------------

# ll_t <- vector("list", length(I_phi_plus1_vect_60))
# ll_row_sums <- matrix(data = NA, nrow = num_loops, ncol = phi_rez)
# 
# ll_phi_avg <- vector(length = length(I_phi_plus1_vect_60))
# 
# for(phi_value in 1:phi_rez){
#   # For each phi value, test likelihood of each day on each loop
#   ll_t[[phi_value]] <- dpois(I_incidence_60, I_phi_plus1_vect_60[[phi_value]], log = T)
#   ll_row_sums[, phi_value] <- exp(rowSums(ll_t[[phi_value]]))
#   ll_phi_avg[phi_value] <- mean(ll_row_sums[, phi_value])
# }
# 
# model_ll_phi_plus1_vect <- matrix(data = NA, nrow = 1, ncol = phi_rez)
# model_ll_phi_plus1_vect <- rbind(ll_phi_avg, phi_pe)
# model_ll <- data.frame(t(model_ll_phi_plus1_vect))
# # plot(model_ll_phi_plus1_vect[2,], model_ll_phi_plus1_vect[1,])
# 
# 
# #GGPLOTs 
# colnames(model_ll) <- c("LL", "phi")
# 
# # Dynamic subtitle to reflect number of loops
# no_loops <- as.character(nrow(I_fit_plus1_phi_60))
# sub_title <- paste("No. simulations = ", no_loops, sept = "")
# 
# ll_plot <- ggplot(data = model_ll,
#                   aes(x = phi, y = log(LL), label = phi)) +
#   geom_point(size = 2) +
#   #Add labels: http://goo.gl/pE9JPI
#   geom_vline(xintercept = 0.0689, linetype = 2) +
#   geom_text(x = 0.0689, label = "fitted phi", angle = 90,
#             y = max(log(model_ll$LL)),
#             vjust = 1.2,
#             size = 3) +
#   geom_text(aes(label = ifelse(LL==max(LL), paste("best phi=", as.character(signif(phi, digits = 3)),sep=""), '')), hjust = -0.1, vjust = 0) +
#   theme_minimal() +
#   ggtitle(bquote(atop("Step-ahead LL", atop(italic(.(sub_title)), "")))) #http://goo.gl/QfFEI0
# 
# ll_plot
# 
# 
# 
# ggsave(filename = "Output\\Simulations\\LL-phi-plus1-log-1-seed130.png",
#        plot = ll_plot,
#        width = 23,
#        height = 15,
#        units = 'cm')



# MLE 1-STEP-AHEAD PARALLE ------------------------------------------------

ll_t <- vector("list", length(I_phi_plus1_vect_par_60))
ll_row_sums <- matrix(data = NA, nrow = num_loops, ncol = phi_rez)

ll_phi_avg <- vector(length = length(I_phi_plus1_vect_par_60))

for(phi_value in 1:phi_rez){
  # For each phi value, test likelihood of each day on each loop
  ll_t[[phi_value]] <- dpois(I_incidence_60, I_phi_plus1_vect_par_60[[phi_value]], log = T)
  
  ll_row_sums[, phi_value] <- exp(rowSums(ll_t[[phi_value]]))
  ll_phi_avg[phi_value] <- mean(ll_row_sums[, phi_value])
}

model_ll_phi_plus1_vect <- matrix(data = NA, nrow = 1, ncol = phi_rez)
model_ll_phi_plus1_vect <- rbind(ll_phi_avg, phi_pe)
model_ll <- data.frame(t(model_ll_phi_plus1_vect))
# plot(model_ll_phi_plus1_vect[2,], model_ll_phi_plus1_vect[1,])


#GGPLOTs 
colnames(model_ll) <- c("LL", "phi")

# Dynamic subtitle to reflect number of loops
no_loops <- as.character(nrow(I_fit_plus1_phi_60))
sub_title <- paste("No. simulations (parallel) = ", no_loops, sept = "")

ll_plot <- ggplot(data = model_ll,
                  aes(x = phi, y = log(LL), label = phi)) +
  geom_point(size = 2) +
  #Add labels: http://goo.gl/pE9JPI
  geom_vline(xintercept = 0.0689, linetype = 2) +
  geom_text(x = 0.0689, label = "fitted phi", angle = 90,
            y = max(log(model_ll$LL)),
            vjust = 1.2,
            size = 3) +
  geom_text(aes(label = ifelse(LL==max(LL), paste("best phi=", as.character(signif(phi, digits = 3)),sep=""), '')), hjust = -0.1, vjust = 0) +
  theme_minimal() +
  ggtitle(bquote(atop("Step-ahead LL", atop(italic(.(sub_title)), "")))) #http://goo.gl/QfFEI0

ll_plot
ggsave(filename = "Output\\Simulations\\LL-phi-plus1-log-1-seed1.png",
       plot = ll_plot,
       width = 23,
       height = 15,
       units = 'cm')

