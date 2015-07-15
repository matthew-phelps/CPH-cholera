# Author: Matthew Phelps
#Desc: Manually going through time steps of infection to check poisson model


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())
library(MASS)
library(deSolve)
library(xlsx)
library(plyr)
library(ggplot2)
library(reshape2)
library(coda)
library(parallel)
library(rstan)





# COMBINED quarters -------------------------------------------------------
rm(list = ls())
load(file = "data\\Rdata\\quarter_combined.Rdata")


### Prepare data to send to Stan
Nsteps <- 16
quarterID <- as.numeric(combined$quarterID)
n <- as.numeric(length(quarterID))
Nquarter <- length(table(quarterID))

S_ti <- matrix(0, Nsteps, Nquarter)
I_ti <- matrix(0, Nsteps, Nquarter)
R_t <- matrix(0, Nsteps, Nquarter)
N_i <- matrix(0, Nsteps, Nquarter)
for (i in 1:Nquarter){
  for( t in 1:Nsteps){
    S_ti[t,i] <- (combined$S[which(combined$quarterID==i)])[t]
    I_ti[t,i] <- (combined$sick.total.week[which(combined$quarterID==i)])[t]
    R_t[t,i] <- (combined$R[which(combined$quarterID==i)])[t]
    N_i[t,i] <- (combined$pop1855[which(combined$quarterID==i)])[t]
  }
}

for (i in 1:Nquarter){
  for( t in 1:Nsteps-1){
    S_ti[t+1,i] <- (combined$S[which(combined$quarterID==i)])[t] - I_ti[t]
  }
}
    
# calcualte the number of infected people in all OTHER quarters EXCEPT quarter "i"
I_tj <- matrix(0, nrow = Nsteps, ncol = Nquarter)
for (j in 1:Nquarter){
  for (t in 1:Nsteps){
    I_tj[t,j] <- sum(I_ti[t,]) - I_ti[t,j]
  }
}

frac_suseptible_it <- matrix(0, nrow = Nsteps, ncol = Nquarter)
for (i in 1:Nquarter){
  for (t in 1:Nsteps){
    frac_suseptible_it[t,i] <- S_ti[t,i] / N_i[t,i]
  }
}




# Model 1.1 -------------------------------------------------------------------
lambda <- data.frame()
for (i in 1:Nquarter){
  for (t in 1:Nsteps){
    lambda[t, i] <- round(frac_suseptible_it[t,i] * (1*I_ti[t,i] + 1*I_tj[t,i]))
  }
}

I_est <- data.frame()
for (i in 1:nrow(lambda)){
  for (j in 1:ncol(lambda)){
    I_est[i,j] <-round(mean(rpois(10000, lambda[i,j])), digits = 0)
  }
}

rpois(100, 15)

# Model 1.2 -------------------------------------------------------------------
beta <- data.frame
lambda <- data.frame()
for (i in 1:Nquarter){
  for (t in 1:Nsteps){
    lambda[t, i] <- round(frac_suseptible_it[t,i] * 0.1 * (1 * I_ti[t,i] + 1*I_tj[t,i]))
  }
}

I_est <- data.frame()
for (i in 1:nrow(lambda)){
  for (j in 1:ncol(lambda)){
    I_est[i,j] <-round(mean(rpois(10000, lambda[i,j])), digits = 0)
  }
}

