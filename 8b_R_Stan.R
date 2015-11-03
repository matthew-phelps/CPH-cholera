# Author: Matthew Phelps
#Desc: Fooling around to learn how to make a discrete time SIR model in R


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
# For execution on a local, multicore CPU with excess RAM we recommend calling
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())




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


dataList <- list(Nquarter=Nquarter, quarterID=quarterID, 
                 frac_suseptible_it = frac_suseptible_it, n=n, S_ti=S_ti, I_ti=I_ti, R_t=R_t, N_i=N_i, Nsteps=Nsteps, I_tj=I_tj)
# rm(i,j, Nquarter=Nquarter, quarterID=quarterID, 
#    n=n, S_t=S_t, I_t=I_t, R_t=R_t, N_t=N_t, Nsteps=Nsteps)

#source("http://mc-stan.org/rstan/stan.R")
stanDso = stan_model(file = "Rcodes\\cph_beta.stan") # compile e
stanDso.1.1 = stan_model(file = "Rcodes\\cph_model1_1.stan" )
stanDso.1.2 = stan_model(file = "Rcodes\\cph_model1_2.stan" )
stanDso.1.3 = stan_model(file = "Rcodes\\cph_model1_3.stan" )

system.time(
SIR.fit.beta<- sampling( object = stanDso,
                     data = dataList,
                     iter = 5000, chains = 4,
                     cores = 8)
)



SIR.fit1.1<- sampling( object = stanDso.1.1,
                     data = dataList,
                     iter = 5000, chains = 3,
                     cores = 3)

SIR.fit1.2<- sampling( object = stanDso.1.2,
                     data = dataList,
                     iter = 20000, chains = 3,
                     cores = 3)

SIR.fit1.3<- sampling( object = stanDso.1.3,
                       data = dataList,
                       iter = 250000, chains = 3,
                       cores = 3)


print(SIR.fit1.1)
print(SIR.fit1.2)
print(SIR.fit1.3)
print(SIR.fit4)

str(SIR.fit2)
fit.extract <- extract(SIR.fit1, permuted = T)
beta <- fit.extract$beta
for (i in 1:13){
  mean.beta[i] <- median(beta[,i])
}
plot(mean.beta)
summary(beta[,2])
histogram(beta[,11])
traceplot(SIR.fit1, pars="beta", inc_warmup = F) # check traceplots


# 
# 
# # ALL Quaters--------------------------------------------------------------
# 
# load(file = "data\\Rdata\\quarter_eng.Rdata")
# 
# 
# ### Prepare data to send to Stan
# Nsteps <- 16
# quarterID <- as.numeric(quarter$quarterID)
# n <- as.numeric(length(quarterID))
# Nquarter <- max(quarterID)
# 
# S_t <- matrix(0, 16, 13)
# I_t <- matrix(0, 16, 13)
# R_t <- matrix(0, 16, 13)
# N_t <- matrix(0, 16, 13)
# for (i in 1:13){
#   for( j in 1:16){
#     S_t[j,i] <- as.matrix(quarter$S[which(quarter$quarterID==i)])[j]
#     I_t[j,i] <- as.matrix(quarter$sick.total.week[which(quarter$quarterID==i)])[j]
#     R_t[j,i] <- as.matrix(quarter$R[which(quarter$quarterID==i)])[j]
#     N_t[j,i] <- as.matrix(quarter$pop1855[which(quarter$quarterID==i)])[j]
#   }
# }
# 
# 
# dataList <- list(Nquarter=Nquarter, quarterID=quarterID, 
#                  n=n, S_t=S_t, I_t=I_t, R_t=R_t, N_t=N_t, Nsteps=Nsteps)
# rm(i,j, Nquarter=Nquarter, quarterID=quarterID, 
#    n=n, S_t=S_t, I_t=I_t, R_t=R_t, N_t=N_t, Nsteps=Nsteps)
# 
# source("http://mc-stan.org/rstan/stan.R")
# stanDso = stan_model(file = "Rcodes\\cph_simple.stan" ) # compile model code
# 
# SIR.fit1<- sampling( object = stanDso,
#                      data = dataList,
#                      iter = 5000, chains = 1)
# 
# print(SIR.fit1)
# 
# 
# fit.extract <- extract(SIR.fit1, permuted = T)
# beta <- fit.extract$beta
# for (i in 1:13){
#   mean.beta[i] <- median(beta[,i])
# }
# plot(mean.beta)
# summary(beta[,2])
# histogram(beta[,11])
# traceplot(SIR.fit1, pars="beta", inc_warmup = F) # check traceplots
# 
# 
# 
# 
# 
# 
# 
# 
# 
# S_t/N * (I_t +0.1)
# 
# sigmaB <- runif(3000000, 1, 2.5)
# tauB <- 1/sigmaB^2
# beta0 <- rnorm(3000000, 0, 2)
# log.beta <- rnorm(3000000, beta0, tauB)
# beta <- exp(log.beta)
# 
# 
# min(tauB)
# max(tauB)
# min(log.beta)
# max(log.beta)
# 
# max(beta)
# mean(beta)
# x<-which.max(beta)
# sigmaB[x]
# 
# beta0[x]
# exp(log(10))
# 
# 
# 
# 
# 








