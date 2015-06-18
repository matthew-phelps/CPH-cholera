# Author: Matthew Phelps
#Desc: Fooling around to learn how to make a discrete time SIR model in R


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)

library(MASS)
library(deSolve)
library(xlsx)
library(plyr)
library(ggplot2)
library(reshape2)
library(coda)
library(parallel)
library(rstan)
search()

# # Basic SIR Demo ----------------------------------------------------------
# 
# # define function
# demo.model <- function(pars){
#   
#   # show parameters
#   print(pars)
#   
#   # additional parameters
#   times <- seq(from = 0, to = 600, by = 1)  # run for 3000 time steps
#   yinit <- c(S = 0.9, I = 0.1, R = 0)       # sets initial conditions
#   
#   # code for the actual model
#   SIR.model <- function (times, yinit, pars){
#     with(as.list(c(yinit, pars)), {
#       
#       dS <- (birth) - (beta * I * S) -                  (death * S)
#       dI <-           (beta * I * S) - (recovery * I) - (death * I)
#       dR <-                            (recovery * I) - (death * R)
#       
#       return(list(c(dS, dI, dR)))
#     })
#   }
#   
#   # run the ODE solver for the function specified (function defined above is used)
#   # return the value of each compartment (S, I, R) for each time step.
#   results <- ode(func = SIR.model, times = times, y = yinit, parms = pars)
#   results <- as.data.frame(results)
#   
#   # return results
#   return(results)
# }
# 
# test.pars <- c(beta = 0.1, recovery = 0.005, death = 0.001, birth = 0.001)
# results   <- demo.model(test.pars)
# 
# matplot(results[, 1], results[, 2:4], type="l", lty=1)
# legend("topright", col=1:3, legend=c("S", "I", "R"), lwd=1)


# # Practice 1 --------------------------------------------------------------
# rm(list = ls())
# graphics.off()
# 
# quarter.temp <- read.csv("data\\Incident cases per week by quarter.csv")
# quarter.temp$week.id <- quarter.temp$startday.index/7 # create time-step index
# quarter <- quarter.temp[, c(2, 14, 8, 9, 10)] # remove un-needed variables
# rm(quarter.temp)
# 
# quarter$quarterID <- as.numeric(quarter$quarter)
# quarter$cum.sick <- 0
# 
# 
# 
# #### Calculate the number of ppl in each compartment (S,I,R) at each time step:
# 
# 
# # calculate cumilative number of infected in each quarter 
# for (i in 2:208){
#   
#   # check to see if the current row is the same quarter as previous row
#   if(quarter$quarterID[i] != quarter$quarterID[i-1]){
#     quarter$cum.sick[i] <- quarter$sick.total.week[i] # if it's a differernt quarter, reset cummulative count to 0
#     
#     # if it's the same quarter, add the new sick to the total count of sick ppl
#   } else {
#     quarter$cum.sick[i] <- quarter$cum.sick[i-1] + quarter$sick.total.week[i]  
#   }
# }
# 
# # now find S and R based on the N and "cumulative sick" numbers
# quarter$S <- quarter$pop1855 - quarter$cum.sick # no. of susceptibles at each timestep
# quarter$R <- quarter$pop1855 - (quarter$S + quarter$sick.total.week)
# 
# 
# ### Prepare data to send to Stan
# ###
# quarter.1 <- quarter[1:16,]
# Nsteps <- 16
# quarterID <- as.numeric(quarter.1$quarter)
# n <- as.numeric(length(quarterID))
# Nquarter <- max(quarterID)
# S_t <- quarter.1$S
# I_t <- quarter.1$sick.total.week
# R_t <- quarter.1$R
# N <- quarter.1$pop1855
# I_plus1 <- 1:16
# 
# 
# dataList <- list(Nquarter=Nquarter, quarterID=quarterID, n=n, S_t=S_t, I_t=I_t, R_t=R_t, N=N, Nsteps=Nsteps, I_plus1=I_plus1)
# 
# 
# stanDso = stan_model(file = "R codes\\cph_simple.stan" ) # compile model code
# 
# SIR.fit1<- sampling( object = stanDso,
#              data = dataList,
#              iter = 100000, chains = 3,
#              )
#              
# print(SIR.fit1)
# 
# fit1.summary <- as.data.frame(summary(SIR.fit1)$summary)
# ggplot(fit1.summary, aes(Rhat)) +
#   geom_bar() +
#   geom_vline(x=1)
# 
# 
# plot(SIR.fit1)
# 
# summary(SIR.fit1)
# fit.extract <- extract(SIR.fit1, permuted = T)
# beta <- fit.extract$beta
# #plot(beta[5000:])
# 
# 
# traceplot(SIR.fit1, pars="beta", inc_warmup = F) # check traceplots
# 
# 
# 
# 
# mcmcCoda = mcmc.list( lapply( 1:ncol(sir.fit1) ,
#                               function(x) { mcmc(as.array(sir.fit1)[,x,]) } ) )
# 
# 
# 
# 
# 
# 



# Practice 2 --------------------------------------------------------------
rm(list = ls())
graphics.off()

quarter.temp <- read.csv("data\\Incident cases per week by quarter.csv")
quarter.temp$week.id <- quarter.temp$startday.index/7 # create time-step index
quarter <- quarter.temp[, c(2, 14, 8, 9, 10)] # remove un-needed variables
rm(quarter.temp)

quarter$quarterID <- as.numeric(quarter$quarter)
quarter$cum.sick <- 0


####
#### Calculate the number of ppl in each compartment (S,I,R) at each time step:
####

# calculate cumilative number of infected in each quarter 
for (i in 2:208){
  
  # check to see if the current row is the same quarter as previous row
  if(quarter$quarterID[i] != quarter$quarterID[i-1]){
    quarter$cum.sick[i] <- quarter$sick.total.week[i] # if it's a differernt quarter, reset cummulative count to 0
    
    # if it's the same quarter, add the new sick to the total count of sick ppl
  } else {
    quarter$cum.sick[i] <- quarter$cum.sick[i-1] + quarter$sick.total.week[i]  
  }
}

# now find S and R based on the N and "cumulative sick" numbers
quarter$S <- quarter$pop1855 - quarter$cum.sick # no. of susceptibles at each timestep
quarter$R <- quarter$pop1855 - (quarter$S + quarter$sick.total.week)

#write.xlsx2(quarter, file = "C:\\Users\\wrz741\\Google Drev\\Copenhagen\\DK Cholera\\CPH\\Data\\quarter.xlsx", sheetname = "Sheet1", row.names = F)

### Prepare data to send to Stan
Nsteps <- 16
quarterID <- as.numeric(quarter$quarter)
n <- as.numeric(length(quarterID))
Nquarter <- max(quarterID)

S_t <- matrix(0, 16, 13)
I_t <- matrix(0, 16, 13)
R_t <- matrix(0, 16, 13)
N_t <- matrix(0, 16, 13)
for (i in 1:13){
  for( j in 1:16){
  S_t[j,i] <- as.matrix(quarter$S[which(as.numeric(quarter$quarter)==i)])[j]
  I_t[j,i] <- as.matrix(quarter$sick.total.week[which(as.numeric(quarter$quarter)==i)])[j]
  R_t[j,i] <- as.matrix(quarter$R[which(as.numeric(quarter$quarter)==i)])[j]
  N_t[j,i] <- as.matrix(quarter$pop1855[which(as.numeric(quarter$quarter)==i)])[j]
  }
}


dataList <- list(Nquarter=Nquarter, quarterID=quarterID, 
                 n=n, S_t=S_t, I_t=I_t, R_t=R_t, N_t=N_t, Nsteps=Nsteps)
rm(i,j, Nquarter=Nquarter, quarterID=quarterID, 
   n=n, S_t=S_t, I_t=I_t, R_t=R_t, N_t=N_t, Nsteps=Nsteps)

source("http://mc-stan.org/rstan/stan.R")
stanDso = stan_model(file = "R codes\\cph_simple.stan" ) # compile model code
stanDso.2 = stan_model(file = "R codes\\cph_beta.stan" ) # compile model code
#stanDso.3 = stan_model(file = "R codes\\cph_beta_alpha.stan" ) # compile model code

SIR.fit1<- sampling( object = stanDso.2,
                     data = dataList,
                     iter = 500, chains = 1)

print(SIR.fit1)


fit.extract <- extract(SIR.fit1, permuted = T)
beta <- fit.extract$beta
for (i in 1:13){
  mean.beta[i] <- median(beta[,i])
}
plot(mean.beta)
summary(beta[,2])
histogram(beta[,11])
traceplot(SIR.fit1, pars="beta", inc_warmup = F) # check traceplots














S_t/N * (I_t +0.1)

sigmaB <- runif(3000000, 1, 2.5)
tauB <- 1/sigmaB^2
beta0 <- rnorm(3000000, 0, 2)
log.beta <- rnorm(3000000, beta0, tauB)
beta <- exp(log.beta)


min(tauB)
max(tauB)
min(log.beta)
max(log.beta)

max(beta)
mean(beta)
x<-which.max(beta)
sigmaB[x]

beta0[x]
exp(log(10))













