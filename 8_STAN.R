# Author: Matthew Phelps
#Desc: STAN for cholera data
# output datasets: unkown at this point


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Data"
setwd(pc)
library(reshape)
library(ggplot2)
library(plyr)
library(rstan)
#library(glmnet)
library(stargazer) # for nice output for html etc

load("Rdata\\quarter_combined.Rdata")
quarter.glm <- quarter.merged.glm
rm(quarter.merged.glm)
quarter.glm$R <- quarter.glm$cum.sick <- NULL



# Prepare data for Stan ---------------------------------------------------

Nsteps <- 16
quarterID <- quarter.glm$quarterID
n <- as.numeric(length(quarterID))
No_quarter <- length(table(quarterID)) # number of unique values 

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
stanDso = stan_model(file = "Rcodes\\cph_simple.stan" ) # compile model code

SIR.fit1<- sampling( object = stanDso,
                     data = dataList,
                     iter = 5000, chains = 1)

print(SIR.fit1)