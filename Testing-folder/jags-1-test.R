rm(list = ls())
graphics.off()
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/data/Rdata")

ifelse(grepl("wrz741", getwd()),
       model.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/RCodes/multi-neighbor",
       model.path <-"/Users/Matthew/GitClones/RCodes/multi-neighbor")

setwd(data.path)
load(file = "multi-model1-data-prep.Rdata")

N_i_daily <- N_pop[, 2]
I_incidence <- I_reps[[1]]
row.names(I_incidence) <- NULL

# 1 beta (citywide) + 1 alpha for city
  gamma_b <- 5
  
  # One hyperprior for entire city
  mu1 <- rnorm(1, 0, 1/0.01)
  tau1 <- rgamma(100, 0.001, 0.001)
  mu2 <- rnorm(1, 0, 1/0.01)
  tau2 <- rgamma(1, 0.001, 0.001)
  
  # 1 Internal force, same for every quarter
  log_beta_1 <- rnorm(1, mu1, tau1)
  beta_1 <- exp(log_beta_1)
  
  # 1 external force, same for every quarter
  log_beta_2 <- rnorm(1, mu2, tau2)
  beta_2 <- exp(log_beta_2)
  
  # Phi - under reporting fraction (same for all quarters)
  
  logit_phi <- rnorm(1, 0, 1/0.01)
  phi<- exp(logit_phi) / (1 + exp(logit_phi))
  
  
  beta_2 <- 0.01
  beta_1 <- 0.1
  phi <- 0.5
  
  S_it_daily <- matrix(NA, nrow = Nsteps, ncol = Nquarter)
  I_prev <- matrix(NA, nrow = Nsteps, ncol = Nquarter)
  beta <- matrix(NA, nrow = Nquarter, ncol = Nquarter)
  for (k in 1:Nquarter){
    # First time-step
    S_it_daily[1, k] <- N_i_daily[k]
    
    # Asign 1 infected person into both
    I_prev[1, k] <- 1
    #I_prev[1, i] <- ifelse(i==5,1,0)
    
    for (i in 1:Nquarter){
      # All external transmission coefficients are the same
      beta[k, i] <- ifelse(k==i, beta_1, beta_2)
    } 
  }
  
  lambdaI <- matrix(NA, nrow = Nsteps-1, ncol = Nquarter)
  lambdaR <- matrix(NA, nrow = Nsteps-1, ncol = Nquarter)
  R_new <- matrix(NA, nrow = Nsteps-1, ncol = Nquarter)
  I_pred <- matrix(NA, nrow = Nsteps-1, ncol = Nquarter)
 
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      #if(t == 1) browser()
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(beta[, i] * (I_prev[t, ])))
      I_pred[t, i] <- rpois(1, lambdaI[t, i])
      lambdaR[t, i] <- I_prev[t, i] * gamma_b
      R_temp <- rpois(1, lambdaR[t, i])
      R_new[t, i] <- min(I_prev[t, i], R_temp)
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i] / phi - R_new[t, i])
      S_it_daily[t+1, i] <- S_it_daily[t, i] - (I_incidence[t, i] / phi)
    }
  }
  plot(I_pred[, i])
  lines(I_incidence[, i], col = "red")
  
  llike <- function(param) {
    lambdaI = param[1]
    
    pred = I_incidence
    singleLL = dpois(I_incidence, lambda = lambdaI, log = T)
    sumLL = sum(singleLL)
    return(sumLL)
  }
  llike(lambdaI)

  
  -20342.41
  -11569.39
  
  x <- seq(0.01, 50, by=0.1)
  mean(rgamma(x, 0.001, 0.001))
  
  plot(x, dgamma(x, shape = 0.001, rate = 0.001))
  
  # Likelihood function
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      I_incidence[t+1, i] ~ dpois(lambdaI[t, i] * phi)

    }
  }

