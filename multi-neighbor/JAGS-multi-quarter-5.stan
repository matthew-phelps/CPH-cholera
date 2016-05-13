# Model 0.1 - Fitting multiple quarters with randomly sampled
# I values between the weekly observed values.

model {
  # Gamma
  gamma ~ dexp(5)
  
  # One hyperprior for entire city
  mu ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
  
  # Phi - under reporting fraction
  logit_phi ~dnorm(0, 0.001)
  phi<- exp(logit_phi) / (1 + exp(logit_phi))
  
  for (i in 1:Nquarter){
    # First time-step
    S_it_daily[1, i] <- N_i_daily[i];
    
    I_prev[1, i] <- ifelse(i==1,1,0)
    
    for (j in 1:Nquarter){
      # Beta log hypreprior distributions
      log_beta[i, j] ~ dnorm(mu, tau);
      # Beta
      beta[i, j] <- exp(log_beta[i, j]);
    } 
  }
   
   
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(beta[, i] * (I_prev[t, ])))
      lambdaR[t, i] <- I_prev[t, i] * gamma
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i]  - R_new[t, i])
      S_it_daily[t+1, i] <- S_it_daily[t, i] - (I_incidence[t, i] / phi)
    }
  }
  
  # Likelihood function
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      I_incidence[t+1, i] ~ dpois(lambdaI[t, i])
      R_new[t, i] ~ dpois(lambdaR[t, i])
    }
  }
  #data# Nsteps
  #data# N_i_daily
  #data# I_incidence
  #data# Nquarter
}
