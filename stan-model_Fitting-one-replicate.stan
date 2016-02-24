# Model 0.1 - Fitting 1 quarter with randomly sampled
# I values between the weekly observed values.

model {
  # Beta log hypreprior distributions
  log_beta ~ dnorm(mu, tau);
  mu ~ dnorm(0, 0.0001)
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 1.5)
  
  # Beta
  beta <- exp(log_beta);
  
  # Phi prior (under-reporting fraction)
  logit_phi ~ dnorm(0, .001);
  
  # Phi (under-reporting fraction)
  phi <- exp(logit_phi) / (1 + exp(logit_phi));
  
  # Gamma
  gamma ~ dexp(5)
  
  # First time-step
  S_it_daily[1] <- N_i_daily;
  R_new[1] <- 0
  I_prev[1] <- 1
  
  
  
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    lambdaI[t] <-  (S_it_daily[t]  / N_i_daily) * (beta * (I_prev[t]))
    lambdaR[t] <- I_prev[t] * gamma
    I_prev[t+1] <- (I_prev[t] + I_incidence[t] - R_new[t])
    S_it_daily[t+1] <- S_it_daily[t] - (I_incidence[t] / phi)
    
  }

  
  # Likelihood function
  for (t in 1:(Nsteps-1)){
    I_incidence[t+1] ~ dpois(lambdaI[t])
    R_new[t] ~ dpois(lambdaR[t])
  }
  
  #data# Nsteps
  #data# N_i_daily
  #data# I_incidence
  
  
  
  
  
  
  
}
