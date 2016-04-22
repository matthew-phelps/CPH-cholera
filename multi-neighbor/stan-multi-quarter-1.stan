# Model 0.1 - Fitting multiple quarters with randomly sampled
# I values between the weekly observed values.

model {
  # Gamma
  gamma ~ dexp(5)
  
  for (i in 1:Nquarter){
    # Beta log hypreprior distributions
    log_beta[i] ~ dnorm(mu[i], tau[i]);
    mu[i] ~ dnorm(0, 0.0001)
    tau[i] <- pow(sigma[i], -2)
    sigma[i] ~ dunif(0, 1.5)
    
    # Beta
    beta[i] <- exp(log_beta[i]);
    
    # Phi prior (under-reporting fraction)
    logit_phi[i] ~ dnorm(0, .001);
    
    # Phi (under-reporting fraction)
    phi[i] <- exp(logit_phi[i]) / (1 + exp(logit_phi[i]));
    
    # First time-step
    S_it_daily[1, i] <- N_i_daily[i];
    I_prev[1, i] <- 1
  }
  
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (beta[i] * (I_prev[t, i]))
      lambdaR[t, i] <- I_prev[t, i] * gamma
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i] - R_new[t, i])
      S_it_daily[t+1, i] <- S_it_daily[t, i] - (I_incidence[t, i] / phi[i])
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
