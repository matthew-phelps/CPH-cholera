# Multible Betas (1 for each quarter) + 1 alpha for city + 1 citywide water


model {
  # Infectious period is exponential dist
  gamma_b ~ dexp(5)
  
  # One hyperprior for internal, one for external
  mu1 ~ dnorm(0, 0.001)
  tau1 ~ dgamma(0.001, 0.001)
  mu2 ~ dnorm(0, 0.001)
  tau2 ~ dgamma(0.001, 0.001)
  
  
  # One city-wide external transmission
  log_beta_2 ~ dnorm(mu2, tau2)
  beta_2 <- exp(log_beta_2)
  
  # Effect of hydraulic connection
  log_eta ~ dnorm(0, 0.001)
  eta <- exp(log_eta)
  
  
  # Phi - under reporting fraction
  logit_phi ~dnorm(0, 0.001)
  phi<- 1 / (1 + exp(-logit_phi))
  
  for (i in 1:Nquarter){
    # First time-step
    S_it_daily[1, i] <- N_i_daily[i];
    
    # Asign 1 infected person into both
    I_prev[1, i] <- ifelse(i==5 || i == 8 || i == 9, 1, 0)
    #I_prev[1, i] <- ifelse(i==5,1,0)
    
    # An independent internal transmission coefficient
    log_beta_1[i] ~ dnorm(mu1, tau1)
    beta_1[i] <- exp(log_beta_1[i])
    for (j in 1:Nquarter){
      # All external transmission coefficients are the same. Water effect, eta,
      # added when there is a water connection
      beta[i, j] <- ifelse(i==j, beta_1[i], beta_2)
      foi[i, j] <- ifelse(water[i, j]==1, eta + beta[i, j], beta[i, j])
    } 
  }
  
  
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(foi[, i] * (I_prev[t, ])))
      R_temp[t, i] <- I_prev[t, i] * gamma_b
      R_new[t, i] <- min(I_prev[t, i], R_temp[t, i])
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i] / phi - R_new[t, i])
      S_it_daily[t+1, i] <- S_it_daily[t, i] - (I_incidence[t, i] / phi)
    }
  }
  
  # Likelihood function with added tracking of "lik" to allow WAIC calculation
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      I_incidence[t+1, i] ~ dpois(lambdaI[t, i] * phi)
      # This log-density function is not documented in the JAGS manual. Found via
      # the 6th response on this forum: https://goo.gl/UisKKW
      llsim [t + 1, i] <- logdensity.pois(I_incidence[t + 1, i], (lambdaI[t, i] * phi))
      lik[t + 1, i] <- exp(llsim[t + 1, i])
    }
  }
  #data# Nsteps
  #data# N_i_daily
  #data# I_incidence
  #data# Nquarter
  #data# water
}
