# Model 0.1 - Fitting multiple quarters with randomly sampled
# I values between the weekly observed values.

model {
  # Infectious period is exponential dist
  gamma_b ~ dexp(5)
  
  # One hyperprior for entire city
  mu1 ~ dnorm(0, 0.001)
  tau1 ~ dgamma(0.001, 0.001)
  mu2 ~ dnorm(0, 0.001)
  tau2 ~ dgamma(0.001, 0.001)
  
  # Phi - under reporting fraction
  logit_phi ~dnorm(0, 0.001)
  phi<- exp(logit_phi) / (1 + exp(logit_phi))
  
  for (i in 1:Nquarter){
    # First time-step. Entire population is suscetpible
    S_it_daily[1, i] <- N_i_daily[i]
    
    # Seed 3 infectious cases at first timesetp
    I_prev[1, i] <- ifelse(i==5 || i == 8 || i == 9,1,0)
    
    for (j in 1:Nquarter){
  
      # For each force of infection (foi), draw log_beta from
      # normal with hyperprior params
      log_beta_1[i, j] ~ dnorm(mu1, tau1) 
      log_beta_2[i, j] ~ dnorm(mu2, tau2) 
      
      beta_1[i, j] <- exp(log_beta_1[i, j])
      beta_2[i, j] <- exp(log_beta_2[i, j])
      
      # Only store the appropriate beta. This does not seem efficient since we
      #always draw a beta_1 and beta_2, even though only 1 will be used
      beta[i, j] <- ifelse(i==j, beta_1[i, j], beta_2[i, j])
    } 
  }
   
   
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(beta[, i] * (I_prev[t, ])))
      lambdaR[t, i] <- I_prev[t, i] * gamma_b
      R_temp[t, i] <- lambdaR[t, i]
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
      llsim [t + 1, i] <- logdensity.pois(I_incidence[t + 1, i], (lambdaI[t, i]*phi))
      lik[t + 1, i] <- exp(llsim[t + 1, i])
    }
  }
  #data# Nsteps
  #data# N_i_daily
  #data# I_incidence
  #data# Nquarter
}
