# Multible Betas (1 for each quarter) + 1 alpha for each quarter
 
model {
  # Gamma
  gamma_b ~ dexp(5)
  
  # One hyperprior for entire city
  mu ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)

  
  # Phi - under reporting fraction
  logit_phi ~dnorm(0, 0.001)
  phi<- exp(logit_phi) / (1 + exp(logit_phi))
  
  for (i in 1:Nquarter){
    # First time-step
    S_it_daily[1, i] <- N_i_daily[i];
    
    # Asign 1 infected person into both
    I_prev[1, i] <- 1
    #I_prev[1, i] <- ifelse(i==5,1,0)
    
    # An independent internal transmission coefficient
    log_beta_1[i] ~ dnorm(mu, tau)
    beta_1[i] <- exp(log_beta_1[i])
    
    # Each quarter has 1 alpha
    log_beta_2[i] ~ dnorm(mu, tau)
    beta_2[i] <- exp(log_beta_2[i])
    for (j in 1:Nquarter){
      #  1 alpha for each quarter
      beta[i, j] <- ifelse(i==j, beta_1[i], beta_2[i])
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
      llsim [t + 1, i] <- logdensity.pois(I_incidence[t + 1, i], lambdaI[t, i])
      lik[t + 1, i] <- exp(llsim[t + 1, i])
    }
  }
  #data# Nsteps
  #data# N_i_daily
  #data# I_incidence
  #data# Nquarter
}
