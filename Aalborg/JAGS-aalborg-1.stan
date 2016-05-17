# Same as CPH model-1 - we just have 1 "quarter" for Aalborg
model {
  # Gamma
  gamma ~ dexp(5)
  
  # One hyperprior for entire city
  mu ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
  sigma <- pow(tau, -0.5) # Do I need this?
  
  # 1 Internal force, same for every quarter
  log_beta_1 ~ dnorm(mu, tau)
  beta_1 <- exp(log_beta_1)
  
  # 1 external force, same for every quarter
  log_beta_2 ~ dnorm(mu, tau)
  beta_2 <- exp(log_beta_2)
  
  # Phi - under reporting fraction (same for all quarters)
  logit_phi ~dnorm(0, 0.001)
  phi<- exp(logit_phi) / (1 + exp(logit_phi))
  
  for (k in 1:Nquarter){
    # First time-step
    S_it_daily[1, k] <- N_i_daily[k];
    
    # Asign 1 infected person into both
    I_prev[1, k] <- 1
    #I_prev[1, i] <- ifelse(i==5,1,0)
    
    for (i in 1:Nquarter){
      # All external transmission coefficients are the same
      beta[k, i] <- ifelse(k==i, beta_1, beta_2)
    } 
  }
  
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(beta[, i] * (I_prev[t, ])))
      lambdaR[t, i] <- I_prev[t, i] * gamma
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i] - R_new[t, i])
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
