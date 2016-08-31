# Model 6 - Water pipe connectivity. If quarters are connected, 
# beta = beta * eta, else beta = beta
# *non-reported cases are not infectious

model {
  # Gamma - rate of recovery from infectious state
  gamma_b ~ dexp(5)
  
  # Force of infection hyperprior
  # One hyperprior for entire city
  mu ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
  
  # Effect of hydraulic connection hyperprior
  mu_2 ~ dnorm(0, 0.001)
  tau_2 ~ dgamma(0.001, 0.001)
  
  # Effect of hydraulic connection
  eta ~ dnorm(mu_2, tau_2)
  
  # Phi - under reporting fraction
  logit_phi ~dnorm(0, 0.001)
  phi <- exp(logit_phi) / (1 + exp(logit_phi))
  
  # Create force of infection matrix and populate 1st time-step from data
  for (i in 1:Nquarter){
    # First time-step
    S_it_daily[1, i] <- N_i_daily[i];
    
    I_prev[1, i] <- ifelse(i==1,1,0) # 1 infected at t=1 for quarter 1
    
    for (j in 1:Nquarter){
      # Betas - force of infection (foi). Diagnols are internal foi,
      # off-diag are between-neighborhood foi.
      # For each, draw log_beta from normal with hyperprior params
      log_beta[i, j] ~ dnorm(mu, tau);

      # If there is a water-pipe connection b/w neighborhoods, foi = foi * eta
      b_temp[i, j] <- exp(log_beta[i, j])
      #beta[i, j] <- b_temp[i,j]
      beta[i, j] <- ifelse(water[i, j]==1, eta * b_temp[i, j], b_temp[i, j]);
    } 
  }
  
  
  # Lambda, I, S, & R
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(beta[, i] * (I_prev[t, ])))
      lambdaR[t, i] <- I_prev[t, i] * gamma_b
      # dpois(lambdaR[t, i]) took too long to fit, so just use lambdaR
      R_temp[t, i] <- lambdaR[t, i] 
      
      # Recovered cannot be greater than prevalence, so if R_temp > I_prev, 
      # R_new <- I_prev 
      R_new[t, i] <- min(I_prev[t, i], R_temp[t, i])
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i] - R_new[t, i])
      S_it_daily[t+1, i] <- S_it_daily[t, i] - (I_incidence[t, i] / phi)
    }
  }
  
  # Likelihood function
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      I_incidence[t+1, i] ~ dpois(lambdaI[t, i])
    }
  }
  #data# Nsteps
  #data# N_i_daily
  #data# I_incidence
  #data# Nquarter
  #data# water
  
  #Inits# inits_list
}
