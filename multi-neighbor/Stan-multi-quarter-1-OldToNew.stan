
// # model that only incorporates the beta term - no alpha yet
  
  data {
    int<lower=0> Nquarter; 	//# number of quarters (=13)
    int<lower=0> n;			//# no. of observations (=208)
    int<lower=0> Nsteps;	//# no. of time steps (=16)
    real<lower=0> S_t[Nsteps, Nquarter];		//# no. susceptible at each observation
    int<lower=0> I_incidence[Nsteps];		//# no. infected at each observation
    real<lower=0> R_new[Nsteps, Nquarter];		//# no. recovered at each observation
    real<lower=0> N_i_daily[Nquarter];		//# total popsize
  }

parameters {
  real log_beta_1;
  real log_beta_2;
  real tau;
}

transformed parameters {
  		//# all transformed params need to be defined 1st in this
  real <lower=0> beta_1;
  real <lower=0> beta_2
  real <lower=0> lambdaI[Nsteps, Nquarter];
  real <lower=0> lambdaR[Nsteps, Nquarter];
  
  beta_1 <- exp(log_beta_1);
  beta_2 <- exp(log_beta_2);
  
  for (k in 1:Nquarter){
    for (i in 1:Nquarter){
      // All external transmission coefficients are the same
      if ( k == i){
        beta[k, i] <- beta_1
      } else{ beta[k, i] <- beta_2}
    }
  }
  
  for (t in 1:(Nsteps-1)) {
    for (i in 1:Nquarter) {
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(beta[, i] * (I_prev[t, ])));
      lambdaR[t, i] <- I_prev[t, i] * gamma;
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i] - R_new[t, i]);
      S_it_daily[t+1, i] <- S_it_daily[t, i] - (I_incidence[t, i] / phi);	
    }
  }
}

model {
  tau ~ gamma(0.001, 0.001); //Check if this transfer from JAGS
  mu ~ normal (0, 100);
  log_beta_1 ~ normal(mu, tau);
  log_beta_2 ~ normal(mu, tau);
  for (t in 1:Nsteps-1){
    for (i in 1:Nquarter){
      I_t[t+1, i] ~ poisson(lambda[t, i]);
    }
  }			
}
