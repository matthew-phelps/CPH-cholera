
// # model that only incorporates the beta term - no alpha yet
  
  data {
    int <lower=0> Nquarter; 	//# number of quarters (=13)
    int <lower=0> Nsteps;	//# no. of time steps (=112)
    int <lower=0> I_incidence[Nsteps, Nquarter];		//# no. infected at each observation

    int <lower=0> N_i_daily[Nquarter];		//# total popsize

  }
  
parameters {
  real log_beta_1;
  real log_beta_2;
  real mu1;
  real tau1;
  real mu2;
  real tau2;
  real logit_phi;
  real gamma_r;
}

transformed parameters {
  		//# all transformed params need to be defined 1st in this
  real <lower=0> beta_1;
  real <lower=0> beta_2;
  matrix <lower=0> [Nquarter, Nquarter] beta;
  real <lower=0> lambdaI[Nsteps, Nquarter];
  real <lower=0> lambdaR[Nsteps, Nquarter];
  matrix <lower=0>[Nsteps, Nquarter] I_prev;
  matrix <lower=0>[Nsteps, Nquarter] S_it_daily;
  real <lower=0> phi;
  
  
  beta_1 <- exp(log_beta_1);
  beta_2 <- exp(log_beta_2);
  phi <- exp(logit_phi) / (1 + exp(logit_phi));
  
  for (k in 1:Nquarter){
    for (i in 1:Nquarter){
      // All external transmission coefficients are the same
      if ( k == i){
        beta[k, i] <- beta_1;
      } else { beta[k, i] <- beta_2;
      }
    }
  }
  
  for (t in 1:(Nsteps-1)) {
    for (i in 1:Nquarter) {
      lambdaI[t, i] <-  (S_it_daily[t, i]  / N_i_daily[i]) * (sum(beta[, i] * (I_prev[t, ])));
      lambdaR[t, i] <- I_prev[t, i] * gamma_r;
      I_prev[t+1, i] <- (I_prev[t, i] + I_incidence[t, i] - round(lambdaR[t, i]));
      S_it_daily[t+1, i] <- S_it_daily[t, i] - (I_incidence[t, i] / phi);
      

    }
  }
}

model {
  // hyper-priors beta_1
  tau1 ~ cauchy(0, 5); // From: https://goo.gl/P5x3Kx
  mu1 ~ normal (0, 100);
  
  // hyper-priors beta_2
  tau2 ~ cauchy(0, 5);
  mu2 ~ normal(0, 100);
  
  log_beta_1 ~ normal(mu1, tau1);
  log_beta_2 ~ normal(mu2, tau2);
  gamma_r ~ exponential(5);
  logit_phi ~ normal(0, 100);
  for (t in 1:Nsteps-1){
    for (i in 1:Nquarter){
      I_incidence[t+1, i] ~ poisson(lambdaI[t, i]); // sampling from data
    }
  }
}
