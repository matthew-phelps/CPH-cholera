data{
  int<lower=0> Nsteps[1];
  vector<lower=0> I_incidence[Nsteps];
  int<lower=0> Nquarter[1];
  int<lower=0> N_i_daily[Nquarter];
  
}

parameters{
  real<lower=0> gamma;
  real mu;
  real tau;
  real sigma;
  real log_beta_1;
  real log_beta_2;
  real<lower=0> logit_phi;
  int<lower=0> lambdaI ;
  int<lower=0> lambdaR;
}

transformed parameters{
  
}

model{}
