data{
  int<lower=1> Nsteps[1];
  vector<lower=0> I_incidence[Nsteps];
  int<lower=0> Nquarter[1];
  int<lower=0> N_i_daily[Nquarter];
  
}

parameters{
  real<lower=0> gamma;
  real mu;
  real tau;
  real sigma;
  
}

transformed parameters{}

model{}
