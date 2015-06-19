//# To relax Model 1.1, we allowed for each location to have an independent internal transmission coefficient (βi)
//# and only allowed for one external transmission coecient shared by all locations (α). The natural logarithm
//# of all transmission coecients here were assigned independent N(0,   1/0.001 ) priors.



data {
	int<lower=0> Nquarter; 	//# number of quarters (=13)
	int<lower=0> n;			//# no. of observations (=208)
	int<lower=0> Nsteps;	//# no. of time steps (=16)
	real<lower=0> S_ti[Nsteps, Nquarter];		//# no. susceptible at each observation
	int<lower=0> I_ti[Nsteps, Nquarter];		//# no. infected at each observation
	int<lower=0> I_tj[Nsteps, Nquarter];
	real<lower=0> R_t[Nsteps, Nquarter];		//# no. recovered at each observation
	real<lower=0> N_i[Nsteps, Nquarter];		//# total popsize
}

parameters {
	real <lower=0> log_beta;
	real <lower=0> log_alpha;
}

transformed parameters {
	real <lower=0> beta;
	real <lower=0> alpha;
	real <lower=0> lambda[Nsteps, Nquarter];
	beta <- exp(log_beta);
	alpha <- exp(log_alpha);
	for (i in 1:Nquarter){
		for (t in 1:Nsteps){
			lambda[t, i] <- (S_ti[t,i] / N_i[t,i]) * (beta[i]*I_ti[t,i] + sum(alpha[i]*I_tj[t,i])  ) ;	
		}
	}
}

model {
	log_alpha ~ normal(0, 1/0.001);
	log_beta ~ normal(0, 1/0.001);
	for (i in 1:Nquarter){
		for (t in 1:Nsteps-1){
			I_ti[t+1, i] ~ poisson(lambda[t, i]);
		}
	}			
}
