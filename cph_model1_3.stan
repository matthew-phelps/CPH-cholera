//# To relax Model 1.1, we allowed for each location to have an independent internal transmission coefficient (βi)
//# and only allowed for one external transmission coecient shared by all locations (α). The natural logarithm
//# of all transmission coecients here were assigned independent N(0,   1/0.001 ) priors.



data {
	int<lower=0> Nquarter; 	//# number of quarters (=13)
	int<lower=0> n;			//# no. of observations (=208)
	int<lower=0> Nsteps;	//# no. of time steps (=16)
	int<lower=0> I_ti[Nsteps, Nquarter];		//# no. infected at each observation
	int<lower=0> I_tj[Nsteps, Nquarter];
	real<lower=0> frac_suseptible_it[Nsteps, Nquarter];
}


parameters {
	real log_beta[Nquarter];
	real log_alpha[Nquarter];
	real logit_phi;
}

transformed parameters {
	real <lower=0> beta[Nquarter];
	real <lower=0> alpha[Nquarter];
	real <lower=0> phi;
	real <lower=0> lambda[Nsteps, Nquarter];
	for (i in 1:Nquarter){
		beta[i] <- exp(log_beta[i]);
		alpha[i] <- exp(log_alpha[i]);
	}
	for (i in 1:Nquarter){
		for (t in 1:Nsteps){
			lambda[t, i] <- frac_suseptible_it[t,i] * phi * (beta[i]*I_ti[t,i] + alpha[i]*I_tj[t,i]) ;	
		}
	}
}

model {
	log_alpha ~ normal(0, 1/0.001);
	log_beta ~ normal(0, 1/0.001);
	logit_phi ~ normal(0, 1);
	for (i in 1:Nquarter){
		for (t in 1:Nsteps-1){
			I_ti[t+1, i] ~ poisson(lambda[t, i]);
		}
	}			
}
