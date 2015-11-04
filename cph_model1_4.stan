//# To relax Model 1.1, we allowed for each location to have an independent internal transmission coefficient (βi)
//# and only allowed for one external transmission coecient shared by all locations (α). The natural logarithm
//# of all transmission coecients here were assigned independent N(0,   1/0.001 ) priors.



data {
	int<lower=0> Nquarter; 	//# number of quarters (=13)
	int<lower=0> n;			//# no. of observations (=208)
	int<lower=0> Nsteps;	//# no. of time steps (=16)
	int<lower=0> I_ti[Nquarter, Nsteps];		//# no. infected at each observation
	int<lower=0> I_tj[Nquarter, Nsteps];
	real<lower=0> frac_suseptible_it[Nquarter, Nsteps];
}

parameters {
	real log_beta[Nquarter, Nquarter];
	real logit_phi;
}

transformed parameters {
	real <lower=0> beta[Nquarter, Nquarter];
	real <lower=0> phi;
	real <lower=0> lambda[Nquarter, Nsteps];
	phi <- exp(logit_phi) / (1 + exp(logit_phi));
	for (i in 1:Nquarter){
		for (j in 1:Nquarter){
			beta[i, j] <- exp(log_beta[i, j]);
		}
	}

	for (i in 1:Nquarter){
		for (t in 1:Nsteps){
			lambda[i, t] <- frac_suseptible_it[i, t] * phi * (beta[i]*I_ti[i, t] + alpha[i]*I_tj[i, t]) ;	
		}
	}
}

model {
	for (i in 1:Nquarter{
		for (j in 1:Nquarter){
			log_beta[i, j] ~ normal(0, 1/0.001);
		}
	}

	logit_phi ~ normal(0, 1);
	for (i in 1:Nquarter){
		for (t in 1:Nsteps-1){
			I_ti[i, t+1] ~ poisson(lambda[i, t]);
		}
	}			
}
