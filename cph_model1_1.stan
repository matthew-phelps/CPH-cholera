//# In this model we only allowed for two unique transmission parameters; one for internal transmission (β) and
//# one for external transmission (i.e.   between areas, α).   We assigned non-informative normal priors centered
//# at zero (N(0,   1 0·001 )) for natural logarithm of both of these parameters.



data {
	int<lower=0> Nquarter; 	//# number of quarters (=13)
	int<lower=0> n;			//# no. of observations (=208)
	int<lower=0> Nsteps;	//# no. of time steps (=16)
	int<lower=0> I_ti[Nsteps, Nquarter];		//# no. infected at each observation
	int<lower=0> I_tj[Nsteps, Nquarter];
	real<lower=0> frac_suseptible_it[Nsteps, Nquarter];
}

parameters {
	real log_beta;
	real log_alpha;
	real logit_phi;
}

transformed parameters {
	real <lower=0> beta;
	real <lower=0> alpha;
	real <lower=0> phi;
	real <lower=0> lambda[Nsteps, Nquarter];
	beta <- exp(log_beta);
	alpha <- exp(log_alpha);
	phi <- exp(logit_phi) / (1 + exp(logit_phi));
	for (i in 1:Nquarter){
		for (t in 1:Nsteps){
			lambda[t, i] <- frac_suseptible_it[t,i] * phi * (beta*I_ti[t,i] + alpha*I_tj[t,i]) ;	
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
