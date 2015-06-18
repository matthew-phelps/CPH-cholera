data {
	int<lower=0> Nquarter; 	//# number of quarters (=13)
	int<lower=0> n;			//# no. of observations (=208)
	int<lower=0> Nsteps;	//# no. of time steps (=16)
	real<lower=0> S_t[Nsteps, Nquarter];		//# no. susceptible at each observation
	int<lower=0> I_t[Nsteps, Nquarter];		//# no. infected at each observation
	real<lower=0> R_t[Nsteps, Nquarter];		//# no. recovered at each observation
	real<lower=0> N_t[Nsteps, Nquarter];		//# total popsize
}

parameters {
	real <lower=0, upper=1.5> sigmaB;
	real beta0;
	real <lower=0> log_beta;
}

transformed parameters {
	real <lower=0> tauB;		//# all transformed params need to be defined 1st in this
	real <lower=0> beta;
	real <lower=0> lambda[Nsteps, Nquarter];
	tauB <- 1/square(sigmaB);
	beta <- exp(log_beta);
	for (i in 1:Nquarter){
		for (t in 1:Nsteps){
			lambda[t, i] <- (S_t[t,i] / N_t[t,i]) * (beta * I_t[t,i] + 0.001) ;	
		}
	}
}

model {
	sigmaB ~ uniform(0, 1.5);
	beta0 ~ normal(0, 100);
	log_beta ~ normal(beta0, tauB);
	for (i in 1:Nquarter){
		for (t in 1:Nsteps-1){
			I_t[t+1, i] ~ poisson(lambda[t, i]);
		}
	}			
}


generated quantities{

}