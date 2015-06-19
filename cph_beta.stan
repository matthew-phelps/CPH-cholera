// # Each quarter has a separate beta term
// # Each time step for each quarter has a separate lambda term

data {
	int <lower=0> Nquarter; 	//# number of quarters (=7)
	int <lower=0> n;			//# no. of observations (=128)
	int <lower=0> Nsteps;	//# no. of time steps (=16)
	real <lower=0> S_ti[Nsteps, Nquarter];		//# no. susceptible at each observation
	int <lower=0> I_ti[Nsteps, Nquarter];		//# no. infected at each observation
	real <lower=0> R_t[Nsteps, Nquarter];		//# no. recovered at each observation
	real <lower=0> N_i[Nsteps, Nquarter];		//# total popsize
}

parameters {
	real <lower=0, upper=1.5> sigmaB[Nquarter];
	real beta0 [Nquarter];
	real <lower=0> log_beta[Nquarter];
}

transformed parameters {
	real <lower=0> tauB[Nquarter];		//# all transformed params need to be defined 1st in this
	real <lower=0> beta[Nquarter];
	real <lower=0> lambda[Nsteps, Nquarter];
	for (i in 1:Nquarter){				//# Is vectoization not supported for math functions?
		tauB[i] <- 1/square(sigmaB[i]);
	}
	for (i in 1:Nquarter){
		beta[i] <- exp(log_beta[i]);
	}
	for (i in 1:Nquarter){
		for (t in 1:Nsteps){
			lambda[t, i] <- (S_ti[t,i] / N_i[t,i]) * (beta[i] * I_ti[t,i] + 0.0001) ;	
		}
	}
}

model {
	sigmaB ~ uniform(0, 1.5);
	beta0 ~ normal(0, 1000);
	log_beta ~ normal(beta0, tauB);
	
	for (i in 1:Nquarter){
		for (t in 1:Nsteps-1){
			I_ti[t+1, i] ~ poisson(lambda[t, i]);
		}
	}			
}