# Model 0_2 - Fitting all quarters with one beta1

model {
# Beta log hypreprior distributions

	# Phi prior
	logit_phi ~ dnorm(0, .001);
	phi <- exp(logit_phi) / (1 + exp(logit_phi));


	for (i in 1:Nquarter){
		log_beta[i] ~ dnorm(mu[i], tau[i]);
		mu[i] ~ dnorm(0, 0.0001)
		tau[i] <- pow(sigma[i], -2)
		sigma[i] ~ dunif(0, 1.5)
		beta[i] <- exp(log_beta[i]);


		# First time-step
		lambda[i, 1] <-  beta[i] * (I_it[i, 1]/phi + 0.0001);
		S_it[i, 1] <- N_i[i, 1];

		# Lambda & S at t + 1
		for (t in 2:(Nsteps-1)){
			lambda[i, t] <-  (S_it[i, t]  / N_i[i, t]) * (beta[i] * (I_it[i, t]));
		}

		# S_it
		for (t in 1:(Nsteps-1)){
			S_it[i, t+1] <- S_it[i, t] - (I_it[i, t] / phi);
		}


	# Likelihood function
	for (t in 1:(Nsteps-1)){
		I_it[i, t+1] ~ dpois(lambda[i, t]);
	}
}
	#data# Nsteps
	#data# N_i
	#data# I_it
	#data# Nquarter

}