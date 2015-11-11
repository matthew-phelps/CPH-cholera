# Model 0.1 - Fitting Christianshavn only

model {
	# Beta log hypreprior distributions
	log_beta ~ dnorm(mu, tau);
	mu ~ dnorm(0, 0.001)
	tau <- pow(sigma, -2)
	sigma ~ dunif(0, 1.5)
	
	# Beta priors
	beta <- exp(log_beta);
	
	# Phi prior
	logit_phi ~ dnorm(0, 0.01);
	phi <- exp(logit_phi) / (1 + exp(logit_phi));

	# First time-step
	lambda[1] <- 1 * beta;

	# Lambda
	for (t in 2:Nsteps){
			lambda[t] <- ( (S_it[t-1]) - (I_it[t-1] / phi) )  / N_i[t] * beta);
	}

	

	# Likelihood function
	for (t in 1:(Nsteps-1)){
			I_it[t+1] ~ dpois(lambda[t]);
	}			
	
	   #data# S_it
	   #data# Nquarter
	   #data# Nsteps
	   #data# N_i
	   #data# I_it


}
