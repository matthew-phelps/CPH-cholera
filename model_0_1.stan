# Model 0.1 - Fitting Christianshavn only

model {
	# Beta log hypreprior distributions
	log_beta ~ dnorm(mu, tau);
	mu ~ dnorm(0, 0.0001)
	tau <- pow(sigma, -2)
	sigma ~ dunif(0, 1.5)
	
	# Beta priors
	beta <- exp(log_beta);
	
	# Phi prior
	logit_phi ~ dnorm(0, .001);
	phi <- exp(logit_phi) / (1 + exp(logit_phi));

	# First time-step
	lambda[1] <-  beta * (I_it[1]/phi + 0.01);
	S_it[1] <- N_i[1];


	# Lambda
	for (t in 2:(Nsteps-1)){
		lambda[t] <-  (S_it[t]  / N_i[t]) * (beta * (I_it[t]+ 0.01));
	}

	

	# Likelihood function
	for (t in 1:(Nsteps-1)){
		I_it[t+1] ~ dpois(lambda[t]);
		S_it[t+1] <- S_it[t] - (I_it[t] / phi);

	}

	   #data# Nsteps
	   #data# N_i
	   #data# I_it



	}
