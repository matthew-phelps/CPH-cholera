# Model 0.1 - Fitting Christianshavn only

model {
	# Beta log hypreprior distributions
	log_beta ~ dnorm(mu, tau);
	mu ~ dnorm(0, 0.0001)
	tau <- pow(sigma, -2)
	sigma ~ dunif(0, 1.5)
	
	# Beta
	beta <- exp(log_beta);
	
	# Phi prior
	logit_phi ~ dnorm(0, .001);
	
	# Phi
	phi <- exp(logit_phi) / (1 + exp(logit_phi));

	# Gamma
	gamma ~ dexp(5)

	# First time-step
#	lambdaI[1] <-  beta * (I_prev[1]/phi + 0.01);
#	lambdaR[1] <- I_prev[1] * gamma
	S_it_daily[1] <- N_i_daily[1];
	R_new[1] <- 0
	I_prev[1] ~ dnorm(0.666, 10)


	# Lambda I & R
	for (t in 1:(Nsteps-1)){
		lambdaI[t] <-  (S_it_daily[t]  / N_i_daily[t]) * (beta * (I_prev[t]));
		lambdaR[t] <- I_prev[t] * gamma
	}

	# S updates
	for (t in 1:(Nsteps-1)){
		S_it_daily[t+1] <- S_it_daily[t] - (I_incidence[t] / phi);
		I_prev[t+1] ~ dnorm(I_prev[t] + I_incidence[t] - R_new[t + 1], 10)

	}	

	# Likelihood function
	for (t in 1:(Nsteps-1)){
		I_incidence[t] ~ dpois(lambdaI[t]);
		R_new[t + 1] ~ dpois(lambdaR[t])
	}

	   #data# Nsteps
	   #data# N_i_daily

	   #data# I_incidence






	}
