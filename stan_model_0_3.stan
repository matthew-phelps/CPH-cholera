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
	lambdaI[1] <-  beta * (I_it_daily[1]/phi + 0.01);
	S_it_daily[1] <- N_i_daily[1];
	gamma[1] ~ dexp(5)
	lambdaR[1] <- I_it_daily[1] * gamma[1]
	R_new[1] <- 0

	# Lambda I
	for (t in 2:(Nsteps-1)){
		gamma[t] ~ dexp(1/5)
		lambdaI[t] <-  (S_it_daily[t]  / N_i_daily[t]) * (beta * (I_it_daily[t]+ 0.01));
		lambdaR[t] <- I_it_daily[t] * gamma[t]
	}

	# S and I updates
	for (t in 1:(Nsteps-1)){
		S_it_daily[t+1] <- S_it_daily[t] - (I_new[t] / phi);
		I_mu[t] <- I_new[t] + I_it_daily[t] - R_new[t + 1]

	}	

	# Likelihood function
	for (t in 1:(Nsteps-1)){
		I_new[t] ~ dpois(lambdaI[t]);
		R_new[t + 1] ~ dpois(lambdaR[t])
		I_it_daily[t + 1] ~ dnorm(I_mu[t], 10)
	}

	   #data# Nsteps
	   #data# N_i_daily
	   #data# I_it_daily





	}
