# Model 1.2 allows for each quarter to have it's own Phi

model {
	# Beta log hypreprior distributions
	for (i in 1:Nquarter){
		for (j in 1:Nquarter){
			log_beta[i, j] ~ dnorm(mu, tau);
		}
	}
	mu ~ dnorm(0, 0.001)
	tau <- pow(sigma, -2)
	sigma ~ dunif(0, 1.5)
	
	# Beta priors
	for (i in 1:Nquarter){
		for (j in 1:Nquarter){
			beta[i, j] <- exp(log_beta[i, j]);
		}
	}
	
	# Phi prior
	for (i in 1:Nquarter){
		logit_phi[i] ~ dnorm(0, 0.01);
		phi[i] <- exp(logit_phi[i]) / (1 + exp(logit_phi[i]));
	}

	# First time-step fix the 'fraction susceptible' at 1
	for (i in 1:Nquarter){
		lambda[i, 1] <- 1 * sum(beta[i, ] * I_it[, 1]);
	}

	# Lambda
	for (t in 2:Nsteps){
		for (i in 1:Nquarter){
			lambda[i, t] <- ( (S_it[i, t-1]) - (I_it[i, t-1] / phi[i]) )  / N_i[i, t] * sum(beta[i, ] * I_it[, t]);
		}
	}

	

	# Likelihood function
	for (i in 1:Nquarter){
		for (t in 1:(Nsteps-1)){
			I_it[i, t+1] ~ dpois(lambda[i, t]);
		}
	}			
	
	   #data# S_it
	   #data# Nquarter
	   #data# Nsteps
	   #data# N_i
	   #data# I_it


}
