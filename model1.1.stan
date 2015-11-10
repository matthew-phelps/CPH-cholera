# To relax Model 1.1, we allowed for each location to have an independent internal transmission coefficient (βi)
# and only allowed for one external transmission coecient shared by all locations (α). The natural logarithm
# of all transmission coecients here were assigned independent N(0,   1/0.001 ) priors.

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
	logit_phi ~ dnorm(0, 0.01);
	phi <- exp(logit_phi) / (1 + exp(logit_phi));

	# First time-step
	for (i in 1:Nquarter){
		lambda[i, 1] <- 1 * sum(beta[i, ] * I_it[, 1]);
	}

	# Lambda
	for (t in 2:Nsteps){
		for (i in 1:Nquarter){
			lambda[i, t] <- ( (S_it[i, t-1]) - (I_it[i, t-1] / phi) )  / N_i[i, t] * sum(beta[i, ] * I_it[, t]);
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
