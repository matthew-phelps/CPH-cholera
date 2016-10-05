# A function to return the WAIC
# Also returns the effective number of parameters (p_waic), elpd and lpd as described by:
# www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf

# Note:  	mean_lik is the log of the (exponentiated) likelihoods
#			var_log_lik is the variance of the log likelihoods
#			these need separate monitors in JAGS

get_waic <- function(mean_lik, var_log_lik){
	
	stopifnot(length(mean_lik)==length(var_log_lik))
	stopifnot(all(mean_lik > 0))
	N <- length(mean_lik)
	
	lpd <- log(mean_lik)
	elpd <- lpd - var_log_lik
	waic <- -2 * elpd
	se <- (var(waic) / N)^0.5
	
	return(list(waic=-2*sum(elpd), p_waic=sum(var_log_lik), elpd=sum(elpd), lpd=sum(lpd), se_waic=se, pointwise=cbind(waic=waic, elpd=elpd, lpd=lpd)))
}

library('runjags')
library('rjags')

# For reproducibility:
set.seed(2016-10-03)

# A simple example:

# Simulate a multidimensional array of data:

m1 <- 'model{

	for(r in 1:R){
		for(c in 1:C){
		
			Obs[r,c] ~ dpois(lambda[r])
			
			# These are required to monitor the variance of the log likelihood:
			log_lik[r,c] <- logdensity.pois(Obs[r,c], lambda[r])
			# And the mean of the likelihood:
			lik[r,c] <- exp(log_lik[r,c])
			
		}
		
		# Model 1 has lambda as fixed:
		lambda[r] <- mean
	}
	
	# Priors
	mean ~ dmouch(1)
	shape ~ dmouch(1)
	# The dmouch distribution is implemented in runjags - see https://www.jstatsoft.org/article/view/v071i09

	#monitor# mean, shape
	#data# R, C, Obs
}'
m2 <- 'model{

	for(r in 1:R){
		for(c in 1:C){
		
			Obs[r,c] ~ dpois(lambda[r])
			
			# These are required to monitor the variance of the log likelihood:
			log_lik[r,c] <- logdensity.pois(Obs[r,c], lambda[r])
			# And the mean of the likelihood:
			lik[r,c] <- exp(log_lik[r,c])
			
		}
		
		# Model 2 has lambda as varying according to a Gamma distribution:
		lambda[r] ~ dgamma(shape, shape/mean)
	}
	
	# Priors
	mean ~ dmouch(1)
	shape ~ dmouch(1)
	# The dmouch distribution is implemented in runjags - see https://www.jstatsoft.org/article/view/v071i09

	#monitor# mean, shape
	#data# R, C, Obs
}'

R <- 20
C <- 10

# Simulate data as gamma-Poisson:
lambda <- rgamma(R, 1, rate=1/10)
Obs <- rpois(R*C, lambda)
dim(Obs) <- c(R,C)

# Fit model 1:
results <- run.jags(m1)
# Check convergence etc:
results
plot(results)
# Extend the model using rjags to get mean and variance monitors:
# (this won't be necessary when runjags is updated)
ll <- jags.samples(as.jags(results), c('lik', 'log_lik'), type=c('mean','variance'), 10000)
# Get the WAIC - for an array we need a small workaround to average over the chains:
mean_lik <- apply(ll$mean$lik,c(1,2),mean)
var_loglik <- apply(ll$variance$log_lik,c(1,2),mean)
waic1 <- get_waic(mean_lik, var_loglik)

# Fit model 2:
results <- run.jags(m2)
# Check convergence etc:
results
plot(results)
# Extend the model using rjags to get mean and variance monitors:
# (this won't be necessary when runjags is updated)
ll <- jags.samples(as.jags(results), c('lik', 'log_lik'), type=c('mean','variance'), 10000)
# Get the WAIC - for an array we need a small workaround to average over the chains:
mean_lik <- apply(ll$mean$lik,c(1,2),mean)
var_loglik <- apply(ll$variance$log_lik,c(1,2),mean)
waic2 <- get_waic(mean_lik, var_loglik)

waic1$waic
waic1$p_waic
waic2$waic
waic2$p_waic
# Model 2 has more parameters (the 20 rows have around 16.5 independent means), but a much better WAIC



# Now resimulate using a fixed lambda:
lambda <- 10
Obs <- rpois(R*C, lambda)
dim(Obs) <- c(R,C)

# Fit model 1:
results <- run.jags(m1)
# Check convergence etc:
results
plot(results)
# Extend the model using rjags to get mean and variance monitors:
# (this won't be necessary when runjags is updated)
ll <- jags.samples(as.jags(results), c('lik', 'log_lik'), type=c('mean','variance'), 10000)
# Get the WAIC - for an array we need a small workaround to average over the chains:
mean_lik <- apply(ll$mean$lik,c(1,2),mean)
var_loglik <- apply(ll$variance$log_lik,c(1,2),mean)
waic1 <- get_waic(mean_lik, var_loglik)

# Fit model 2:
results <- run.jags(m2)
# Check convergence etc:
results
plot(results)
# Extend the model using rjags to get mean and variance monitors:
# (this won't be necessary when runjags is updated)
ll <- jags.samples(as.jags(results), c('lik', 'log_lik'), type=c('mean','variance'), 10000)
# Get the WAIC - for an array we need a small workaround to average over the chains:
mean_lik <- apply(ll$mean$lik,c(1,2),mean)
var_loglik <- apply(ll$variance$log_lik,c(1,2),mean)
waic2 <- get_waic(mean_lik, var_loglik)

waic1$waic
waic1$p_waic
waic2$waic
waic2$p_waic
# Now model 1 has the better WAIC as expected (and far fewer parameters)





# Another example based on Andrew Gelman's 8 schools data, to match that used
# in the Vehtari and Gelman (2014) paper

# Example:

model <- "
model {
	for (j in 1:J){  						# J = the number of schools 
		y[j] ~ dnorm (theta[j], tau.y[j])	# data model: the likelihood
		theta[j] <- mu.theta + eta[j]
		tau.y[j] <- pow(sigma.y[j], -2)
		
		# These are required to monitor the variance of the log likelihood:
		log_lik[j] <- logdensity.norm(y[j], theta[j], tau.y[j])
		# And the mean of the likelihood:
		lik[j] <- exp(log_lik[j])
	}
	for (j in 1:J){
		eta[j] ~ dnorm (0, tau.theta)
	}
	tau.theta <- pow(sigma.theta, -2)
	sigma.theta ~ dhalfcauchy(prior.scale)  # The dhalfcauchy distribution is also implemented in runjags
	mu.theta ~ dnorm (0.0, 1.0E-6)			# noninformative prior on mu
	#data# J, y, sigma.y, prior.scale
	#monitor# theta, mu.theta, sigma.theta
}"

# Data as used by Gelman:
schools <-
structure(list(school = structure(1:8, .Label = c("A", "B", "C",
"D", "E", "F", "G", "H"), class = "factor"), estimate = c(28L,
8L, -3L, 7L, -1L, 1L, 18L, 12L), sd = c(15L, 10L, 16L, 11L, 9L,
11L, 10L, 18L)), .Names = c("school", "estimate", "sd"), class = "data.frame", row.names = c(NA,
-8L))


# In the Vehtari and Gelman 2014 paper they try scaling the data to show the effect of this:
N <- 20
data_scale <- seq(0.1,5,length=N)
params=elpd <- numeric(N)

for(i in 1:N){

	J <- nrow(schools)
	y <- schools$estimate * data_scale[i]
	sigma.y <- schools$sd
	prior.scale <- 25
	
	results <- run.jags(model, sample=10000)
	ll <- jags.samples(as.jags(results), c('lik', 'log_lik'), type=c('mean','variance'), 10000)
	w <- get_waic(as.mcmc(ll$mean$lik)[,1], as.mcmc(ll$variance$log_lik)[,1])
	
	params[i] <- w$p_waic
	elpd[i] <- w$elpd
	
}

par(mfrow=c(1,2))
plot(data_scale, elpd, type='l', ylim=c(-45, -28))
plot(data_scale, params, type='l', ylim=c(0,14))
# CF figure 2 of Vehtari and Gelman 2014 (to ensure our calculations are correct!)
