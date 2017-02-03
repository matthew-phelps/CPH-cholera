# A function to return the WAIC - Sent to me by Matthew Denwood 
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
