combChains <- function(x, testing=testing){
  # Combine models into 1 obj, each with 4 chains, then collapse 4 chains
  # together
  if(testing){
    y <- x %>%
      combine.MCMC(return.samples = 100) %>%
      combine.MCMC()
  } else {
    y <- x %>%
      combine.MCMC() %>%
      combine.MCMC()
  }
  gc()
  y
}


getMedianLog <- function(x){
  x <- log(x)
  # Get median values for each parameter
  mcmc_median <- apply(x, MARGIN = 2, FUN = median)
  mcmc_names <- names(x[1, ]) # name the rows
  return(list(mcmc_median = mcmc_median, mcmc_names = mcmc_names))
}

getSDLog <- function(x){
  # browser()
  x <- log(x)
  mcmc_sd <- apply(x, MARGIN = 2, FUN = sd)
  mcmc_names <- names(x[1, ]) # name the rows

  return(list(mcmc_sd = mcmc_sd, mcmc_names = mcmc_names))
}

getMedian <- function(x){
  # Get median values for each parameter
  mcmc_median <- apply(x, MARGIN = 2, FUN = median)
  mcmc_names <- names(x[1, ]) # name the rows
  return(list(mcmc_median = mcmc_median, mcmc_names = mcmc_names))
}

getSD <- function(x){
  # browser()
  mcmc_sd <- apply(x, MARGIN = 2, FUN = sd)
  mcmc_names <- names(x[1, ]) # name the rows
  
  return(list(mcmc_sd = mcmc_sd, mcmc_names = mcmc_names))
}

mkDfMcmc <- function(mcmc_object) data.frame(as.matrix(mcmc_object, iters = FALSE))

mkDf <- function(x){
  # browser()
  dimx <- sqrt(length(x))
  data.frame(matrix(x, nrow = dimx, ncol = dimx))
}

mkBetas <- function(mcmc_median){
  # Convert to matrix format for easier reading
  betas_temp <- mcmc_median[1:81]
  mkDf(betas_temp)
}

matOrderFun <- function(x) {
  x[order(rownames(x)), order(colnames(x))]
}

checkOrder <- function(betas, q_names){
  # Re-order matrix rowNAMES and columnNAMES alphabetically
  matNames <- function(x, q_names) {
    rownames(x) <- q_names
    colnames(x) <- q_names
    x
  }
  betas %>%
    matNames(q_names) %>%
    matOrderFun()
}



mcmcSummary <- function(x, q_names, testing = FALSE){
  # Produce mcmc data frame and hpd for each parameter
  mcmc_obj <- combChains(x, testing = testing)
  # browser()
  mcmc_df <- mkDfMcmc(mcmc_object = mcmc_obj)
  median_val_log <- mcmc_df %>%
    getMedianLog()
  sd_val_log <- mcmc_df %>%
    getSDLog()
  
  median_val <- mcmc_df %>%
    getMedian()
  sd_val <- mcmc_df %>%
    getSD()
  
  
  betas_median_log <- median_val_log$mcmc_median %>%
    mkBetas() %>%
    checkOrder(q_names = q_names)
  betas_sd_log <- sd_val_log$mcmc_sd %>%
    mkBetas() %>%
    checkOrder(q_names = q_names)
  # browser()
  phi_median <- median_val$mcmc_median['phi']
  gamma_median <- median_val$mcmc_median['gamma_b']
  
  phi_sd <- sd_val$mcmc_sd['phi']
  gamma_sd <- sd_val$mcmc_sd['gamma_b']
  gc()
  return(list(betas_median_log =betas_median_log,
              betas_sd_log = betas_sd_log,
              phi_median=phi_median,
              gamma_median=gamma_median,
              phi_sd = phi_sd,
              gamma_sd = gamma_sd))
}
