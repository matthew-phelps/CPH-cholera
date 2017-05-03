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


getMedian <- function(x){
  # Get median values for each parameter
  mcmc_median <- apply(x, MARGIN = 2, FUN = median)
  mcmc_names <- names(x[1, ]) # name the rows
  return(list(mcmc_median = mcmc_median, mcmc_names = mcmc_names))
}

getMean <- function(x){
  # Get median values for each parameter
  mcmc_mean <- apply(x, MARGIN = 2, FUN = mean)
  mcmc_names <- names(x[1, ]) # name the rows
  return(list(mcmc_mean = mcmc_mean, mcmc_names = mcmc_names))
}

getIntervals <- function(mcmc){
  int_hpd <- data.frame(HPDinterval(mcmc, 0.95))
  hi_hpd <- int_hpd$upper
  lo_hpd <- int_hpd$lower
  return(list(int_hpd = int_hpd, hi_hpd = hi_hpd, lo_hpd=lo_hpd))
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



mcmcPrep <- function(x, q_names, testing = FALSE){
  # Produce mcmc data frame and hpd for each parameter
  mcmc_obj <- combChains(x, testing = testing)
  
  int_hpd <- getIntervals(mcmc_obj)
  # browser()
  mcmc_df <- mkDfMcmc(mcmc_object = mcmc_obj)
  median_val <- mcmc_df %>%
    getMedian()
  mean_val <- mcmc_df %>%
    getMean()
  
  betas_median <- median_val$mcmc_median %>%
    mkBetas() %>%
    checkOrder(q_names = q_names)
  
  phi_median <- median_val$mcmc_median['phi']
  gamma_median <- median_val$mcmc_median['gamma_b']
  
  phi_mean <- mean_val$mcmc_mean['phi']
  gamma_mean <- mean_val$mcmc_mean['gamma_b']
  
  return(list(mcmc_df = mcmc_df,
              betas_median=betas_median,
              int_hpd=int_hpd,
              phi_median=phi_median,
              gamma_median=gamma_median,
              phi_mean = phi_mean,
              gamma_mean = gamma_mean))
}




smMcmc <- function(x){
  # Make vector of which MCMC draws result in parameter combinations that fall
  # within the 95% hpd for all parameters
  
  # Delete TRUE rows
  testHi <- function(x, hi_hpd) ifelse(x > hi_hpd, T, F)
  testLo <- function(x, lo_hpd) ifelse(x < lo_hpd, T, F)
  xrow <- x$mcmc_df %>%
    nrow()
  hi_hpd <- x$int_hpd$hi_hpd
  lo_hpd <- x$int_hpd$lo_hpd
  #browser()
  # Split mcmc df into two parts because memory constraints
  inx_hi_1 <- x$mcmc_df %>%
    slice(1:(xrow/4)) %>%
    mapply(testHi, ., hi_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  
  inx_hi_2 <- x$mcmc_df %>%
    slice(((xrow/4)+1):(xrow/2)) %>%
    mapply(testHi, .,  hi_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  inx_hi_3 <- x$mcmc_df %>%
    slice(((xrow/2)+1):(xrow*3/4)) %>%
    mapply(testHi, .,  hi_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  
  inx_hi_4 <- x$mcmc_df %>%
    slice(((xrow*3/4)+1):xrow) %>%
    mapply(testHi, .,  hi_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  inx_hi <- c(inx_hi_1, inx_hi_2, inx_hi_3, inx_hi_4)
  
  x_small <- x$mcmc_df[inx_hi, ]
  rm(x)
  xrow <- nrow(x_small)
  # browser()
  # Remove any MCMC that produce results below 95%hpd for any parameter
  inx_lo_1 <- x_small %>%
    slice(1:(xrow/4)) %>%
    mapply(testLo, ., lo_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  inx_lo_2 <- x_small %>%
    slice(((xrow/4)+1):(xrow/2)) %>%
    mapply(testLo, ., lo_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  inx_lo_3 <- x_small %>%
    slice(((xrow/2)+1):(xrow*3/4)) %>%
    mapply(testLo, ., lo_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  inx_lo_4 <- x_small %>%
    slice(((xrow*3/4)+1):xrow) %>%
    mapply(testLo, ., lo_hpd) %>%
    rowSums() %>%
    {ifelse(.>0, FALSE, TRUE)}
  
  inx_lo <- c(inx_lo_1, inx_lo_2, inx_lo_3, inx_lo_4)
  
  mcmc_95hpd <- x_small[inx_lo, ]
  rm(x_small)
  
  # If mcmc is not reduced enough, take only the first 50k 
  mcmc_95hpd <- mcmc_95hpd %>%
    slice(1:50000)
  
  # For betas, turn each mcmc row into a matrix
   browser()
  betas_95hpd <- mcmc_95hpd %>%
    dplyr::select(1:81) %>%
    apply(1, mkDf)
  
  
  phi_95hpd <- mcmc_95hpd %>%
    dplyr::select(phi)
  
  gamma_95hpd <- mcmc_95hpd %>%
    dplyr::select(gamma_b)
  phi_median <- x$phi_median
  gamma_median <- x$gamma_median
  betas_median <- x$betas_median
  phi_mean <- x$phi_median
  gamma_mean <- x$gamma_mean
  
  phi_range <- c(lo = lo_hpd[82], hi = hi_hpd[82])
  gamma_range <- c(lo = lo_hpd[83], hi = hi_hpd[83])
  
  return(list(phi_mean = phi_mean,
              gamma_mean = gamma_mean,
              phi_median = phi_median,
              gamma_median = gamma_median,
              betas_median = betas_median,
              phi_range = phi_range,
              gamma_range = gamma_range,
              phi_95hpd = phi_95hpd,
              gamma_95hpd = gamma_95hpd,
              betas_95hpd = betas_95hpd))
}
