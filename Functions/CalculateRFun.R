
RCalc <- function(betas, lo_hpd, hi_hpd, gamma, q_names, 
                  order=TRUE){
  # Find R for each mcmc chain (each combination of beta & gamma)
  # Order = T specifies that quarters are arranged alphabetically
  
  
  rForEachMcmc <- function(gamma){
    # R = beta / gamma
    gam_list <- as.list(gamma[,1]) # Make gamma into list for easy mapply
    Rest <- function(x,y) x / y 
    x <- mapply(Rest, betas, gam_list, SIMPLIFY = FALSE)
    return(x)
  }
  x <- rForEachMcmc(gamma)
  ## TEST that function is calculating correctly. Evaluates to TRUE
  stopifnot(all(x[[2]]== betas[[2]] / gamma[2, ]))
  
  rListToDataFrame <- function(x){
    # Convert earch mcmc iteraction set to one numeric vector of length = number
    # of parameters
    z1 <- lapply(x, function(x)  as.vector(as.matrix(x)))
    # Bind rows together so each row is mcmc iter
    z2 <- data.frame(matrix(unlist(z1), nrow = length(z1), byrow = T))
    return(z2)
  }
  
  r_vals <- rListToDataFrame(x)
  
  R_hi <- sapply(r_vals, quantile, 0.975)
  R_lo <- sapply(r_vals, quantile, 0.025)
  R_median <- sapply(r_vals, median)
  mkMat <- function(x){
    # browser()
    dimx <- sqrt(length(x))
    (matrix(x, nrow = dimx, ncol = dimx))
  }
  
  R_hi <- mkMat(R_hi)
  R_lo <- mkMat(R_lo)
  R_median <- mkMat(R_median)
  
  ## TEST that the rows and columns are in correct order
  stopifnot(quantile(r_vals$X2, 0.975) == R_hi[2,1] & quantile(r_vals$X10, 0.025) == R_lo[1,2])
  
  ## R - TOTAL
  rTotal <- function(R_lo, R_hi, R_median){
    R_tot <- R_median %>%
      rowSums(na.rm = TRUE) %>%
      data.frame()%>%
      `colnames<-` ("R_median") %>%
      mutate(lower =  rowSums(R_lo, na.rm = T),
             upper = rowSums(R_hi, na.rm = T),
             quarter = q_names,
             R_type = "tot")
    return(R_tot)
  }
  R_tot <- rTotal(R_lo, R_hi, R_median)
  
  
  ## R-INTERNAL
  R_int_low <- diag(R_lo)
  R_int_hi <- diag(R_hi)
  
  R_int <- data.frame(diag(R_median))
  R_int <- R_int %>%
    `colnames<-` ("R_median") %>%
    mutate(lower =R_int_low,
           upper = R_int_hi,
           quarter = q_names,
           R_type = "int")
  
  #### R EXT
  #### 
  diag(R_lo) <- NA
  diag(R_hi) <- NA
  R_ext <- R_median
  diag(R_ext) <- NA
  
  # browser()
  R_ext_lo <- rowSums(R_lo, na.rm = T)
  R_ext_hi <- rowSums(R_hi, na.rm = T)
  R_ext <- data.frame(rowSums(R_ext, na.rm = T))
  
  R_ext <- R_ext %>%
    `colnames<-` ("R_median") %>%
    mutate(lower = R_ext_lo,
           upper = R_ext_hi,
           quarter = q_names,
           R_type = "ext")
  
  if(order){
    # Order from highest to lowest in terms of R_INTERN
    R_int$quarter <- factor(R_int$quarter, levels = R_int$quarter[order(R_int$R_median)])
    R_ext$quarter <- factor(R_ext$quarter, levels = R_int$quarter[order(R_int$R_median)])
    R_tot$quarter <- factor(R_tot$quarter, levels = R_int$quarter[order(R_int$R_median)])
  }
  medianValsToDataFrame <- function(R_median){
    R_median <- R_median %>%
      data.frame%>%
      `colnames<-`(q_names) %>%
      `row.names<-`(q_names)
  }
  
  R_median <- medianValsToDataFrame(R_median)
  
  return(list(R_median = R_median,
              R_int = R_int,
              R_ext = R_ext,
              R_tot = R_tot))
  
}