
RCalc <- function(betas, lo_hpd, hi_hpd, gamma, q_names, 
                 order=TRUE){
  # Find R for each mcmc chain (each combination of beta & gamma)
  # Order = T specifies that quarters are arranged alphabetically
  
  # Make gamma into list for easy mapply
  gam_list <- as.list(gamma[,1]) 
  Rest <- function(x,y) x / y 
  x <- mapply(Rest, betas, gam_list, SIMPLIFY = FALSE)
  ## TEST that function is calculating correctly. Evaluates to TRUE
   stopifnot(all(x[[2]]== betas[[2]] / gamma[2, ]))
 
  # Convert earch mcmc iteraction set to one numeric vector
  z1 <- lapply(x, function(x)  as.vector(as.matrix(x)))
  
  # bind rows together so each row is mcmc iter
  z2 <- data.frame(matrix(unlist(z1), nrow = length(z1), byrow = T))
  R_hi <- sapply(z2, quantile, 0.975)
  R_lo <- sapply(z2, quantile, 0.025)
  R_median <- sapply(z2, median)
  mkMat <- function(x){
    # browser()
    dimx <- sqrt(length(x))
    (matrix(x, nrow = dimx, ncol = dimx))
  }
  
  R_hi <- mkMat(R_hi)
  R_lo <- mkMat(R_lo)
  R_median <- mkMat(R_median)
  
  ## TEST that the rows and columns are in correct order
  quantile(z2$X2, 0.975) == R_hi[2,1] & quantile(z2$X10, 0.025) == R_lo[1,2]
  
  ## R - TOTAL
  ##
  # browser()
  R_tot_lo <- rowSums(R_lo, na.rm = T)
  R_tot_hi <- rowSums(R_hi, na.rm = T)
  R_tot <- data.frame(rowSums(R_median, na.rm = T))
  
  R_tot <- R_tot %>%
    `colnames<-` ("R_median") %>%
    mutate(lower = R_tot_lo,
           upper = R_tot_hi,
           quarter = q_names,
           R_type = "tot")
  
  
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
  
  
  return(list(R_int = R_int,
              R_ext = R_ext,
              R_tot = R_tot))
}
