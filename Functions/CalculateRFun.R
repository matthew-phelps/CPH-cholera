
Rint <- function(betas, lo_hpd, hi_hpd, gamma, q_names, 
                 order=TRUE){
  browser()
  # Find R for each mcmc chain (each combination of beta & gamma)
  gam_list <- as.list(gamma[,1]) # tun gamma into list for easy mapply
  Rest <- function(x,y){
    # browser()
    x / y
  }
  x <- mapply(Rest, betas, gam_list, SIMPLIFY = FALSE)
  
  # # Prove that function is calculating correctly
  # z <- betas[[2]] / gamma[2, ]
  # x[[2]] == z

  # Find highest and lowest values for each quarter
  
  
  
  
  B_int_low <- diag(lo_hpd)
  B_int_hi <- diag(hi_hdp)
  B_int <- betas %>%
    as.matrix() %>% # convert to matrix for diag() function to work
    diag()
  R_int <- data.frame(B_int / gamma)
  R_int <- R_int %>%
    `colnames<-` ("R_value") %>%
    mutate(lower = B_int_low / gamma,
           upper = B_int_hi /gamma,
           quarter = q_names,
           R_type = "int")
  if(order){
    # Order from highest to lowest
    R_int$quarter <- factor(R_int$quarter, levels = R_int$quarter[order(R_int$R_value)])
  }
  return(R_int)
}


Rext <- function(betas, lo_hpd, hi_hpd, gamma, q_names, 
                 order=TRUE){
  diag(lo_hpd) <- NA
  diag(hi_hdp) <- NA
  B_ext_low <- rowSums(lo_hpd, na.rm = T)
  B_ext_hi <- rowSums(hi_hdp, na.rm = T)
  
  B_ext <- betas %>% as.matrix()
  diag(B_ext) <- NA
  B_ext <- rowSums(B_ext, na.rm = T)
  
  R_ext <- data.frame(B_ext / gamma)
  R_ext <- R_ext %>%
    `colnames<-` ("R_value") %>%
    mutate(lower = B_ext_low / gamma,
           upper = B_ext_hi /gamma,
           quarter = q_names,
           R_type = "ext")
  if(order){
    # Order from highest to lowest
    R_ext$quarter <- factor(R_ext$quarter, levels = R_ext$quarter[order(R_ext$R_value)])
  }
  return(R_ext)
}



