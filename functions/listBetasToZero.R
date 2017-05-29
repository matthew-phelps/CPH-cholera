listBetasToZero <- function(betas_list, quarter_num) {
  # List the quarter numbers that should have their interal transmission set to
  # 0
  mcmc_out$betas_95hpd %>%
    lapply(as.matrix) %>%
    lapply(diagToZero, quarter_num)
}

diagToZero <- function(x, quarter_num){
  if(is.null(quarter_num)){
    x
  } else{
    diag(x)[quarter_num] <- 0
    x
  }
}
# 
# wrapQuaterBetaToZero <- function(mcmc_obj, quart_num=NULL, n_loops, min_cases){
#   browser()
#   prob_vec <- vector(mode = "numeric", length = length(quart_num))
#   prob_vec <- rbind(lapply(quart_num, function(q){
#     out1 <- listBetasToZero(mcmc_obj$betas_95hpd, q)
#     out2 <- SimFromZero(loops=n_loops, 
#                         I_reps = I_reps, N_it = N_it,
#                         betas_95hpd = out1,
#                         phi_95hpd = mcmc_obj$phi_95hpd,
#                         gamma_95hpd = mcmc_obj$gamma_95hpd)
#     out3 <- out2 %>%
#       SimDataToPlot() %>%
#       rmNonOutbreaks(min_cases = min_cases) %>%
#       SimCI()
#     out3$prob_outbreak
#   }))
#   return(prob_vec)
# }


wrapQuaterBetaToZero <- function(mcmc_obj, quart_num = NULL, n_loops, min_cases,
                                 group = TRUE){
  # browser()
  if(group){
    if(is.null(quart_num)){
      # If no quarters are being set to zero, bypass that step
      out1 <- mcmc_obj$betas_95hpd
    } else {
      out1 <- listBetasToZero(mcmc_obj$betas_95hpd, quart_num)
    }
  
    out2 <- SimFromZero(loops=n_loops, 
                        I_reps = I_reps, N_it = N_it,
                        betas_95hpd = out1,
                        phi_95hpd = mcmc_obj$phi_95hpd,
                        gamma_95hpd = mcmc_obj$gamma_95hpd)
    out3 <- out2 %>%
      SimDataToPlot() %>%
      rmNonOutbreaks(min_cases = min_cases) %>%
      SimCI()
    return(out3$prob_outbreak)
  } else if (!group){
    prob_vec <- vector(mode = "numeric", length = length(quart_num))
    prob_vec <- map_dbl(quart_num, function(q){
      # browser()
      out1 <- listBetasToZero(mcmc_obj$betas_95hpd, q)
      out2 <- SimFromZero(loops=n_loops, 
                          I_reps = I_reps, N_it = N_it,
                          betas_95hpd = out1,
                          phi_95hpd = mcmc_obj$phi_95hpd,
                          gamma_95hpd = mcmc_obj$gamma_95hpd)
      out3 <- out2 %>%
        SimDataToPlot() %>%
        rmNonOutbreaks(min_cases = min_cases) %>%
        SimCI()
      out3$prob_outbreak
    })
    return(prob_vec)
  }
}