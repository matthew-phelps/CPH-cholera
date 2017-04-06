listBetasToZero <- function(betas_list, quarter_num) {
  # List the quarter numbers that should have their interal transmission set to
  # 0
  mcmc_out$betas_95hpd %>%
    lapply(as.matrix) %>%
    lapply(diagToZero, quarter_num)
}

diagToZero <- function(x, quarter_num){
  diag(x)[quarter_num] <- 0
  x
}