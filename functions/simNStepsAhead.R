SimNStepsAhead <- function(itr_num, init_infected,
                           init_susceptible, N_it=N_it,
                           betas_95hpd, phi_95hpd,
                           gamma_95hpd, seed=seed,
                           Nahead = 7) {
  # Simulate from t = 0
  if(missing(init_infected))
    stop("Need to specify starting values for each quarter")
  if(!is.null(seed)) set.seed(seed)
  # browser()
  Lambda_est_pe <-  matrix(nrow = Nahead, ncol = Nquarter)
  LambdaR <-        matrix(nrow = Nahead, ncol = Nquarter)
  R_new <-          matrix(nrow = Nahead, ncol = Nquarter)
  I_new <-          matrix(nrow = Nahead, ncol = Nquarter)
  I_prev_vect <-    matrix(nrow = Nahead+1, ncol = Nquarter)
  S_plus1_mat <-    matrix(nrow = Nahead+1, ncol = Nquarter)
  
  
  
  # Starting values
  I_prev_vect[1, ] <- unlist(init_infected)
  S_plus1_mat[1, ] <- unlist(init_susceptible) # init all 
  
  # To sample from a random realization of the epidemic for each simulation:
  
  
  
  for (t in 1:7){
    # browser()
    for(i in 1:Nquarter){
      # browser()
      Lambda_est_pe[t, i] <- S_plus1_mat[t, i] / N_it[i] * sum(betas_95hpd[[itr_num]][, i] * I_prev_vect[t, ])
      LambdaR[t, i] <- I_prev_vect[t, i] * gamma_95hpd[itr_num, ]
      R_new[t, i] <- min(LambdaR[t, i], I_prev_vect[t, i]) # no more recovereds than infected
      
      I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] ) )
      I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_new[t, i] - R_new[t, i]))
      
      S_temp <- S_plus1_mat[t, i] -    I_new[t, i] /  phi_95hpd[itr_num, ] # Should be I_reps instead?
      S_plus1_mat[t + 1, i] <- max(0, S_temp)
      
    }
  }
  return(I_new = I_new[7, ])
}

nStepsAheahWrap <- function(loops, Nsteps, I_reps=I_reps, N_it=N_it,
                            betas_95hpd, phi_95hpd,
                            gamma_95hpd, seed = NULL){
  I_step <- matrix(NA, nrow = Nsteps-7, ncol = Nquarter)
  I_new_plus1 <-  list()
  store_prev <-   list()
  store_S <-      list() 
  store_param <- list(phi = 0, gamma = 0, beta = list())
  init_infected <- data.frame(matrix(0, nrow = Nsteps-7, ncol = Nquarter))
  rand_realization <- sample(1:10, loops, replace = TRUE)
  
  for (z in 1:loops){
    for(s in 1:(Nsteps-7)) {
      
      init_infected[s, ] <- I_reps[[rand_realization[z]]][s, ]
      sum_infect <- vapply(init_infected, sum, double(1))
      init_susceptible <- N_it[,1] - sum_infect
      
      
      I_step[s, ] <- SimNStepsAhead(itr_num = z,
                                    init_infected = init_infected[s, ],
                                    init_susceptible = init_susceptible,
                                    N_it = N_it, betas_95hpd = betas_95hpd,
                                    phi_95hpd = phi_95hpd,
                                    gamma_95hpd = gamma_95hpd, seed = seed)
      
    }
    
    I_new_plus1[[z]] <- as_tibble(I_step)
    I_new_plus1[[z]]$sim_num <- z
  }
  return(list(one = "one",
              I_new_plus1 = I_new_plus1))
}



a <-data.frame(matrix(1, nrow = 3, ncol=4))
a
cumsum(a[, 1])
