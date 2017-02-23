# Copenhagen Simulation

simPlusOne <- function(loops, gamma=gamma, I_reps=I_reps, N_it=N_it){
  set.seed(13)
  # browser()
  gamma <- gamma
  Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
  LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
  R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_prev_vect <-    matrix(nrow = Nsteps, ncol = Nquarter)
  S_plus1_mat <-    matrix(nrow = Nsteps, ncol = Nquarter)
  
  I_new_plus1 <-  list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))
  store_prev <-   list()
  store_S <-      list() 
  
  # Starting values
  I_prev_vect[1, ] <- I_reps[[1]][1,]
  I_prev_vect[1, c(5, 8, 9)] <- 1 # Init St.A.V & Ø + Nyb with cases
  S_plus1_mat[1, ] <- N_it[, 1] # init all S
  
  # To sample from a random realization of the epidemic for each simulation:
  
  rand_realization <- sample(1:10, loops, replace = TRUE)
  
  for (z in 1:loops){
    for (t in 1:(Nsteps-1)){
      # browser()
      for(i in 1:Nquarter){
        
        Lambda_est_pe[t, i] <- S_plus1_mat[t, i] / N_it[i] * sum(betas[, i] * I_prev_vect[t, ])
        LambdaR[t, i] <- I_prev_vect[t, i] * gamma
        R_new[t, i] <- min(LambdaR[t, i], I_prev_vect[t, i]) # no more recovereds than infected
        
        I_data <- I_reps[[rand_realization[z]]][t, i] # Observed data
        
        I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] * phi ) )
        I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_data / phi  - R_new[t, i]))
        
        S_temp <- S_plus1_mat[t, i] -    I_data / phi # Should be I_reps instead?
        S_plus1_mat[t + 1, i] <- max(0, S_temp)
        
      }
    }
    # For each quarter: store sum of infections attributed to each quarter over
    # all time-steps
    #  browser()
    store_prev[[z]] <- I_prev_vect
    store_S[[z]] <- S_plus1_mat
    I_new_plus1[[z]] <- data.frame(I_new)
    I_new_plus1[[z]]$sim_sum <- z
  }
  list(I_new_plus1 = I_new_plus1,
       store_prev = store_prev, store_S = store_S)
}
