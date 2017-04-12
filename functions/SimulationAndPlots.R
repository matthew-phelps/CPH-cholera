SimPlusOne <- function(loops, I_reps=I_reps, N_it=N_it,
                       betas_95hpd, phi_95hpd,
                       gamma_95hpd, seed = NULL){
  # Simulate 1 time-step ahead
  if(!is.null(seed)) set.seed(seed)
  # browser()
  Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
  LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
  R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_prev_vect <-    matrix(nrow = Nsteps, ncol = Nquarter)
  S_plus1_mat <-    matrix(nrow = Nsteps, ncol = Nquarter)
  
  I_new_plus1 <-  list(data.frame(matrix(data = NA, nrow = Nsteps, ncol = Nquarter)))
  store_prev <-   list()
  store_S <-      list() 
  # store_param <- list(beta=list(), phi=0, gamma=0)
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
        # browser()
        Lambda_est_pe[t, i] <- S_plus1_mat[t, i] / N_it[i] * sum(betas_95hpd[[z]][, i] * I_prev_vect[t, ])
        LambdaR[t, i] <- I_prev_vect[t, i] * gamma_95hpd[z, ]
        R_new[t, i] <- min(LambdaR[t, i], I_prev_vect[t, i]) # no more recovereds than infected
        
        I_data <- I_reps[[rand_realization[z]]][t, i] # Observed new cases data
        
        I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] * phi_95hpd[z, ] ) )
        I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_data / phi_95hpd[z, ]  - R_new[t, i]))
        
        S_temp <- S_plus1_mat[t, i] -    I_data /  phi_95hpd[z, ] # Should be I_reps instead?
        S_plus1_mat[t + 1, i] <- max(0, S_temp)
        
      }
    }
    # For each quarter: store sum of infections attributed to each quarter over
    # all time-steps. Also store parameters used in iteration so R values cane
    # be reconstructed
    # browser()
    # store_param$beta[[z]] <- betas_95hpd[[z]]
    # store_param$phi[z] <- phi_95hpd[z, ]
    # store_param$gamma[z] <- gamma_95hpd[z, ]
    store_prev[[z]] <- I_prev_vect
    store_S[[z]] <- S_plus1_mat
    I_new_plus1[[z]] <- data.frame(I_new)
    I_new_plus1[[z]]$sim_sum <- z
  }
  list(I_new_plus1 = I_new_plus1,
       store_prev = store_prev, store_S = store_S)
}


SimFromZero <- function(loops, I_reps=I_reps, N_it=N_it,
                        betas_95hpd, phi_95hpd,
                        gamma_95hpd, seed=NULL){
  # Simulate from t = 0
  if(!is.null(seed)) set.seed(seed)
  # browser()
  Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
  LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
  R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_prev_vect <-    matrix(nrow = Nsteps, ncol = Nquarter)
  S_plus1_mat <-    matrix(nrow = Nsteps, ncol = Nquarter)
  
  I_new_plus1 <-  list()
  store_prev <-   list()
  store_S <-      list() 
  store_param <- list(phi = 0, gamma = 0, beta = list())
  # Starting values
  I_prev_vect[1, ] <- I_reps[[1]][1,]
  I_prev_vect[1, c(5, 8, 9)] <- 1 # Init St.A.V & Ø + Nyb with cases
  I_prev_vect[1, 9] <- 1
  S_plus1_mat[1, ] <- N_it[, 1] # init all S
  
  # To sample from a random realization of the epidemic for each simulation:
  
  rand_realization <- sample(1:10, loops, replace = TRUE)
  
  for (z in 1:loops){
    for (t in 1:(Nsteps-1)){
      # browser()
      for(i in 1:Nquarter){
        # browser()
        Lambda_est_pe[t, i] <- S_plus1_mat[t, i] / N_it[i] * sum(betas_95hpd[[z]][, i] * I_prev_vect[t, ])
        LambdaR[t, i] <- I_prev_vect[t, i] * gamma_95hpd[z, ]
        R_new[t, i] <- min(LambdaR[t, i], I_prev_vect[t, i]) # no more recovereds than infected
        
        I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] ))
        I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_new[t, i] - R_new[t, i]))
        
        S_temp <- S_plus1_mat[t, i] -    I_new[t, i] /  phi_95hpd[z, ] # Should be I_reps instead?
        S_plus1_mat[t + 1, i] <- max(0, S_temp)
        
      }
    }
    # For each quarter: store sum of infections attributed to each quarter over
    # all time-steps
    #  browser()
    
    store_param$beta[[z]] <- betas_95hpd[[z]]
    store_param$phi[z] <- phi_95hpd[z, ]
    store_param$gamma[z] <- gamma_95hpd[z, ]
    store_prev[[z]] <- as_tibble(I_prev_vect)
    store_S[[z]] <- as_tibble(S_plus1_mat)
    I_new_plus1[[z]] <- as_tibble(I_new)
    I_new_plus1[[z]]$sim_sum <- z
  }
  store_prev <- store_prev[!sapply(store_prev, is.null)]
  store_S <- store_S[!sapply(store_S, is.null)]
  I_new_plus1 <- I_new_plus1[!sapply(I_new_plus1, is.null)]
  
  list(I_new_plus1 = I_new_plus1,
       store_prev = store_prev, store_S = store_S,
       store_param = store_param)
}

SimFromZeroPointValue <- function(loops, I_reps=I_reps, N_it=N_it,
                                  betas_95hpd, phi_95hpd,
                                  gamma_95hpd, seed=NULL){
  # Simulate from t = 0 using a single set of parameter values
  # Does not store simulations where epidemic did not catch, but stores record
  # of number of simulations that did not catch
  if(!is.null(seed)) set.seed(seed)
  # browser()
  Lambda_est_pe <-  matrix(nrow = Nsteps, ncol = Nquarter)
  LambdaR <-        matrix(nrow = Nsteps, ncol = Nquarter)
  R_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_new <-          matrix(nrow = Nsteps, ncol = Nquarter)
  I_prev_vect <-    matrix(nrow = Nsteps, ncol = Nquarter)
  S_plus1_mat <-    matrix(nrow = Nsteps, ncol = Nquarter)
  
  I_new_plus1 <-  list()
  store_prev <-   list()
  store_S <-      list() 
  # store_param <- list(phi = 0, gamma = 0, beta = list())
  # Starting values
  I_prev_vect[1, ] <- I_reps[[1]][1,]
  I_prev_vect[1, c(5, 8, 9)] <- 1 # Init St.A.V & Ø + Nyb with cases
  I_prev_vect[1, 9] <- 1
  S_plus1_mat[1, ] <- N_it[, 1] # init all S
  
  # To sample from a random realization of the epidemic for each simulation:
  
  rand_realization <- sample(1:10, loops, replace = TRUE)
  
  for (z in 1:loops){
    for (t in 1:(Nsteps-1)){
      # browser()
      for(i in 1:Nquarter){
        # browser()
        Lambda_est_pe[t, i] <- S_plus1_mat[t, i] / N_it[i] * sum(betas_95hpd[, i] * I_prev_vect[t, ])
        LambdaR[t, i] <- I_prev_vect[t, i] * gamma_95hpd
        R_new[t, i] <- min(LambdaR[t, i], I_prev_vect[t, i]) # no more recovereds than infected
        
        I_new[t, i] <- rpois(1, (Lambda_est_pe[t, i] * phi_95hpd) )
        I_prev_vect[t + 1, i] <- max(0, (I_prev_vect[t, i] + I_new[t, i] / phi_95hpd  - R_new[t, i]))
        
        S_temp <- S_plus1_mat[t, i] -    I_new[t, i] /  phi_95hpd # Should be I_reps instead?
        S_plus1_mat[t + 1, i] <- max(0, S_temp)
        
      }
    }
    # For each quarter: store sum of infections attributed to each quarter over
    # all time-steps
    #  browser()
    if(sum(I_new[1:111])>50){
      # store_param$beta[[z]] <- betas_95hpd[[z]]
      # store_param$phi[z] <- phi_95hpd[z, ]
      # store_param$gamma[z] <- gamma_95hpd[z, ]
      store_prev[[z]] <- as_tibble(I_prev_vect)
      store_S[[z]] <- as_tibble(S_plus1_mat)
      I_new_plus1[[z]] <- as_tibble(I_new)
      I_new_plus1[[z]]$sim_sum <- z
    }
  }
  store_prev <- store_prev[!sapply(store_prev, is.null)]
  store_S <- store_S[!sapply(store_S, is.null)]
  I_new_plus1 <- I_new_plus1[!sapply(I_new_plus1, is.null)]
  
  list(I_new_plus1 = I_new_plus1,
       store_prev = store_prev, store_S = store_S)
  # store_param = store_param)
}


SimDataToPlot <- function(simulation_data, nAhead = NULL){
  
  I_simulated_plus1 <- simulation_data$I_new_plus1 %>%
    bind_rows() %>%
    `colnames<-` (c(q_names, "sim_num")) %>%
    gather(quarter, I_simulated, 1:9)
  
  if(is.numeric(nAhead)){
    I_simulated_plus1$day <- 1:(112-nAhead)
  } else {
    I_simulated_plus1$day <- 1:112
  }
  I_simulated_plus1[is.na(I_simulated_plus1)] <- 0
  return(I_simulated_plus1)
}

SimPlot <- function(simulation_data = NULL, observed_data,
                    alpha_sim = 0.01, alpha_data = 0.1,
                    color = "blue",
                    ci = NULL, ribbon = FALSE,
                    rib_col = "red", rib_alpha = 0.3){
  # Adding a CI ribbon wil cause the actual simulation lines to NOT be plotted
  
  if(!ribbon){
    plot_obj <- ggplot() + 
      geom_line(data = simulation_data, 
                alpha = alpha_sim,
                color = color,
                aes(x = day, y = I_simulated,
                    group = interaction(quarter, sim_num))) +
      facet_wrap(~quarter)
  }
  
  # Add 95% CI if provided
  if(!is.null(ci) & !ribbon){
    plot_obj <- plot_obj + 
      geom_line(data = ci,
                color = "red",
                linetype = "dashed",
                aes(x = day, y = `97.5%`)) +
      geom_line(data = ci,
                color = "red",
                linetype = "dashed",
                aes(x = day, y = `2.5%`)) +
      facet_wrap(~quarter)
  } else if(!is.null(ci) && ribbon){
    plot_obj <- ggplot() + 
      geom_ribbon(data = ci,
                  aes(x = day,
                      ymin=`2.5%`, ymax =`97.5%`),
                  alpha = rib_alpha,
                  fill = rib_col) +
      geom_line(data = ci,
                aes(x = day,
                    y = avg),
                color = "red",
                alpha = 0.9) +
      facet_wrap(~quarter)
  }
  
  
  plot_obj <- plot_obj +
    geom_line(data = observed_data,
              alpha = alpha_data,
              aes(x = day,
                  y = I_new, 
                  group = interaction(quarter, rep))) +
    geom_vline( xintercept = 40, linetype = 2,
                color = "black", alpha = 0.3, size = 0.6) +
    facet_wrap(~quarter) +
    labs(y = "Daily new infections") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    theme(legend.position = "none",
          panel.spacing.y = unit(2, "lines"))
  
  return(plot_obj)
}



SimPlotReps <- function(simulation_data, observed_data,
                        alpha_sim = 0.01, alpha_data = 0.1){
  ggplot() + 
    geom_line(data = simulation_data, 
              alpha = alpha_sim,
              size = 1,
              aes(x = day, y = I_simulated,
                  group = interaction(quarter, sim_num),
                  color = as.factor(sim_num))) +
    geom_line(data = observed_data,
              alpha = alpha_data,
              aes(x = day,
                  y = I_new, 
                  group = interaction(quarter, rep))) +
    facet_wrap(~quarter) +
    theme_minimal() +
    theme(legend.position = "none")
}




SimAndData <- function(num_sims, seed = NULL){
  # wrapper for the sim function and the data prep function 
  sim_tim <- SimFromZero(loops=num_sims, 
                         I_reps = I_reps, N_it = N_it,
                         betas_95hpd = mcmc_out$betas_95hpd,
                         phi_95hpd = mcmc_out$phi_95hpd,
                         gamma_95hpd = mcmc_out$gamma_95hpd, seed = seed)
  sim_tim<- SimDataToPlot(sim_tim)
}


SimCI <- function(sim_data_list){
  ## Calculate 95% CI on simulation output data, then get data into form for
  ## ggplot
  
  sim_data <- sim_data_list$sim_data %>%
    dplyr::select(-quarter, -day)
  column_holder <- sim_data_list$sim_data %>%
    dplyr::select(quarter, day)
  # if(ncol(sim_data)==4 | ncol(sim_data)==5){
  #   x <- sim_data %>%
  #     spread(key = sim_num, value = I_simulated)
  # }else{
  #   stop("simulation data not correct type")
  # }
  # Calculate 95%CI
  ci <- sim_data %>%
    t() %>% # need to apply fun to collums, not rows
    as_tibble() %>%
    sapply(., quantile, probs=c(0.025, 0.975)) %>%
    t() %>%
    as_tibble()
  
  median <- sim_data %>%
    t() %>% # need to apply fun to collums, not rows
    as_tibble() %>%
    sapply(., median)
  
  avg <- sim_data %>%
    t() %>% # need to apply fun to collums, not rows
    as_tibble() %>%
    sapply(., mean)
  
  # Add names back to Ci output
  summary_obj <- ci %>%
    add_column(median = median,
               avg = avg,
               quarter = column_holder$quarter,
               day = column_holder$day)
  return(list(prob_outbreak = sim_data_list$prob_outbreak,
              sim_summary = summary_obj))
}



rmNonOutbreaks <- function(sim_data, min_cases) {
  if(ncol(sim_data)==4 | ncol(sim_data)==5){
    # cols = simulations / rows = quarter-days
    x <- sim_data %>%
      spread(key = sim_num, value = I_simulated)
    
  }else{
    stop("simulation data not correct type")
  }
  
  indexOutbreaks <- function(x) ifelse(x>min_cases, TRUE, FALSE)
  
  column_holder <- dplyr::select(x, quarter, day)
  sim_data_tmp <- dplyr::select(x, -quarter, -day)
  
  index_outbreaks <- sim_data_tmp %>%
    colSums() %>%
    indexOutbreaks()
  out <- sim_data_tmp[, index_outbreaks]
  
  prob_outbreak <- sum(index_outbreaks) / ncol(sim_data_tmp)
  
  # Check to make sure non-outbreaks removed
  stopifnot(!all(colSums(out)<min_cases))
  out <- cbind(column_holder, out)
  
  return(list(prob_outbreak = prob_outbreak,
              sim_data = out))
}



