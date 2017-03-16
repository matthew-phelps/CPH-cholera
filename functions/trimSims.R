
trimSims <- function(sim_data, cutoff, 
                     quarter_val = "St. Annae Vester",
                     day_val = 21){
  # Returns only simulations that acheived the cutoff number of cumulative
  # infections in the St. Annae Vester quarter by day 21
  
  stopifnot(!length(sim_data$sim_num) < 112) # need at least 1 full sim
  
  indexSimsByCutoff <- function(spread_sim_data, cutoff){
    # Index simulations that have taken off by day 21
    cutoff <= spread_sim_data %>%
      dplyr::filter(quarter == quarter_val &
                      day < day_val) %>%
      dplyr::select(-quarter, -day) %>%
      colSums()
  }
  
  
  colIndexToDataFrame <- function(col_inx){
    col_inx %>%
      names() %>%
      as.numeric() %>%
      as.data.frame()%>%
      mutate(inx = col_inx) %>%
      `colnames<-` (c("sim_num", "include"))
  }
  
  filterSimsByIndex <- function(sim_data, sim_inx){
    sim_data %>%
      dplyr::left_join(., sim_inx, by = "sim_num") %>%
      filter(include==TRUE)
  }
  
  sim_inx <- sim5_full_data %>%
    spread(., sim_num, I_simulated) %>%
    indexSimsByCutoff(cutoff) %>%
    colIndexToDataFrame()
  
  sim_filtered <- filterSimsByIndex(sim_data, sim_inx)
  
  pct_of_original <- sum(sim_inx$include) / nrow(sim_inx)
  
  
  return(list(pct_of_original = pct_of_original, sim_filtered = sim_filtered))
}