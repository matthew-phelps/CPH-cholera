
trimSims <- function(sim_data, cutoff){
  # Returns only simulations that acheived the cutoff number of cumulative
  # infections in the St. Annae Vester quarter by day 21
  indexSimsByCutoff <- function(x, cutoff){
    # Test simulations if they have taken off by day 21
    cutoff < x %>%
      dplyr::filter(quarter == "St. Annae Vester" &
                      day <21) %>%
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
  
  sim5_filtered <- filterSimsByIndex(sim_data, sim_inx)
  
  return(sim5_filtered)
}