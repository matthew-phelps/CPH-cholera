# Author: Matthew Phelps
#Desc: Prepare data from JAGS for simulations



# LOAD & PREP DATA ---------------------------------------------------------------
load(file = 'Data/Rdata/sim-model-5-data1.Rdata')
N_i_daily <- N_pop
q_names <- N_pop$quarter


# OBSERVED INCIDENCE DATA -------------------------------------------------
# Data into format for ggplot
addDay <- function(x){
  x1 <- data.frame(x)
  x1$day <- 1:112
  x1
}
tidyReps <- function(x) x %>% gather(quarter, I_new,1:9)

I_reps_plot <- I_reps %>%
  lapply(addDay) %>%
  lapply(tidyReps)


nameReplace <- function(x){
  x$quarter[x$quarter=="St..Annae.Oester"] <- "St. Annae Oester"
  x$quarter[x$quarter=="St..Annae.Vester"] <- "St. Annae Vester"
  x
}
I_reps_plot <- lapply(I_reps_plot, nameReplace)


# Turn list into one long df
for (i in 1:length(I_reps_plot)){
  I_reps_plot[[i]]$rep <- paste(i)
}


I_reps_plot <- do.call(rbind.data.frame, I_reps_plot)

# WEEKLY AVG --------------------------------------------------------------

# # Find daily avg incidence each week. Plot daily avg incidence at weekly
# # time-steps to use as our "observed" data.
# 
# weekly_avg <- combined
# weekly_avg$week.id <- 1:16
# weekly_avg$avg <- weekly_avg$sick.total.week/7
# weekly_avg <- dplyr::select(weekly_avg, c(quarter, week.id, avg))
# 



# INITIALIZE EMPTY DF -----------------------------------------------------


N_it <- matrix(NA, Nquarter, 1)
N_it[, 1] <- unique(combined$est.pop.1853)



# SAVE --------------------------------------------------------------------
# If in future we sample from posterior, keep "y" object that I remove below 
rm(N_i_daily)
gc()
# save(list = ls(), file = 'Data/Rdata/sim-model-5-data2.Rdata' )

