# Author: Matthew Phelps
#Desc: Interpolate between weekly observations
# Dependicies: Data 1, Data 2, Data, 3, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------
library(tidyverse)
library(plyr)
library(zoo) # For interpolation functions
library(data.table)
library(grid)


# LOAD DATA ---------------------------------------------------------------
source("Data-4-prepare-JAGS.R")
I_it <- data.frame(t(I_it))
colnames(I_it) <- q_names[,1]
I_it$day_index <- seq(from = 7, to = (Nsteps )* 7, length.out = Nsteps)


# DATA MUNGING ------------------------------------------------------

I_daily <- data.frame(matrix(data = NA, nrow = (Nsteps) * 7, ncol = Nquarter))
colnames(I_daily) <- t(q_names)
I_daily$day_index <- 1:nrow(I_daily)

I_it_long <- melt(data = I_it, id.vars = 'day_index')
I_daily_long <- data.table::melt(data = I_daily, id.vars = 'day_index')

I_daily_long <- merge(I_daily_long, I_it_long, by = c('day_index', 'variable'), all.x = T)
I_daily_long$value.x <- NULL
I_daily_long$value.y[which(I_daily_long$day_index == 1)] <- 0

Nsteps <- nrow(I_daily)

# To wide form
I_daily <- data.table::dcast(I_daily_long, day_index~variable)




# REPLICATE SETS ----------------------------------------------------------
# sample 1 - 7, N cases times, with replacement. Gives vector of N cases 
# with 1 - 7 repeated over and over. These are the case counts for each day?


# Sort I_daily_long by Quarter then day.index - this will allow easier binding downstream
I_daily_long <- I_daily_long[order(I_daily_long$variable, I_daily_long$day_index), ]
dayCount_fn <- function(x) {
  # Creates vector with a length == # of cases observed that week.
  # Each element of vector is a number (1-7) that represents the
  # day of the week to assign a single case
  z <- sample(1:7, x, replace = T)
  z
}

# Sampling:
n <- 10 # number of replicated datasets
replicate_list <- list() # to store the replicates lists
set.seed(13)
for (k in 1:n){
  days_list_1 <- list()
  for (i in 1:nrow(I_it_long)){
    # Loop through observed data and for each week apply dayCount_fn
    # to create a random distribution of cases over the week
    if (I_it_long$value[i] > 0) {
      days_list_1[i] <-as.data.frame(dayCount_fn(I_it_long$value[i]))
    } else {
      days_list_1[i] <- 0
    }
  }
  
  # For each replication, store results as 7xn_days df
  day_sum <- data.frame(cases = matrix(data = 0, nrow = 7, ncol = 1))
  for (j in 1:length(days_list_1)){
    for (i in 1:7){
      # Count number of cases for each week and quarter
      # assigned to each day
      day_sum[i, j] <- sum(days_list_1[[j]] == i )
    }
  }
  replicate_list[k] <- list(day_sum)
}


# Re-create original data structure, but with each row == one day
I_daily_replicate <- data.frame(cases = replicate_list[[1]][, 1])


I_daily_replicate_ls <- list()
for (k in 1:n){
  I_daily_replicate <- data.frame(cases = replicate_list[[1]][, 1])
  for (i in 2:ncol(replicate_list[[k]])){
    for (j in 1:7){
      I_daily_replicate <- rbind(I_daily_replicate, replicate_list[[k]][, i][j])
    }
  }
  I_daily_replicate_ls[k] <- list(I_daily_replicate)
}

# Add variables & re-order columns
I_daily_replicate$day_index <- I_daily_long$day_index
I_daily_replicate$quarter <- I_daily_long$variable
I_daily_replicate <- I_daily_replicate[, c("day_index", 'quarter', 'cases')]

I_multi_replicate <- I_daily_replicate
for (k in 1:n){
  
  I_multi_replicate <- cbind(I_multi_replicate, I_daily_replicate_ls[k])
}

colnames(I_multi_replicate)[3:(n+3)] <- paste("rep", 1:(n+1), sep = "") # http://goo.gl/v2XXAO

# VERIFY DATA MUNGING -----------------------------------------------------

# All statements should evaluate to TRUE if everything worked correctly

checkReps <- function(name){
  sum(I_multi_replicate$rep2[which(I_multi_replicate$quarter == paste(name)) ]) == combined$cum.sick[which(combined$quarter == name & combined$week.id == 15)]
}

check_vec <- NA
for (i in 1:nrow(q_names)){
  check_vec[i] <- (checkReps(paste(q_names[i,1])))
}

# Stop if summations of replicates do not equal summations of original data
stopifnot(all(check_vec))
rm(check_vec, checkReps, dayCount_fn)


# CLEAN -----------------------------------------------------

rm(I_it_long, S_it,
   I_it, N_i, i, n, quarterID,
   I_daily_long, I_daily, I_daily_replicate,
   I_daily_replicate_ls, replicate_list, j, k,
   days_list_1)



# SAVE OUTPUT -------------------------------------------------------------

# 
# save(list = ls(), file = "Data_4.Rdata")


