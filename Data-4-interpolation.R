# Author: Matthew Phelps
#Desc: Interpolate between weekly observations
# Dependicies: Data 1, Data 2, Data, 3, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------

graphics.off()
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")

setwd(wd.path)
rm(list = ls())
library(dplyr)
library(plyr)
library(zoo) # For interpolation functions
library(data.table)
library(ggplot2)
library(grid)


# LOAD DATA ---------------------------------------------------------------

load(file = "Data_3.Rdata")
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
  # Creates vector with a length = # of cases observed that week.
  # Each element of vector is a number (1-7) that represents the
  # day of the week to assign a single case
  set.seed(13)
  z <- sample(1:7, x, replace = T)
  z
}

# Sampling:
n <- 100 # number of replicated datasets
replicate_list <- list() # to store the replicates lists

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
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "Christianshavn") ]) == combined$cum.sick[which(combined$quarter == "Christianshavn" & combined$week.id == 15)]
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "Kjoebmager") ]) == combined$cum.sick[which(combined$quarter == "Kjoebmager" & combined$week.id == 15)]
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "Nyboder") ]) == combined$cum.sick[which(combined$quarter == "Nyboder" & combined$week.id == 15)]
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "Oester") ]) == combined$cum.sick[which(combined$quarter == "Oester" & combined$week.id == 15)]
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "Rosenborg") ]) == combined$cum.sick[which(combined$quarter == "Rosenborg" & combined$week.id == 15)]
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "St. Annae Oester") ]) == combined$cum.sick[which(combined$quarter == "St. Annae Oester" & combined$week.id == 15)]
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "St. Annae Vester") ]) == combined$cum.sick[which(combined$quarter == "St. Annae Vester" & combined$week.id == 15)]
sum(I_multi_replicate$rep1[which(I_multi_replicate$quarter == "Combined") ]) == combined$cum.sick[which(combined$quarter == "Combined" & combined$week.id == 15)]



# PLOT REPLICATE --------------------------------------------------------------------
panel_data <- combined
panel_data$day_index <- (combined$week.id +1) * 7
panel_data <- dplyr::rename(panel_data, quarter = quarter)


# Reshape to long format again:

panel_plot <- ggplot() +
  geom_line(data = I_multi_replicate,
            aes(x = day_index, y = rep1,
                group = quarter),
            color = "red",
            size = 1.2) +
  geom_line(data = panel_data,
            size = 1.2,
            color = 'black',
            linetype = 1,
            alpha = 0.3,
            aes(x = day_index, y = sick.total.week/7,
                group = quarter)) +
  geom_point(data = panel_data,
             size = 3.2,
             color = "black",
             aes(x = day_index, y = sick.total.week/7,
                 group = quarter)) +
  facet_wrap(~quarter)
panel_plot



ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\replicate_panel.tiff',
       plot = panel_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)




# AVERGAES ----------------------------------------------------------------
# Just to check that things are behaving as they should
day_avg <- data.frame(avg = rowMeans(I_multi_replicate[,3:(n+3)]))
day_avg$quarter <- I_daily_long$variable
day_avg$day_index <- I_daily_long$day_index



# CLEAN UP ----------------------------------------------------------------

rm(I_it_long, panel_plot, panel_data, S_it,
   I_it, combined, N_i, i, n, quarterID, day_avg,
   I_daily_long, I_daily, I_daily_replicate,
   I_daily_replicate_ls, replicate_list, j, k,
   days_list_1)



# SAVE OUTPUT -------------------------------------------------------------


save(list = ls(), file = "Data_4.Rdata")


