# Author: Matthew Phelps
#Desc: Interpolate between weekly observations
# Dependicies: Data 1, Data 2, Data, 3, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata"
setwd(pc)
rm(list = ls())

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


# INTERPOLATING WITH SPLINES ----------------------------------------------
# See: http://goo.gl/GyUf5K
# Gives the prevalence at each day - not incidence
I_splined <- lapply(I_daily, na.spline)
I_splined <- as.data.frame(t(do.call(rbind.data.frame, I_splined)))
row.names(I_splined) <- NULL

# Remove negative values
I_splined[I_splined < 0] <- 0

# PLOT SPLINED--------------------------------------------------------------------
panel_data <- combined
panel_data$day_index <- (combined$week.id +1) * 7
panel_data <- dplyr::rename(panel_data, variable = quarter)


# Reshape to long format again:
I_splined_long <- melt(I_splined, id.vars = "day_index")
panel_plot <- ggplot() +
  geom_line(data = I_splined_long,
            aes(x = day_index, y = value,
                group = variable),
            color = "red",
            size = 1.2) +
  geom_line(data = panel_data,
            size = 1.2,
            color = 'black',
            linetype = 1,
            alpha = 0.3,
            aes(x = day_index, y = sick.total.week,
                group = variable)) +
  geom_point(data = panel_data,
             size = 3.2,
             color = "black",
             aes(x = day_index, y = sick.total.week,
                 group = variable)) +
  facet_wrap(~variable)
panel_plot



# INCIDENCE PER DAY -------------------------------------------
# This gives the number of new infections (incidence) at each time step.
# Allows for easier calcuations later since our observed weekly 
# data is incidence not prevelance

I_incidence_temp <- I_splined[, 2:9] / 7
I_incidence_temp$day_index <- 1:Nsteps

# Check summations to make sure we're on track
do.call(rbind.data.frame, lapply(I_incidence_temp, sum))
do.call(rbind.data.frame, lapply(I_it, sum))

# PLOT INCIDENCE ----------------------------------------------------------

# Reshape to long format again:
I_incidence_temp_long <- melt(I_incidence_temp, id.vars = "day_index")


panel_plot <- ggplot() +
  geom_line(data = I_incidence_temp_long,
            aes(x = day_index, y = value,
                group = variable),
            color = "red",
            size = 1.2) +
  geom_line(data = panel_data,
            size = 1.2,
            color = 'black',
            linetype = 1,
            alpha = 0.3,
            aes(x = day_index, y = sick.total.week,
                group = variable)) +
  geom_point(data = panel_data,
             size = 3.2,
             color = "black",
             aes(x = day_index, y = sick.total.week,
                 group = variable)) +
  facet_wrap(~variable)
panel_plot

# SAVE FOR JAGS -----------------------------------------------------------

Nsteps <- nrow(I_splined)
S_it_daily <- matrix(0, Nquarter, Nsteps)
I_incidence <- matrix(0, Nquarter, Nsteps)
N_i_daily <- matrix(0, Nquarter, Nsteps)


for (i in 2:(Nquarter+1)){
  I_incidence[i-1, ] <- I_incidence_temp[, i]
  N_i_daily[i-1, ] <- N_i[i-1, ]

  }

rownames(I_incidence) <- q_names[, 1]

# Make sure quarters are labeled correctly. Evaluates to T if correct:
check <- function() {
  if (sum(I_incidence[1, ]) == sum(I_it[,1])){
    print("CORRECT")
  } else{
    stop("Quarters to not match")
  }
}

check()



# SAVE OUTPUT -------------------------------------------------------------

dataList <- list(Nquarter=Nquarter,
                 S_it_daily = S_it_daily,
                 N_i_daily = N_i_daily,
                 I_incidence=I_incidence,
                 Nsteps=Nsteps)
rm(I_splined_long, I_it_long, I_splined, panel_plot,
   panel_data, S_it, I_it, combined, N_i, i, t, n,
   quarterID, check, I_daily_long, I_daily, I_incidence_temp_long, I_incidence_temp)
save(list = ls(), file = "Data_4.Rdata")


