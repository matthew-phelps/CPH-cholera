# Author: Matthew Phelps
#Desc: Interpolate between weekly observations
# Dependicies: Data 1, Data 2, Data, 3, 5_GLM_data_reshape



# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\data\\Rdata"
setwd(pc)
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

ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\splined_panel.tiff',
       plot = panel_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)





# NORMALIZED PR FOR EACH DAY ----------------------------------------------
# find normalized incidence during each day of the week
# q_i = p_i / sum_over_i(p_i)

q_names[] <- lapply(q_names, as.character)
# Get data by week for each quarter
p_i_ls <- lapply(I_splined[, q_names[, 1]], matrix, nrow = 7)

# Sum of prevalence per week
p_i_sum_ls <- lapply(p_i_ls, colSums)

q_i <- p_i_ls
for (l in 1:length(p_i_sum_ls)){
  for (days in 1:7){
    for (weeks in 1:(Nsteps/7)){
      if (p_i_sum_ls[[l]][weeks] > 0){
        q_i[[l]][days, weeks] <- p_i_ls[[l]][days, weeks] / p_i_sum_ls[[l]][weeks]
      }
      else {
        q_i[[l]][days, weeks] <- 0
      }
    }
  }
}

rm(p_i_sum_ls, p_i_ls, l, days, weeks)


# Re-distribute the incidence across the week -----------------------------
# We will re-distribute the observed weekly incidence to each day
# based upon the normalized pr of observing an infection on each day

I_incidence <- q_i

for (l in 1:length(q_i)){
  for (weeks in 1:(Nsteps/7)){
    if (I_it[weeks, l] == 0){
      I_incidence[[l]][, weeks] <- 0
    } else {
      I_incidence[[l]][, weeks] <- rowMeans(rmultinom(n = 100000, size = I_it[weeks, l], prob = q_i[[l]][, weeks]))
    }
  }
}
rm(l, weeks)

I_incidence <- lapply(I_incidence, matrix, nrow = Nsteps)
I_in <- do.call(rbind.data.frame, I_incidence)
row.names(I_in) <- NULL
I_incidence <- as.data.frame(matrix(data = I_in[, "V1"], nrow = Nsteps, ncol = Nquarter))
colnames(I_incidence) <- q_names[,1]
I_incidence$day_index <- 1:Nsteps
rm(I_in)



# PLOT INCIDENCE ----------------------------------------------------------
I_incidence_long <- melt(I_incidence, id.vars = "day_index")
panel_plot <- ggplot() +
  geom_line(data = I_incidence_long,
            aes(x = day_index, y = value,
                group = variable),
            color = "red",
            size = 1.2) +
  theme_minimal() +
#   geom_line(data = panel_data,
#             size = 1.2,
#             color = 'black',
#             linetype = 1,
#             alpha = 0.3,
#             aes(x = day_index, y = sick.total.week,
#                 group = variable)) +
#   geom_point(data = panel_data,
#              size = 3.2,
#              color = "black",
#              aes(x = day_index, y = sick.total.week,
#                  group = variable)) +
  facet_wrap(~variable)
panel_plot



# SAVE OUTPUT -------------------------------------------------------------

dataList <- list(Nquarter=Nquarter,
                 S_it_daily = S_it_daily,
                 N_i_daily = N_i_daily,
                 I_incidence=I_incidence,
                 I_prev = I_prev,
                 Nsteps=Nsteps)
rm(I_splined_long, I_it_long, I_splined, panel_plot,
   panel_data, S_it, I_it, combined, N_i, i, t, n,
   quarterID, check, I_daily_long, I_daily, I_incidence_temp_long, I_incidence_temp)
save(list = ls(), file = "Data_4.Rdata")


