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

# INCIDENCE PER DAY -------------------------------------------
# This gives the number of new infections (incidence) at each time step.
# Allows for easier calcuations later since our observed weekly 
# data is incidence not prevelance

I_incidence_temp <- I_splined[, 2:9] / 7
I_incidence_temp$day_index <- 1:Nsteps
I_incidence_temp <- I_incidence_temp %>%
  select(day_index, everything())

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
I_prev <- matrix(0, Nquarter, Nsteps)
N_i_daily <- matrix(0, Nquarter, Nsteps)


for (i in 1:(Nquarter)){
  N_i_daily[i, ] <- N_i[i, ]
}
I_incidence <- t(I_incidence_temp[2:(Nquarter + 1)])
I_prev <- t(I_splined[, 2:(Nquarter + 1)])

rownames(I_incidence) <- q_names[, 1]
rownames(I_prev) <- q_names[, 1]
# Make sure quarters are labeled correctly. Evaluates to T if correct:
check <- function() {
  x <- matrix(NA, 0, 0)
  y <- matrix(NA, 0, 0)
  for(i in 1:Nquarter){
    x[i]<- round(sum(I_incidence[i, ]), digits = 0)
    y[i] <- sum(I_it[,i])
  } 
  result = list()
  result[[1]] <- x
  result[[2]] <- y
  result
  
}
check()


# NORMALIZED PR FOR EACH DAY ----------------------------------------------
# find normalized incidence during each day of the week
# q_i = p_i / sum_over_i(p_i)

# Just for Christianshavn - need to exapnd for all quarters

# Initialize
# 

p_i <- matrix(data = I_splined[, "St. Annae Vester"], nrow = 7)
p_i_sum <- colSums(p_i)
q_i <- matrix(NA, nrow = dim(p_i)[1], ncol = dim(p_i)[2])

for (i in 1:7){
  for (j in 1:length(p_i_sum)){
    q_i[i, j] <- p_i[i, j] / p_i_sum[j]
  }
}

# Function fo get p_i and Q_i for each quarter:
q_names[] <- lapply(q_names, as.character) # converts q_names to character df

# Get data by week for each quarter
p_i_ls <- lapply(I_splined[, q_names[, 1]], matrix, nrow = 7)

# Sum of prevalence per week
p_i_sum_ls <- lapply(p_i_ls, colSums)

q_i <- p_i_ls
for (l in 1:length(p_i_sum_ls)){
  for (days in 1:7){
    for (weeks in 1:length(p_i_sum_ls[[l]])){
     q_i[[l]][days, weeks] <- p_i_ls[[l]][days, weeks] / p_i_sum_ls[[l]][weeks] 
    }
  }
}






x <-names(p_i_ls[1])
ncol(p_i_ls[[x]])

plot((p_i_sum))
print(p_i_sum, digits = 0)




# Re-distribute the incidence across the week -----------------------------
# We will re-distribute the observed weekly incidence to each day
# based upon the normalized pr of observing an infection on each day

I_incidence <- matrix(data = NA, nrow = 7, ncol = Nsteps/7)

for (i in 1:(Nsteps/7)){
  I_incidence[, i] <- rowMeans(rmultinom(n = 1000000, size = I_it[i, "St. Annae Vester"], prob = q_i[, i]))
}

i2 <- matrix(data = (I_incidence), nrow = Nsteps)

plot(i2, type = 'l')

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


