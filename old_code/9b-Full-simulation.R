# Author: Matthew Phelps
#Desc: Full model from Initial time-step
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())

library(ggplot2)
library(reshape)
require(grid)

# LOAD data ---------------------------------------------------------------

load(file = 'data\\Rdata\\model_sim_data.Rdata')
set.seed(123)


# MODEL (STOCHASTIC) -------------------------------------------------------------------

# For each model run, create 8x8 matrix so each neighborhood pair has beta estimate
# Value is drawn from one row of MCMC posterior. Sampleing with replacement
loops <- 2000
I_est_list <- list()
S_it_est_list <- list()

for (z in 1:loops){
  
  # Use MCMC output from one MCMC draw
  step1 <- betas_matrix[sample(nrow(betas_matrix), 1), ]
  step2 <- do.call(rbind, step1)
  step3 <- matrix(step2, nrow = 8, ncol = 8, byrow = F)
  beta_sample_itr <- as.data.frame(step3)
  rm(step1, step2, step3)
  
  phi_sample <- phi_matrix[sample(nrow(phi_matrix), 1), ]
  
  
  # Estimate epidemic from initial state
  Lambda_est <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      Lambda_est[i, t] <- S_it_est[i, t] / N_it[i, t] *   sum(beta_sample_itr[i, ] * I_it_est[, t])
      I_it_est[i, t+1] <- rpois(1, Lambda_est[i, t])
      S_it_est[i, t+1] <- (S_it_est[i, t]) - (I_it_est[i, t] / phi_sample)
    }
  }
  I_est_list[[z]] <- I_it_est
  S_it_est_list[[z]] <- S_it_est
}



# STOCHASTIC DATA RESHAPING for PLOTTING ----------------------------------------------------------

# Infectious Data for all quarters (city_stoch level). Flatten each matrix
city_stoch <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_stoch$week_index <- 1:Nsteps
city_stoch$day_index <- city_stoch$week_index * 7
for (z in 1:loops){
  city_stoch[z] <- as.data.frame(colSums(I_est_list[[z]]))
}
city_stoch_melt <- melt(city_stoch, id.vars = 'week_index')




# Infectious for single quarter
I_quarter <-(I_est_list[[1]][1, ])
for (z in 2:(loops)){
  I_quarter <- rbind(I_quarter, I_est_list[[z]][1, ])
}
I_quarter <- as.data.frame(t(I_quarter))
I_quarter$week_index <- 1:Nsteps
I_quarter_melt <- melt(I_quarter, id.vars = 'week_index')


# Susceptible city_stoch-wide
city_stoch_S <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_stoch_S$week_index <- 1:Nsteps
for (z in 1:loops){
  city_stoch_S[z] <- as.data.frame(colSums(S_it_est_list[[z]]))
}
city_stoch_S_melt <- melt(city_stoch_S, id.vars = 'week_index')



# PLOTS -------------------------------------------------------------------

# Quartery Infectious (Christianshavn)
plot1 <- ggplot(data = I_quarter_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkred', alpha = 0.05) +
  ggtitle('Christianshavn simulated\n n = 2000')
plot1
ggsave(plot1, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\Christanshavn.pdf',
       width=15, height=9,
       units = 'in')


# city_stoch level Infectious
plot2 <- ggplot(data = city_stoch_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkgreen', alpha = 0.05) +
  ggtitle('city_stoch level simulated\n n = 2000')
plot2
ggsave(plot2, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\city_stoch-I.pdf',
       width=15, height=9,
       units = 'in')


# city_stoch level Infectious
plot3 <- ggplot(data = city_stoch_S_melt, aes(x = week_index, y = value, group = variable)) +
  geom_line(color = 'darkblue', alpha = 0.05) +
  ggtitle('city_stoch level Susceptible\n n = 2000')
plot3
ggsave(plot3, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\city_stoch-S.pdf',
       width=15, height=9,
       units = 'in')



# PE City-wide ------------------------------------------------------------



# MODEL (POINT ESTIMATES) -------------------------------------------------------------------

# Reshape parameters PE data



# Estimate epidemic from initial state

loops <- 3000
I_est_pe_list <- list()
S_it_est_pe_list <- list()
for (z in 1:loops){
  Lambda_est_pe <- matrix(data = 0, nrow = Nquarter, ncol = Nsteps)
  for (t in 1:(Nsteps-1)){
    for (i in 1:Nquarter){
      Lambda_est_pe[i, t] <- S_it_est[i, t] / N_it[i, t] *   sum(beta_pe[i, ] * I_it_est[, t])
      I_it_est[i, t+1] <- rpois(1, Lambda_est_pe[i, t])
      S_it_est[i, t+1] <- (S_it_est[i, t]) - (I_it_est[i, t] / phi_pe)
    }
  }
  I_est_pe_list[[z]] <- I_it_est
  S_it_est_pe_list[[z]] <- S_it_est
}




# PE RESHAPE DATA ---------------------------------------------------------

# Infectious Data for all quarters (city_pe level). Flatten each matrix
city_pe <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_pe$week_index <- 1:Nsteps
city_pe$day_index <- city_pe$week_index * 7

for (z in 1:loops){
  city_pe[z] <- as.data.frame(colSums(I_est_pe_list[[z]]))
}
city_pe_melt <- melt(city_pe, id.vars = 'day_index')


# Infectious for single quarter
I_quarter_melt_list <- list()
for (i in 1:Nquarter){
  I_quarter <-(I_est_pe_list[[1]][i, ])
  for (z in 2:(loops)){
    I_quarter <- rbind(I_quarter, I_est_pe_list[[z]][i, ])
  }
  I_quarter <- as.data.frame(t(I_quarter))
  I_quarter$week_index <- 1:Nsteps
  I_quarter$day_index <- 7 * I_quarter$week_index
  I_quarter$week_index <- NULL
  I_quarter$quarter_name <- q_names[i, 1]
  I_quarter_melt_list[[i]] <- melt(I_quarter, id.vars = c('day_index', 'quarter_name'))
}

# Merge all observations from all quarters into one long df
I_quarter_melt <- I_quarter_melt_list[[1]]
for (i in 2:Nquarter){
  I_quarter_melt <- rbind(I_quarter_melt, I_quarter_melt_list[[i]])
}
rm(I_quarter_melt_list)




# Prepare observed data aggregated to the week ----------------------------------------
load(file = "Rdata\\quarter_combined.Rdata")
Nsteps <- 16
quarterID <- as.numeric(combined$quarterID)
n <- as.numeric(length(quarterID))
Nquarter <- length(table(quarterID))
q_names <-as.data.frame(unique(combined$quarter))

S_it <- matrix(0, Nquarter, Nsteps)
I_it <- matrix(0, Nquarter, Nsteps)
N_i <- matrix(0, Nquarter, Nsteps)
for (i in 1:Nquarter){
  for( t in 1:Nsteps){
    S_it[i, t] <- (combined$S[which(combined$quarterID==i)])[t]
    I_it[i, t] <- (combined$sick.total.week[which(combined$quarterID==i)])[t]
    N_i[i, t] <- (combined$pop1855[which(combined$quarterID==i)])[t]
  }
}
row.names(I_it) <- q_names[, 1]

city_week <- as.matrix(colSums(I_it))
city_week <- as.data.frame(city_week)
city_week$week_index <- 1:Nsteps
city_week$day_index <- city_week$week_index * 7
city_week$week_index <- NULL


# city_pe level Infectious
plot4 <- ggplot() +
  geom_line(data = city_pe_melt,
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.05) +
  geom_line(data = city_week,
            aes(x = day_index, y = V1),
            color = 'darkred', alpha = 0.5, size = 1.3) +
  ggtitle('City (point estimates) \nsimulated n = 3000')
plot4
ggsave(plot4, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\city_pe-I.pdf',
       width=15, height=9,
       units = 'in')






# PANEL QUARTER INFECTIONS ------------------------------------------------


# Panel Quarters
panel_data <- combined
panel_data$day_index <- (combined$week.id +1) * 7
panel_data <- dplyr::rename(panel_data, quarter_name = quarter)

panel_I_quarter <- ggplot (I_quarter_melt, 
                           aes( x = day_index, y = value, 
                                group = variable, color = quarter_name)) +
  geom_line(alpha = 0.01) +
  geom_line(data = panel_data,
            size = 1.2,
            color = 'black',
            linetype = 4,
            aes(x = day_index, y = sick.total.week,
                group = quarter_name)) +
  geom_vline( xintercept = 40, linetype = 2, alpha = 0.6, color = "black") +
  facet_wrap(~quarter_name) +
  xlab("Day index") +
  ylab("Incident cases") +
  xlim(0, 105) +
  ggtitle ("Incident cases per week by quarter\n") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 21, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold", vjust = 1.4),
        plot.title = element_text(size = 25, face="bold"),
        strip.text.x = element_text(size = 16),
        strip.background = element_rect(color = '#F0F0F0', fill = '#F0F0F0'),
        panel.margin.y = unit(2, "lines"))

ggsave(panel_I_quarter,
       filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\panel_I_quarter_t0.pdf',
       width=20, height=12,
       units = 'in')

# Plot last model run
# plot(S_it_est[1, ], type = 'l', col = 'darkred', lwd = 2)
plot(I_it_est[1, ], type = 'l', col = 'darkred', lwd = '2')


