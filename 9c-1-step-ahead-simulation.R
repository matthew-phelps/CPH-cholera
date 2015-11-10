# Author: Matthew Phelps
#Desc: 1-step ahead simulations
# Dependicies: Data 1, Data 2, 5_GLM_data_reshape, 8c_JAGS


# Intro -------------------------------------------------------------------

graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH"
pc <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH"
setwd(pc)
rm(list = ls())

library(ggplot2)
library(reshape)

# LOAD data ---------------------------------------------------------------

load(file = 'data\\Rdata\\model_sim_data.Rdata')
load(file = 'data\\Rdata\\I_it.Rdata')
load(file = 'data\\Rdata\\S_it.Rdata')
load(file = 'data\\Rdata\\q_names.Rdata')
set.seed(123)



# 1-step ahead WITHOUT Phi ------------------------------------------------------------
# Should be without Phi, because the S_it(phi) is the observed Susceptible
# population

loops <- 2000
I_plus1_list <- list()

for (z in 1:loops){
  I_est_tplus1 <- as.data.frame(matrix(data = 0, nrow = Nquarter, ncol = Nsteps))
  Lambda_est_pe <- as.data.frame(matrix(data = 0, nrow = Nquarter, ncol = Nsteps))
  for (i in 1:Nquarter){
    for (t in 1:(Nsteps-1)){
      Lambda_est_pe[i, t] <- S_it[i, t] / N_it[i, t] * sum(beta_pe[i, ] * I_it[, t])
      I_est_tplus1[i, t+1] <- rpois(1, Lambda_est_pe[i, t])
    }
  }
  I_plus1_list[[z]] <- I_est_tplus1
  row.names(I_plus1_list[[z]]) <- q_names[, 1]
  
}



# DATA RESHAPING----------------------------------------------------


# t + 1 Infectious for single quarter
I_quarter_melt_list <- list()
for (i in 1:Nquarter){
  I_quarter <-(I_plus1_list[[1]][i, ])
  for (z in 2:(loops)){
    I_quarter <- rbind(I_quarter, I_plus1_list[[z]][i, ])
  }
  I_quarter <- as.data.frame(t(I_quarter))
  I_quarter$week_index <- 1:Nsteps
  I_quarter$day_index <- 7 * I_quarter$week_index
  I_quarter$week_index <- NULL
  I_quarter_melt_list[[i]] <- melt(I_quarter, id.vars = 'day_index')
}

# Infectious Data for all quarters (city_pe level). Flatten each matrix
city_pe <- as.data.frame(matrix(data = 0, nrow = 16, ncol = loops))
city_pe$week_index <- 1:Nsteps
city_pe$day_index <- city_pe$week_index * 7

for (z in 1:loops){
  city_pe[z] <- as.data.frame(colSums(I_plus1_list[[z]]))
}
city_pe_melt <- melt(city_pe, id.vars = 'day_index')
ymax <- max(city_pe)




# PLOTTING ----------------------------------------------------------------


plot1 <- ggplot(data = city_pe_melt[which(city_pe_melt$variable != 'day_index'), ], aes(x = day_index, y = value, group = variable)) +
  geom_line(color = 'darkgreen', alpha = 0.05) +
  geom_vline( xintercept = 40, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("People") +
  ggtitle('Citywide weekly \none-step-ahead estimates') +
  coord_cartesian(xlim = c(0, 112), ylim = c(-5, (ymax + .05 * ymax))) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 20, face="bold"))

plot1


# Quartery Infectious (Christianshavn)
plot_christianshavn<- ggplot(data = I_quarter_melt, aes(x = day_index, y = value, group = variable)) +
  geom_line(color = 'darkred', alpha = 0.05) +
  ggtitle('Christianshavn simulated\n n = 2000')
plot_christianshavn
ggsave(plot_christianshavn, 
       file = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\Christanshavn_t_plus1.pdf',
       width=15, height=9,
       units = 'in')

# PLOTS of NON-SIM DATA FOR COMPARISON ------------------------------------

# City-wide aggregated to the week ----------------------------------------
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


plot2 <- ggplot() +
  geom_line(data = city_pe_melt[which(city_pe_melt$variable != 'day_index'), ],
            aes(x = day_index, y = value, group = variable),
            color = 'darkgreen', alpha = 0.01) +
  geom_line(data = city_week, aes(x = day_index, y = V1),
            color = 'darkred', alpha = 0.6, size = 1) +
  geom_vline( xintercept = 40, linetype = 2, alpha = 0.6, color = "black") +
  xlab("Day index") +
  ylab("People") +
  ggtitle('Citywide weekly \none-step-ahead estimates') +
  coord_cartesian(xlim = c(0, 112), ylim = c(-5, (ymax + .05 * ymax))) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 20, face="bold"))

plot2

ggsave(plot2,
       filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\Simulations\\Citywide-1-step-ahead.pdf',
       width = 15,
       height = 9,
       units = 'in')
