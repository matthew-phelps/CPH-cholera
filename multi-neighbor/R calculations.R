# Author: Matthew Phelps 
# Desc: Calculate the R-internal and R-external for each neighborhood using
# beta values
# 
# 
# Intro -------------------------------------------------------------------
graphics.off()
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       wd.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata",
       wd.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/Rdata")
ifelse(grepl("wrz741", getwd()),
       save.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Output/Results",
       save.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Output/Results")

setwd(wd.path)


library(ggplot2)
library(tidyr)
require(grid)
library(coda)
library(CholeraDataDK)

# LOAD data ---------------------------------------------------------------
load(file = "int_hpd.Rdata")
#load(file = "sim-model-5-data.Rdata") # update as more MCMCs are run



b <- T
m3 <- F
ifelse(m3, load("sim-model-3-data.Rdata"), load(file = "sim-model-5-data-b.Rdata"))
ifelse(b, jags_m5_ls <- jags_m5_ls_b, NA)

rm(I_it_daily, N_it, weekly_avg, phi, Nsteps)




# DATA SHAPING ------------------------------------------------------------
# Get the lower and upper CIs into correct shape for this analysis
lower_ci <- matrix(int_hpd[1:(Nquarter*Nquarter), 1],
                             nrow = Nquarter, ncol = Nquarter)
colnames(lower_ci) <- q_names
rownames(lower_ci) <- q_names
upper_ci <- matrix(int_hpd[1:(Nquarter*Nquarter), 2],
                              nrow = Nquarter, ncol = Nquarter)
colnames(upper_ci) <- q_names
rownames(upper_ci) <- q_names


# R-INTERNAL --------------------------------------------------------------
B_int_low <- diag(lower_ci)
B_int_hi <- diag(upper_ci)


x <- as.matrix(betas) # convert to matrix for diag() function to work
B_int <- diag(x) # extract diagonals
rm(x)

R_int <- data.frame(B_int / gamma )
colnames(R_int) <- "R_value"
R_int$lower <- B_int_low / gamma
R_int$upper <- B_int_hi /gamma
R_int$quarter <- q_names
R_int$R_type <- "int"
R_int$quarter <- factor(R_int$quarter, levels = R_int$quarter[order(R_int$R_value)])


# R-EXTERNAL -------------------------------------------------------------- 
# Defined as: The estimated number of infectious cases casused in all other
# quarters by a single infecious case in the target quarter

diag(lower_ci) <- NA
diag(upper_ci) <- NA
B_ext_low <- rowSums(lower_ci, na.rm = T)
B_ext_hi <- rowSums(upper_ci, na.rm = T)


x <- as.matrix(betas)
diag(x) <- NA
B_ext <- rowSums(x, na.rm = T)

R_ext <- data.frame(B_ext / gamma)
colnames(R_ext) <- "R_value"
R_ext$lower <- B_ext_low / gamma
R_ext$upper <- B_ext_hi / gamma
R_ext$quarter <- q_names
R_ext$R_type <- "ext"
R_ext$quarter <- factor(R_ext$quarter, levels = R_ext$quarter[order(R_int$R_value)])

# DATA SHAPING ------------------------------------------------------------
R <- rbind(R_int, R_ext)

# CATAPILER PLOT ----------------------------------------------------------
pd <- position_dodge(0.4)
r0_1 <- ggplot(data = R) +
  geom_hline(yintercept = 1, linetype = 3, color = "black") +
  geom_point(aes(x = quarter, y = R_value, shape = R_type, color = R_type),
             position = pd,
             size = 1.9) +
  geom_errorbar(aes(x = quarter, ymin = lower, ymax = upper, color = R_type),
                width = 0.4,
                size = 0.8,
                position = pd) +
  scale_y_log10(breaks = c(0.0000001,
                           0.000001,
                           0.00001,
                           0.0001,
                           0.001,
                           0.01,
                           0.1,
                           1,
                           10),
                labels = c("0.0000001",
                           "0.000001",
                           "0.00001",
                           "0.0001", 
                           "0.001", 
                           "0.01",
                           "0.1",
                           "1",
                           "10")) +
  theme_classic() +
  ggtitle("R intern & R extern values") + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1.0))
r0_1

setwd(save.path)
ggsave(r0_1,
       filename = 'R6 - r0 log scale.jpg',
       width = 15,
       height = 9,
       units = 'in')
# PLOT: NON-LOG SCALE -----------------------------------------------------
pd <- position_dodge(0.4)
r0_2 <- ggplot(data = R) +
  geom_hline(yintercept = 1, linetype = 3, color = "light grey") +
  geom_point(aes(x = quarter, y = R_value, shape = R_type, color = R_type),
             position = pd,
             size = 1.9) +
  geom_errorbar(aes(x = quarter, ymin = lower, ymax = upper, color = R_type),
                width = 0.4,
                size = 0.8,
                position = pd) +
  theme_classic() +
  ggtitle("R intern & R extern values") + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1.0))



r0_2
setwd(save.path)
ggsave(r0_2,
       filename = 'R7 - r0 model 5.jpg',
       width = 15,
       height = 9,
       units = 'in')