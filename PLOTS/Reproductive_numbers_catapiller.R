require(grid)
library(coda)
source("PLOTS/plot-functions.R")
source("functions/CalculateRFun.R")




# MODEL 1 -----------------------------------------------------------------
load("Data/Rdata/sim-model-1-data-1.Rdata")

r_plot <- R_log_scale(R)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m1.jpg",
       height = 7,
       width = 10)



# MODEL 2 -----------------------------------------------------------------
load("Data/Rdata/sim-model-2-data-1.Rdata")
R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R <- rbind(R_list$R_int, R_list$R_ext, R_list$R_tot)

r_plot <- R_log_scale(R)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m2.jpg",
       height = 7,
       width = 10)


# MODEL 5 -----------------------------------------------------------------
load("Data/Rdata/sim-model-5-data-1.Rdata")

R_list <- RCalc(betas = mcmc_out$betas_95hpd,
                lo_hpd = lo_hpd, hi_hpd = hi_hpd, gamma = mcmc_out$gamma_95hpd,
                q_names = q_names, order = TRUE)

R <- rbind(R_list$R_int, R_list$R_ext, R_list$R_tot)

r_plot <- R_log_scale(R)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m5.jpg",
       height = 7,
       width = 10)
