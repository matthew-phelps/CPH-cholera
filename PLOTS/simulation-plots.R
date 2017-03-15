rm(list = ls())
library(ggplot2)
library(cowplot)
load("data/Rdata/sim5_step_summary.Rdata")
load("data/Rdata/sim5_full_summary.Rdata")

load("data/Rdata/sim3_step_summary.Rdata")
load("data/Rdata/sim3_full_summary.Rdata")

source("multi-neighbor/sim-model-5-data-prep.R")
source("multi-neighbor/SimulationAndPlots.R")
rm(mcmc_out)


sim5_step <- SimPlot(observed_data = I_reps_plot,
                     color = "blue", alpha_sim = 0.05,
                     ci = sim5_step_summary,
        ribbon = TRUE)
sim5_full <- SimPlot(observed_data = I_reps_plot,
                     color = "blue", alpha_sim = 0.05,
                     ci = sim5_full_summary,
        ribbon = TRUE)



sim3_step <- SimPlot(observed_data = I_reps_plot,
                     rib_col = "blue", alpha_sim = 0.05,
                     ci = sim3_step_summary,
        ribbon = TRUE)
sim3_full <- SimPlot(observed_data = I_reps_plot,
                     rib_col = "blue", alpha_sim = 0.05,
                     ci = sim3_full_summary,
        ribbon = TRUE)

plot_grid(sim3_step, sim5_step ,labels = c("A", "B"), ncol = 2, nrow = 1)
plot_grid(sim3_full, sim5_full ,labels = c("A", "B"), ncol = 2, nrow = 1)



