rm(list = ls())
library(cowplot)
source("functions/trimSims.R")
source("functions/SimulationAndPlots.R")
source("multi-neighbor/sim-model-5-data-prep.R")
load(file =  "data/Rdata/sim3_full_data.Rdata")
load(file =  "data/Rdata/sim5_full_data.Rdata")

sim3_trim <- trimSims(sim3_full_data, 10, quarter_val = "St. Annae Oester",
                      day_val = 21)
sim5_trim <- trimSims(sim5_full_data, 10)

sim3_summary <- SimCI(sim3_trim$sim_filtered)
sim5_summary <- SimCI(sim5_trim$sim_filtered)



sim3_plot <- SimPlot(observed_data = I_reps_plot,
        rib_col = "blue", alpha_sim = 0.05,
        ci = sim3_summary,
        ribbon = TRUE)

sim5_plot <- SimPlot(observed_data = I_reps_plot,
                     rib_col = "red", alpha_sim = 0.05,
                     ci = sim5_summary,
                     ribbon = TRUE)

plot_grid(sim3_plot, sim5_plot ,labels = c("3", "5"), ncol = 2, nrow = 1)
