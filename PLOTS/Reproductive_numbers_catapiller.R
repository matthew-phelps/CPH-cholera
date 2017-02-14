
source("multi-neighbor/R calculations.R")
source("PLOTS/plot-functions.R")


r_plot <- R_log_scale(R)
ggsave(plot = r_plot, filename = "Plot-output/R-log-scale.jpg")
