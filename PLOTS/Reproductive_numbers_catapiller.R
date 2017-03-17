require(grid)
library(coda)

source("PLOTS/plot-functions.R")
load("Data/Rdata/r-values-model-1.Rdata")
load("Data/Rdata/r-values-model-2.Rdata")
load("Data/Rdata/r-values-model-3.Rdata")
load("Data/Rdata/r-values-model-4.Rdata")
load("Data/Rdata/r-values-model-5.Rdata")
# MODEL 1 -----------------------------------------------------------------
r_plot <- R_log_scale(R_model1)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m1.jpg",
       height = 7,
       width = 10)



# MODEL 2 -----------------------------------------------------------------
r_plot <- R_log_scale(R_model2)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m2.jpg",
       height = 7,
       width = 10)


# MODEL 4 -----------------------------------------------------------------
r_plot <- R_log_scale(R_model4)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m2.jpg",
       height = 7,
       width = 10)


# MODEL 5 -----------------------------------------------------------------

r_plot <- R_log_scale(R_model5)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m5.jpg",
       height = 7,
       width = 10)
