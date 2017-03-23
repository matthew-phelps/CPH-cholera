library(tidyverse)
require(grid)
library(coda)
library(cowplot)

source("functions/plot-functions.R")
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



# MODEL 3 -----------------------------------------------------------------
r_plot <- R_log_scale(R_model3$R_vals)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m3.jpg",
       height = 7,
       width = 10)


# MODEL 4 -----------------------------------------------------------------
r_plot <- R_log_scale(R_model4)
r_plot

ggsave(plot = r_plot, filename = "Plot-output/R-log-m2.jpg",
       height = 7,
       width = 10)


# MODEL 5 -----------------------------------------------------------------

r_int_ext <- R_model5$R_vals %>%
  filter(R_type == "int" | R_type == "ext" | R_type == "tot")%>%
  R_log_scale() %>%
  RExtIntStyle()

r_int_ext <- r_int_ext + ggtitle("R internal and R external")

r_in_out <-  R_model5$R_vals %>%
  filter(R_type == "in" | R_type == "ext")%>%
  R_log_scale() %>%
  RInOutStyle()

r_in_out <- r_in_out + ggtitle("R Out and R In")

out <- cowplot::plot_grid(r_int_ext, r_in_out, nrow = 2)
out

ggsave(plot = out, filename = "Plot-output/R-log-m5.jpg",
       height = 10,
       width = 8)
