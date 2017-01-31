# Author: Matthew Phelps
#Desc: Verifying interpolation
library(tidyverse)
source("Data-5-interpolation.R")

# PLOT REPLICATE --------------------------------------------------------------------
panel_data <- combined
panel_data$day_index <- (combined$week.id +1) * 7
panel_data <- dplyr::rename(panel_data, quarter = quarter)


# Reshape to long format again:

panel_plot <- ggplot() +
  geom_line(data = I_multi_replicate,
            aes(x = day_index, y = rep1,
                group = quarter),
            color = "red",
            size = 1.2) +
  geom_line(data = panel_data,
            size = 1.2,
            color = 'black',
            linetype = 1,
            alpha = 0.3,
            aes(x = day_index, y = sick.total.week/7,
                group = quarter)) +
  geom_point(data = panel_data,
             size = 3.2,
             color = "black",
             aes(x = day_index, y = sick.total.week/7,
                 group = quarter)) +
  facet_wrap(~quarter)
panel_plot



ggsave(filename = 'C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH\\Output\\replicate_panel.tiff',
       plot = panel_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)
