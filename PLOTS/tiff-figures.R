# TIFF plots
rm(list = ls())
library(pander)
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(stargazer)
source("functions/waic_dic_summary.R")
source("functions/MapBaseAndLayers.R")
source("Spatial-data-1.R")
figure_path <- "C:/Users/wrz741/Google Drive/Copenhagen/Thesis/Figures/"
multiPlotWrapper(mapdf, wall_fort, water_fort, l_size = 0.1,
                 wall_l_size = 0.3, p_size = 1, txt_size = 10,
                 leg_height = 2.5, transp = 0.7)

base_map <- baseMap(mapdf, l_size = 0.1) %>%
  addWall(wall_fort, line_size=0.3) %>%
  addWater(water_fort, line_size= 0)

attack_rate <- base_map %>% attackRateMap(mapdf, l_size = 0.1, txt_size = 10,
                                          leg_height = 2.5) %>%
  addLabels(mapdf, transp = 0.7)


ggsave(paste(figure_path, "AR_map.png", sep=""),
       attack_rate, dpi = 400,
       width = 7, height = 6.5, units = "in")

source("Data-3-combine quarters.R")
source("functions/plot-functions.R")

quarter <- quarter_panel_incidence(combined = combined, txt_size = 11)

ggsave(paste(figure_path, "quater_pannel.png", sep=""),
       quarter, dpi = 400,
       width = 7, height = 4.5, units = "in")





