# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

library(cowplot) # multiplots: https://goo.gl/RK49b0

source("PLOTS/map-base.R")
source("PLOTS/map-layers.R")

case <- inc_rate_map(mapdf) %>%
  add_hosp(hosp_tidy)

cfr <- cfr_map(mapdf) %>%
  add_hosp(hosp_tidy)
  
case_first <- first_case_map(mapdf) %>%
  add_hosp(hosp_tidy)
  
water_infra <- pipe_map(pipes_tidy) %>%
  add_hosp(hosp_tidy)

R_ext <- R_ext_map(mapdf = mapdf)
R_int <- R_int_map(mapdf = mapdf)

# MULTIPLOT ---------------------------------------------------------------
map_multi <- plot_grid(water_infra, case_first, case, cfr, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)

save_plot(map_multi, file = "Plot-output/map_multi.jpg",
          base_height = 10)

