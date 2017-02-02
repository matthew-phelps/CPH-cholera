# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots

## intro

source("PLOTS/map-base.R")
source("Functions/multiplot function.R")

case <- inc_rate_map(mapdf) %>%
  add_hosp(hosp_tidy)

cfr <- cfr_map(mapdf) %>%
  add_hosp(hosp_tidy)


pipe_map(pipes_tidy) %>%
  add_hosp(hosp_tidy)



# MULTIPLOT ---------------------------------------------------------------

multiplot(case, cfr, cols = 2)
