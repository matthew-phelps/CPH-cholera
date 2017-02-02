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

case_first <- first_case_map(mapdf) %>%
  add_hosp(hosp_tidy)

water_infra <- pipe_map(pipes_tidy) %>%
  add_hosp(hosp_tidy)



# MULTIPLOT ---------------------------------------------------------------

multiplot(water_infra, case_first, case, cfr, layout=matrix(c(1,2,3,4), nrow=2, byrow=TRUE))
