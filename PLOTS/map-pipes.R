# Author: Matthew Phelps
#Desc: Trying to plot using a Raster basemap instead of Gmaps
# output datasets: many plots



source("PLOTS/map-base.R")

pipe_map(pipes_tidy) %>%
  add_hosp(hosp_tidy)

