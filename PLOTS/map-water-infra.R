# Author: Matthew Phelps
#Desc: Map of pipe infrastrucutre
# output datasets: many plots

## intro

library (tidyverse)

source("PLOTS/map-base.R")
base_map

# Get spatial data into a form that ggplot2 can handle
pipes_df <- as.data.frame(pipes)
pipes_tidy <- tidy(pipes, region = "Company")
pipes_tidy <- left_join(quarter_tidy, pipes_df, by = c("group" = "Company"))
pipes_tidy <- pipes_tidy[ order(pipes_tidy$order),]



pipes_map <- base_map + geom_path(data = mapdf,
                                        aes(x = long, y = lat, group = id,
                                            fill = (cum.sick/est.pop.1853)*100),
                                        color = "grey")+
  scale_fill_gradientn(name = "Cumulative infections \nper 100 people",
                       colours = brewer.pal(9, "Reds")) +
  theme(legend.position = c(0.1,0.15))
inc_rate_map
