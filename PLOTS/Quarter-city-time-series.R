# Author: Matthew Phelps
#Desc: City and quarter plot output
library(dplyr)
library(cowplot)
source("Data-3-combine quarters.R")
source("PLOTS/plot-functions.R")


# City-wide time-series ---------------------------------------------------
outbreak_city <- quarter %>%
  group_by(week.id) %>%
  summarise( sick = sum(sick.total.week),
             dead = sum(dead.total.week)) %>%
  arrange(week.id) %>%
  mutate(week_date = week_date)

citywide <- citywide_plot(outbreak_city)

quarter <- quarter_panel_incidence(combined = combined)


# PLOT --------------------------------------------------------------------

ts_multi <- plot_grid(citywide, quarter,labels = c("A", "B"), ncol = 1, nrow = 2)
save_plot(ts_multi, file = "Plot-output/ts_multi.jpg",
          base_height = 10,
          base_width = 14)
