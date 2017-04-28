library(tidyverse)
require(grid)
library(coda)
library(cowplot)

source("functions/plot-functions.R")
load(file = 'Data/Rdata/models-version2-data.Rdata')

# MODEL 5 -----------------------------------------------------------------
models_data[[1]]$R_model$R_vals
inx <- 1:length(models_data)
lapply(inx, function(x){
  r_in_out <-  models_data[[x]]$R_model$R_vals %>%
    filter(R_type == "in" | R_type == "ext" | R_type == "int" |
             R_type == "tot")%>%
    R_log_scale(pd = 0.6, line_size = 0.4, point_size = 1.5) %>%
    RStyle()
  r_in_out <- r_in_out + ggtitle(paste("model", x, sep = " "))
  
  return(r_in_out)
})

