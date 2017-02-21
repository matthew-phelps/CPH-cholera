add_hosp <- function(old_map, hosp_tidy){
  old_map + geom_point(data = hosp_tidy,
                       aes(x = coords.x1, y = coords.x2),
                       size = 3,
                       color = "darkblue")
}


# add_hosp_legend <- function(old_map, hosp_tidy){
#   get_y <- function(x){
#     ((max(x) - min(x)) / 2) + min(x)
#   }
#   old_map + geom_point(data = hosp_tidy,
#                        aes(x = 1401630, y = 7496379),
#                        size = 3,
#                        color = "green")
# }


inc_rate_map <- function(mapdf){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = AR),
                          color = "grey")+
    scale_fill_gradientn(name = "Attack rate \nper 100 people",
                         colours = brewer.pal(9, "Reds"),
                         limits=c(0,10)) +
    theme(legend.position = c(0.15,0.15))
}



cfr_map <- function(mapdf){
  x_loc <- (xrng[2] - xrng[1]) /2 + xrng[1]
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = CFR),
                          color = "grey")+
    scale_fill_gradientn(name = "CFR",
                         colours = brewer.pal(9, "Reds"),
                         limits = c(50,80)) +
    theme(legend.position = c(0.1,0.15))
}





pipe_map <- function(pipes_tidy){
  base_map + geom_path(data = pipes_tidy,
                       aes(x = long, y = lat,
                           group = group),
                       color = "black")
}





first_case_map <- function(mapdf){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = start),
                          color = "grey")+
    scale_fill_gradientn(name = "Start week",
                         colours = brewer.pal(3, "Greens"),
                         limits = c(1,3),
                         breaks = c(1,2,3)) +
    theme(legend.position = c(0.1,0.15))
}


## Look at:https://gist.github.com/hadley/233134
## for how to discritize the scale

R_ext_map <- function(mapdf, Log = FALSE){
  if(Log){
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = log(R_ext)),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(min(log(mapdf$R_ext)),
                                      max(log(mapdf$R_ext))))+
      theme(legend.position = c(0.1,0.15))
  } else {
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = R_ext),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(0,
                                      max(mapdf$R_ext)))+
      theme(legend.position = c(0.1,0.15))
  }
}

R_int_map <- function(mapdf, Log = FALSE){
  if(Log){
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = log(R_int)),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(min(log(mapdf$R_int)),
                                      max(log(mapdf$R_int))))+
      theme(legend.position = c(0.1,0.15))
  } else {
    base_map + geom_polygon(data = mapdf,
                            aes(x = long, y = lat, group = id,
                                fill = R_int),
                            color = "grey")+
      scale_fill_gradientn(name = "R_ext",
                           colours = brewer.pal(3, "Oranges"),
                           limits = c(0,
                                      max(mapdf$R_int)))+
      theme(legend.position = c(0.1,0.15))
  }
}

add_map_lab <- function(old_map, centroids){
  old_map + geom_text(data = centroids,
                      aes(label = quarter,
                          x = lab_long, y = lab_lat))
}