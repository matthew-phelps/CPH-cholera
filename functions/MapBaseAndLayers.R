

baseMap <- function(mapdf, l_size = 1) {
  # Build base-map of CPH - including wall and water and quarters
  wall_fort
  water_fort
  base_map <- ggplot() +
    geom_polygon(data = mapdf,
                 aes(x = long, y = lat, group = id),
                 fill = "grey92", color = "grey",
                 size = l_size) +
    theme(axis.title.x = element_blank(), # remove x,y, label
          axis.title.y = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(), # remove tick marks
          axis.text = element_blank(), # no axis labels
          panel.grid.minor = element_blank(), # no gridlines
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.margin = unit(c(0,0,0,0), 'lines'))
  base_map <- base_map + coord_quickmap()
  
  
  
  return(base_map)
}

addLabels <- function(base_map, mapdf, transp){
  # browser()
  centroids <- setNames(do.call("rbind.data.frame", by(mapdf, mapdf$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
    rownames(centroids) <- unique(mapdf$group)
  centroids$label <- mapdf$id[match(rownames(centroids), mapdf$group)]
  
  
  # Replace "_" with " "
  centroids$label <- gsub("_", " ", centroids$label)
  
  # Length of background whitespace
  centroids$size <- nchar(centroids$label)
  kx <- 39 
  ky <- 55
  base_map + 
    with(centroids, annotate(geom="rect", xmin = long - kx*size, xmax = long + kx*size,
                                      ymin = lat - ky-17, ymax = lat + ky,
                                      fill = "white", alpha = transp)) +
    with(centroids, annotate(geom="text", x = long, y = lat, label = label, size = 2.5))
  
  
}
addWall <- function(base_map, wall_fort, line_size) {
  # Get x/y range to set map to later after new layers added
  xrng <- ggplot_build(base_map)$layout$panel_ranges[[1]]$x.range
  yrng <- ggplot_build(base_map)$layout$panel_ranges[[1]]$y.range
  xrng[2] <- xrng[2] + 50
  yrng[2] <- yrng[2] - 200
  yrng[1] <- yrng[1] + 250
  base_map <- base_map +
    geom_path(data = wall_fort,
              aes(x = long, y = lat,
                  group = group),
              color = "grey",
              size = line_size)
  base_map <- base_map +
    coord_cartesian(xlim = c(xrng),
                    ylim = c(yrng))
  # theme(aspect.ratio = 0.5) 
  return(base_map)
}

addWater <- function(base_map, water_fort, line_size) {
  xrng <- ggplot_build(base_map)$layout$panel_ranges[[1]]$x.range
  yrng <- ggplot_build(base_map)$layout$panel_ranges[[1]]$y.range
  xrng[2] <- xrng[2] + 50
  xrng[1] <- xrng[1] - 00
  yrng[2] <- yrng[2] - 200
  yrng[1] <- yrng[1] + 250
  base_map <- base_map +
    geom_polygon(data = water_fort,
                 aes(x = long, y = lat,
                     group = group),
                 fill = "#99CCFF",
                 alpha = 0.7,
                 size = line_size)
  
  base_map <- base_map +
    coord_cartesian(xlim = c(xrng),
                    ylim = c(yrng))
  # theme(aspect.ratio = 0.5) 
  return(base_map)
}

addScale <- function(base_map, mapdf) {
  
  
  # https://goo.gl/8035ro
  base_map <- base_map + 
    ggsn::scalebar(mapdf,
                   location = "topleft",
                   dist = 0.5, height = 0.01, st.size = 2,
                   model = WGS84)
  
}

addHosp <- function(old_map, hosp_tidy, p_size){
  old_map + geom_point(data = hosp_tidy,
                       aes(x = coords.x1, y = coords.x2),
                       size = p_size,
                       color = "darkblue")
}





attackRateMap <- function(base_map, mapdf, l_size, txt_size,
                         leg_height){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = AR),
                          color = "grey",
                          size = l_size)+
    scale_fill_gradientn(name = "Attack rate \nper 100 people",
                         colours = brewer.pal(9, "Reds"),
                         limits=c(0,10)) +
    theme(legend.title = element_text(size = txt_size),
          legend.position = c(0.11,0.15),
          legend.text = element_text(size = (txt_size-2)),
          legend.key.height = unit(leg_height, units = "mm"))
}



cfr_map <- function(base_map, mapdf, l_size, txt_size,
                    leg_height){
 
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = CFR),
                          color = "grey",
                          size = l_size)+
    scale_fill_gradientn(name = "CFR",
                         colours = brewer.pal(9, "Reds"),
                         limits = c(50,80)) +
    theme(legend.title = element_text(size = txt_size),
          legend.position = c(0.1,0.15),
          legend.text = element_text(size = (txt_size-2)),
          legend.key.height = unit(leg_height, units = "mm"))
}





pipe_map <- function(base_map, pipes_tidy, l_size){
  base_map + geom_path(data = pipes_tidy,
                       aes(x = long, y = lat,
                           group = group),
                       color = "black",
                       size = l_size)
}





first_case_map <- function(base_map, mapdf, l_size, txt_size,
                           leg_height){
  base_map + geom_polygon(data = mapdf,
                          aes(x = long, y = lat, group = id,
                              fill = start),
                          color = "grey",
                          size = l_size)+
    scale_fill_gradientn(name = "Start week",
                         colours = brewer.pal(3, "Greens"),
                         limits = c(1,3),
                         breaks = c(1,2,3)) +
    theme(legend.title = element_text(size = txt_size),
          legend.position = c(0.1,0.15),
          legend.text = element_text(size = (txt_size-2)),
          legend.key.height = unit(leg_height, units = "mm"))
}


## Look at:https://gist.github.com/hadley/233134
## for how to discritize the scale

R_ext_map <- function(base_map, mapdf, l_size, Log = FALSE){
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

R_int_map <- function(base_map, mapdf, l_size, Log = FALSE){
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
      theme(legend.position = c(0.1,0.10))
  }
}

add_map_lab <- function(old_map, centroids){
  old_map + geom_text(data = centroids,
                      aes(label = quarter,
                          x = lab_long, y = lab_lat))
}


multiPlotWrapper <- function(mapdf, wall_fort, water_fort, l_size, wall_l_size,
                             p_size, txt_size, leg_height, transp = 0.5) {
  leg_height
  # Wrapper for all plots - so can change visuals on all plots quickly
  base_map <- baseMap(mapdf, l_size = l_size) %>%
    addWall(wall_fort, line_size=wall_l_size) %>%
    addWater(water_fort, line_size= 0)
  
  attack_rate <- base_map %>% attackRateMap(mapdf, l_size = l_size, txt_size = txt_size,
                                    leg_height = leg_height) %>%
    addHosp(hosp_tidy, p_size = p_size) %>%
    addLabels(mapdf, transp = transp)
  
  cfr <- base_map %>% cfr_map(mapdf, l_size = l_size, txt_size = txt_size,
                              leg_height = leg_height) %>%
    addHosp(hosp_tidy, p_size= p_size) %>%
    addLabels(mapdf, transp = transp)
  
  case_first <- base_map %>% first_case_map(mapdf, l_size = l_size, txt_size = txt_size,
                                            leg_height = leg_height) %>%
    addHosp(hosp_tidy, p_size = p_size) %>%
    addLabels(mapdf, transp = transp)
  
  water_infra <- base_map %>% pipe_map(pipes_tidy, l_size = l_size) %>%
    addHosp(hosp_tidy, p_size = p_size)
  
  map_multi <- plot_grid(water_infra, case_first, attack_rate, cfr, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
  return(map_multi)
}
