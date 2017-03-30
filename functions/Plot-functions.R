# Author: Matthew Phelps
#Desc: Various plots of the cholera data
# output datasets: many plots

library (ggplot2)




citywide_plot <- function(x, txt_size) {
  ggplot(x, aes(x = week_date))+
    geom_line(aes(y = sick, color = "cases"), size = 1) +
    geom_line(aes( y = dead, color = "deaths"), size = 1) +
    geom_vline( xintercept = 40, linetype = 2, alpha = 0.6, color = "black") +
    xlab("Day index") +
    ylab("People") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.5),
          axis.text.x = element_text(size = txt_size),
          axis.text.y = element_text(size = txt_size),
          axis.title.x = element_text(size = txt_size),
          axis.title.y = element_text(size = txt_size))
}




quarter_panel_incidence <- function(combined, txt_size) {
  ggplot (combined,
          aes( x = week.id,
               y = sick.total.week / est.pop.1853 * 100,
               group = quarter))+
    geom_line(size = 1) +
    geom_vline( xintercept = 5, linetype = 2, color = "black") +
    facet_wrap(~quarter) +
    xlab("Week index") +
    ylab("Incidence per 100") +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text.x = element_text(size = txt_size-2),
          axis.text.y = element_text(size = txt_size-2),
          axis.title.x = element_text(size = txt_size, vjust = -0.1),
          axis.title.y = element_text(size = txt_size, vjust = 0.5),
          strip.background = element_rect(color = '#F0F0F0', fill = '#F0F0F0')) 
}





R_log_scale <- function(R,  pd = position_dodge(0.4),
                        line_size, point_size) {
  ggplot(data = R) +
    geom_hline(yintercept = 1, linetype = 3, color = "black") +
    geom_point(aes(x = quarter, y = R_median, shape = R_type, color = R_type),
               position = pd,
               size = point_size) +
    geom_errorbar(aes(x = quarter, ymin = lower, ymax = upper, color = R_type),
                  width = 0.4,
                  size = line_size,
                  position = pd) +
    scale_y_log10(breaks = c(0.0000001,
                             0.000001,
                             0.00001,
                             0.0001,
                             0.001,
                             0.01,
                             0.1,
                             1,
                             10),
                  labels = c("0.0000001",
                             "0.000001",
                             "0.00001",
                             "0.0001", 
                             "0.001", 
                             "0.01",
                             "0.1",
                             "1",
                             "10")) +
    theme_classic() +
    ylab("Median R value") +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1.0))
}


R <- function(base_plot){
  base_plot + 
    scale_color_manual(name = "R type",
                       values = c("dodgerblue4", "orange3"),
                       labels = c("Outflow", "Inflow")) +
    scale_shape_manual(name = "R type",
                       values = c(19, 17),
                       labels = c("Outflow", "Inflow"))
}


RExtIntStyle <- function(base_plot){
  base_plot + 
    scale_color_manual(name = "R type",
                       values = c("dodgerblue4", "green4", "red3"),
                       labels = c("External", "Internal", "Total")) +
    scale_shape_manual(name = "R type",
                       values = c(19, 17, 15),
                       labels = c("External", "Internal", "Total"))
}

RInOutStyle <- function(base_plot){
  base_plot + 
    scale_color_manual(name = "R type",
                       values = c("dodgerblue4", "orange3"),
                       labels = c("Outflow", "Inflow")) +
    scale_shape_manual(name = "R type",
                       values = c(19, 17),
                       labels = c("Outflow", "Inflow"))
}



R_non_log <- function(R, pd = position_dodge(0.4)) {
  ggplot(data = R) +
    geom_hline(yintercept = 1, linetype = 3, color = "light grey") +
    geom_point(aes(x = quarter, y = R_median, shape = R_type, color = R_type),
               position = pd,
               size = 1.9) +
    geom_errorbar(aes(x = quarter, ymin = lower, ymax = upper, color = R_type),
                  width = 0.4,
                  size = 0.8,
                  position = pd) +
    theme_classic() +
    ggtitle("R intern & R extern values") + 
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1.0))
}


rTable <- function(R_median, log = TRUE){
  par(mar=c(3,6.5,6,2.9)) # Margins around plot ()
  if(log){
    color2D.matplot(log(R_median), 
                    show.values = TRUE,
                    axes = FALSE,
                    xlab = "",
                    ylab = "",
                    vcex = 2,
                    vcol = "black",
                    extremes = c("white", "blue"))
  } else {
    color2D.matplot((R_median), 
                    show.values = TRUE,
                    axes = FALSE,
                    xlab = "",
                    ylab = "",
                    vcex = 2,
                    vcol = "black",
                    extremes = c("white", "blue"))
  }
  xpos <- seq_len(ncol(R_median)) +0.2
  ypos <- seq_len(ncol(R_median)) - 0.4
  axis(3, # specifies top border x position
       at = xpos,
       labels = F, tick = FALSE, cex.axis = 0.7)
  text(x = xpos,
       labels = names(R_median),
       srt = 45, # angle to rotate
       pos = 3, # specifies to put txt at top
       par("usr")[4] +0.7, # 0.7 lines above the top. [4] places ref to top border
       adj = 0,
       xpd = T) # not sure but allows txt to overflow table
  axis(2, 
       at = ypos,
       labels = F, tick = FALSE, cex.axis = 0.7)
  text(y = ypos,
       labels = rev(names(R_median)),
       srt = 45, # angle to rotate
       pos = 2, # specifies to put txt at top
       par("usr")[1] + -0.1,  # 0.1 lines left of left border. [1] places ref to left border
       adj = 0,
       xpd = T) # not sure but allows txt to overflow table
}

# sim plus 1
sim1_plus1 <- function(I_simulated_plus1){
  ggplot() + 
    geom_line(data = I_simulated_plus1, 
              alpha = 0.1,
              aes(x = day, y = I_simulated,
                  group = interaction(quarter, sim_num),
                  color = quarter)) +
    geom_line(data = combined, aes(x = (week.id+1) * 7,
                                   y = sick.total.week/7, group = quarter)) +
    facet_wrap(~quarter) +
    theme_minimal() +
    theme(legend.position = "none")
}