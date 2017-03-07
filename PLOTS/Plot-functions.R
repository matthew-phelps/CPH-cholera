# Author: Matthew Phelps
#Desc: Various plots of the cholera data
# output datasets: many plots

library (ggplot2)




citywide_plot <- function(x) {
  ggplot(x, aes(x = week_date))+
    geom_line(aes(y = sick, color = "cases"), size = 1) +
    geom_line(aes( y = dead, color = "deaths"), size = 1) +
    geom_vline( xintercept = 40, linetype = 2, alpha = 0.6, color = "black") +
    xlab("Day index") +
    ylab("People") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.8, 0.5),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))
}




quarter_panel_incidence <- function(combined) {
  ggplot (combined,
          aes( x = week.id,
               y = sick.total.week / est.pop.1853,
               group = quarter,
               color = quarter))+
    geom_line(size = 1) +
    geom_vline( xintercept = 5, linetype = 2, color = "black") +
    facet_wrap(~quarter) +
    xlab("Week index") +
    ylab("Incidence per 100") +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12, vjust = -0.1),
          axis.title.y = element_text(size = 12, vjust = 0.5),
          strip.background = element_rect(color = '#F0F0F0', fill = '#F0F0F0')) 
}





R_log_scale <- function(R,  pd = position_dodge(0.4)) {
  ggplot(data = R) +
    geom_hline(yintercept = 1, linetype = 3, color = "black") +
    geom_point(aes(x = quarter, y = R_median, shape = R_type, color = R_type),
               position = pd,
               size = 1.9) +
    geom_errorbar(aes(x = quarter, ymin = lower, ymax = upper, color = R_type),
                  width = 0.4,
                  size = 0.8,
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
    ggtitle("R intern & R extern values") + 
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1.0))
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