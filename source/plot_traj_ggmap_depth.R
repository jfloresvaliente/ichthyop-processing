#=============================================================================#
# Name   : plot_traj_ggmap_depth
# Author : Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Plot particle paths from data.frame of the form
#          ['Drifter','Lon','Lat','Depth','Year','Month','ReleaseDepth','Age']
# URL    : 
#=============================================================================#
plot_traj_ggmap_depth <- function(
  df
  ,xlim = c(-90, -70)
  ,ylim = c(-20, 0)
  ,zlim = c(-60,0)
  ,pngfile = NULL
  ,title = ''
  ){
  #============ ============ Arguments ============ ============#
  
  # df = Data frame with the form ['Drifter','Lon','Lat','Depth','Year','Month','ReleaseDepth','Age']
  # xlim = range for X axis (longitude)
  # ylim = range for Y axis (latitude)
  # zlim = range for Z axis (depth)
  # XY = Central point from where the satellite image will be taken
  # pngfile = file name to save the plot. If NULL, plot and show in graphic device
  # title = title of the plot, If '', then, title is not show

  #============ ============ Arguments ============ ============#
  
  library(ggplot2)
  library(fields)

  map <- ggplot(data = df)
  map   <- map +
    # geom_point(data = df, aes(x = Lon, y = Lat, colour = Depth), size = .075) +
    geom_path(data = df, aes(group = Drifter, x = Lon, y = Lat, colour = Depth), size = .25) +
    scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), limits = zlim, expression(Depth)) +
    labs(x = 'Longitude (W)', y = 'Latitude (S)', title = title) +
    borders(fill='grey',colour='grey') +
    coord_fixed(xlim = xlim, ylim = ylim, ratio = 2/2) +
    theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
          axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
          axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
          axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
          plot.title   = element_text(face='bold', color='black', size=15, angle=0),
          legend.text  = element_text(face='bold', color='black', size=15),
          legend.title = element_text(face='bold', color='black', size=20),
          legend.position   = c(0.92, 0.9),
          legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))

  print(pngfile); flush.console()
  if(!is.null(pngfile)) ggsave(filename = pngfile, width = 9, height = 9) else map
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#