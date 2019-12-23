#===============================================================================
# Name   : plot_traj_ggmap_day
# Author : Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Plot particle paths from data.frame of the form
#          ['Drifter','Lon','Lat','Depth','Year','Month','ReleaseDepth','Age']
# URL    : 
#===============================================================================
plot_traj_ggmap_day <- function(
  df
  ,xlim = c(-82, -80.15)
  ,ylim = c(-7,-5)
  ,zlim = c(0,28)
  ,XY = c(-81,-6)
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
  
  library(ggmap)
  library(fields)

  mymap <- get_map(location = c(lon = XY[1], lat = XY[2]), zoom = 8, maptype = 'satellite', color='bw')
  map   <- ggmap(mymap)
  map   <- map +
    # geom_point(data = df, aes(x = Lon, y = Lat, colour = Day), size = .075) +
    geom_path(data = df, aes(group = Drifter, x = Lon, y = Lat, colour = Day), size = .5) +
    scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), limits = zlim, expression(Days )) +
    labs(x = 'Longitude (W)', y = 'Latitude (S)', title = title) +
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
#===============================================================================
# END OF PROGRAM
#===============================================================================