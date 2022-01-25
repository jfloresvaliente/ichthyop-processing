library(ggplot2)
library(fields)

dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/REPSOL/'
load(paste0(dirpath, 'REPSOL_trajs.Rdata'))

lastday  <- 150
# fechas   <- c(1, seq(from = 5, to = lastday, by = 5))
fechas   <- 150

xlim <- c(-78.5, -76.5)
ylim <- c(-13.5, -11.0)
zlim <- c(0,lastday)


for(i in 1:length(fechas)){
  df <- subset(df, df$drifter %in% 1:100)
  # df <- subset(df2, df2$timer == fechas[i])
  
  map   <- ggplot(data = df)
  map   <- map +
    geom_point(data = df, aes(x = lon, y = lat, colour = timer), size = .01) +
    # geom_path(data = df, aes(group = Drifter, x = Lon, y = Lat, colour = Length), size = .075) +
    scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), limits = zlim, expression(Time (days))) +
    labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
    borders(fill='grey',colour='grey') +
    coord_fixed(xlim = xlim, ylim = ylim) +
    theme(axis.text.x  = element_text(face='bold', color='black', size=12, angle=0),
          axis.text.y  = element_text(face='bold', color='black', size=12, angle=0),
          axis.title.x = element_text(face='bold', color='black', size=18, angle=0),
          axis.title.y = element_text(face='bold', color='black', size=18, angle=90),
          legend.text  = element_text(size=12),
          legend.title = element_text(size=15, face= 'bold'),
          legend.position   = c(0.82, 0.85),
          legend.direction  = 'vertical',
          legend.key.height = unit(25, 'pt'),
          legend.key.width  = unit(10, 'pt'),
          legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))
  
  pngfile <- paste0(dirpath, 'REPSOL',fechas[i],'.png')
  print(pngfile)
  ggsave(filename = pngfile, width = 9, height = 9)
}

