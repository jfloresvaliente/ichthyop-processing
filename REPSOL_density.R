library(ggplot2)
library(fields)

dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/REPSOL/'
xlim <- c(-78.5, -76.5)
ylim <- c(-13.5, -11.0)
zlim <- c(0,40)

bin_lon <- 0.05
bin_lat <- bin_lon

lastday  <- 10
fechas   <- c(1, seq(from = 5, to = lastday, by = 10))
fechas   <- lastday

load(paste0(dirpath, 'REPSOL_trajs.Rdata'))
df2 <- df; rm(df)

for(i in 1:length(fechas)){
  df <- subset(df2, df2$timer == fechas[i])
  
  p1 <- ggplot(data = df)+
    stat_bin_hex(data = df, mapping = aes(x = lon, y = lat), binwidth = c(bin_lon, bin_lat))+
    scale_fill_gradientn(colours = tim.colors(64), limits = zlim, na.value = '#800000', expression(Particulas))+
    labs(x = 'Longitude (W)', y = 'Latitude (S)', fill = '') +
    borders(fill='grey',colour='grey') +
    coord_fixed(xlim = xlim, ylim = ylim)+
    theme(axis.text.x  = element_text(face='bold', color='black', size=12, angle=0),
          axis.text.y  = element_text(face='bold', color='black', size=12, angle=0),
          axis.title.x = element_text(face='bold', color='black', size=18, angle=0),
          axis.title.y = element_text(face='bold', color='black', size=18, angle=90),
          legend.text  = element_text(size=12),
          legend.title = element_text(size=15, face= 'bold'),
          legend.position   = c(0.89, 0.75),
          legend.direction  = 'vertical',
          legend.key.height = unit(15, 'pt'),
          legend.key.width  = unit(05, 'pt'),
          legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.3, linetype='solid'))
  pngfile <- paste0(dirpath, 'REPSOL_density',fechas[i],'.png')
  print(pngfile)
  ggsave(filename = pngfile, width = 9, height = 9)
}

