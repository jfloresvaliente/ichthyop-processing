library(ncdf4)    # install.packages('ncdf4')     # el paquete se instala una sola vez
library(ggplot2)  # install.packages('ggplot2')
library(fields)   # install.packages('fields')

dirpath <- 'C:/Users/jflores/Documents/JORGE/colaboradores/OmarVaramiento/'
ncfile  <- 'roms3d_ichthyop-run202008121802.nc'

firstdrifter    <- 1
lastdrifter     <- 1000
firsttime       <- 1
lasttime        <- 121 # (10 dias x 12 pasos de tiempo) + t0 = 121
xlim            <- c(-78.5, -76)
ylim            <- c(-14, -12)
zlim            <- c(-20,0) # Rango de profundidad de las partículas

# No cambiar nada desde aqui
nc <- nc_open(filename = paste0(dirpath, ncfile))

drifter <- rep(seq(firstdrifter, lastdrifter), each = lasttime)
timer   <- rep(seq(firsttime, lasttime), times = lastdrifter)
days    <- timer/12 # Transforma los pasos de tiempo en dias
lon     <- as.vector(t(ncvar_get(nc, 'lon',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
lat     <- as.vector(t(ncvar_get(nc, 'lat',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
depth   <- as.vector(t(ncvar_get(nc, 'depth', c(firstdrifter, firsttime), c(lastdrifter, lasttime))))

df <- data.frame(drifter, timer, days, lon, lat, depth)
colnames(df) <- c('Drifter', 'Timer', 'Days', 'Lon','Lat', 'Depth')

# Grafico en funcion de la profundidad
pngfile <- paste0(dirpath, 'trajectories_depth.png')
map   <- ggplot(data = df)
map   <- map +
  # geom_point(data = df, aes(x = Lon, y = Lat, colour = Depth), size = .075) +
  geom_path(data = df, aes(group = Drifter, x = Lon, y = Lat, colour = Depth), size = .075) +
  scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), limits = zlim, expression(Depth)) +
  labs(x = 'Longitude (W)', y = 'Latitude (S)') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim, ratio = 2/2) +
  theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
        plot.title   = element_text(face='bold', color='black', size=15, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=20),
        legend.position   = c(0.92, 0.85),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))
if(!is.null(pngfile)) ggsave(filename = pngfile, width = 9, height = 9) else map

# Grafico en funcion del tiempo
pngfile <- paste0(dirpath, 'trajectories_days.png')
map   <- ggplot(data = df)
map   <- map +
  # geom_point(data = df, aes(x = Lon, y = Lat, colour = Depth), size = .075) +
  geom_path(data = df, aes(group = Drifter, x = Lon, y = Lat, colour = Days), size = .075) +
  scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), limits = round(range(df$Days)), expression(Days)) +
  labs(x = 'Longitude (W)', y = 'Latitude (S)') +
  borders(fill='grey',colour='grey') +
  coord_fixed(xlim = xlim, ylim = ylim, ratio = 2/2) +
  theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
        axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
        axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
        plot.title   = element_text(face='bold', color='black', size=15, angle=0),
        legend.text  = element_text(face='bold', color='black', size=15),
        legend.title = element_text(face='bold', color='black', size=20),
        legend.position   = c(0.92, 0.85),
        legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))
if(!is.null(pngfile)) ggsave(filename = pngfile, width = 9, height = 9) else map
