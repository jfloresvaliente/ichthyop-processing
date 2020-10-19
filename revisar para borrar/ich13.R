source('source/ichthyop_libraries.R')
dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO60dias/results_DEB/'

# Histograma de Longitud al final de la simulacion - INICIO
length_interval <- 1 # intervalo en mm
length_max      <- 100 # xlim maximo en mm

png(filename = paste0(dirpath, 'hist_length61days.png'), width = 1250, height = 750, res = 120)

par(mfrow = c(3,4), mar = c(3,4,1,.5))
for(month in 1:12){
  rfile <- paste0(dirpath, 'trajectoriesM', month, '.Rdata')
  load(rfile); print(rfile)
  dat <- trajectories; rm(trajectories)

  # Obtener el indice del ultimo paso de tiempo de la simulacion
  lastday <- length(levels(factor(dat$Timer)))

  d_last <- subset(dat, dat$Timer == lastday)
  dens <- density(d_last$length, adjust = length_interval)

  hist(x = d_last$length,
       xlim = c(0, length_max), ylim = c(0, 0.1),
       prob = T, breaks = seq(0,length_max, length_interval),
       xlab = '', ylab = '', main = '',
       xaxs='i',yaxs='i')
  lines(dens, col = 'red', lwd = 2)
  mtext(side = 1, line = 2, cex = .85, text = 'Final Length (mm)')
  mtext(side = 2, line = 2, cex = .85, text = 'Density')
  legend('topleft', legend = paste0('Month', month), bty = 'n')
  box()
}
dev.off()
# Histograma de Longitud al final de la simulacion - FINAL


