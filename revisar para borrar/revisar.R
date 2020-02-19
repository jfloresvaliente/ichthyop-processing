library(maps)
library(mapdata)
library(R.matlab)
library(fields)
library(ncdf4)

namevar <- 'tempz'
dirpath <- 'E:/ROMS_SIMULATIONS/peru02km/'
# no cambiar desde aqui
lon <- as.matrix(read.table(paste0(dirpath, 'lon_rho.txt')))
lat <- as.matrix(read.table(paste0(dirpath, 'lat_rho.txt')))
vari <- readMat(paste0(dirpath, namevar,'.mat'))$zvar


for(i in 1:12){
  nc <- nc_open(paste0(dirpath, 'roms_avg_Y2009M' ,i, '.newperushtopoP.nc'))
  vari <- ncvar_get(nc, 'temp')
  snap <- vari[,,42,1]
  
  png(paste0(dirpath, namevar, '_month',i,'map.png'), width = 850, height = 850)
  par(lwd = 2, mar = c(5,5,1,1))
  image.plot(lon, lat, snap, zlim = c(13, 28),xaxs='i',yaxs='i', xlab = '', ylab = '', xaxt= 'n', yaxt= 'n')
  # mtext(text = 'LON', side = 1, font = 2, line = 3, cex = 2)
  # mtext(text = 'LAT', side = 2, font = 2, line = 3, cex = 2)
  axis(side = 1, font = 2, cex.axis=2, las = 1, lwd.ticks = 2)
  axis(side = 2, font = 2, cex.axis=2, las = 2, lwd.ticks = 2)
  map('worldHires', add = T, col = 'grey', fill = T)
  box()
  legend('topright', legend = paste0('Month', i), cex = 2, text.width = 2, bty = 'n')
  dev.off()
}

# png('C:/Users/jflores/Desktop/peru.png')
# par(mar = c(3,3,.5,.5), lwd = 2)
# plot(-90:-70, -20:0, type = 'n', xlab = 'LON', ylab = 'LAT', axes = F, xaxs='i',yaxs='i')
# # abline(v = c(seq(-90,-70,2)), h = c(seq(-20,0,2)))
# # points(x = rep(seq(-89,-71,2), 10), y = rep(seq(-19,-1,2), each = 10), col = 'red', cex = 1.5, pch = 16)
# map('worldHires', add=T, fill=T, col='gray', ylim = c(-20, 0), xlim = c(-90,-70))
# axis(1, font = 2, lwd.ticks = 2)
# axis(2, font = 2, lwd.ticks = 2, las = 2)
# box()
# dev.off()
