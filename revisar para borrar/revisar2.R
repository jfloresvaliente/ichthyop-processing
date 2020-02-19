library(ncdf4)
library(fields)
library(maps)
library(mapdata)

path <- 'E:/ROMS_SIMULATIONS/peru02km/'

rang <- NULL
for(i in 1:12){
  ncfile <- paste0(path, 'roms_avg_Y2009', 'M', i, '.newperushtopoP.nc')
  # ncfile <- paste0(path, 'roms_avg_Y2009', 'M', i,'.AscatMerClim.nc')
  print(ncfile)
  nc <- nc_open(ncfile)
  
  if(i == 1){
    lon <- ncvar_get(nc = nc, varid = 'lon_rho')
    lat <- ncvar_get(nc = nc, varid = 'lat_rho')
    mask <- ncvar_get(nc = nc, varid = 'mask_rho'); mask[mask == 0] <- NA
  }
  temp <- ncvar_get(nc = nc, varid = 'temp')
  temp <- temp[,,dim(temp)[3],]
  # temp[temp == 0] <- NA
  # rang <- rbind(rang, range(temp, na.rm = T))
  
  for(j in 1:dim(temp)[3]){
    samp <- temp[,,j]
    png(paste0(path,'month',i,'step',j,'.png'), width = 850, height = 850)
    par(lwd = 2, mar = c(5,5,1,1))
    image.plot(lon, lat, samp*mask,
               xlab = '', ylab = '',
               legend.width = 3,
               legend.cex = 5,
               zlim = c(10,30), xaxt= 'n', yaxt= 'n')
    mtext(text = 'LON', side = 1, font = 2, line = 3, cex = 2)
    mtext(text = 'LAT', side = 2, font = 2, line = 3, cex = 2)
    axis(side = 1, font = 2, cex.axis=2, las = 1, lwd.ticks = 2)
    axis(side = 2, font = 2, cex.axis=2, las = 2, lwd.ticks = 2)
    map('worldHires', add = T, col = 'grey', fill = T)
    box()
    legend('topright', legend = paste('Month', i), cex = 2, bty = 'n', xjust = 0)
    dev.off()
  }
}
