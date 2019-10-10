library(ncdf4)
library(fields)
library(maps)
library(mapdata)

path <- 'E:/ROMS_SIMULATIONS/ROMS6B_VINCENT_SIMULATION/'
ncfile <- list.files(path = path, pattern = '.nc', full.names = T)[1]
nc <- nc_open(ncfile)
lon <- ncvar_get(nc = nc, varid = 'lon_rho')
lat <- ncvar_get(nc = nc, varid = 'lat_rho')
mask <- ncvar_get(nc = nc, varid = 'mask_rho'); mask[mask == 0] <- NA
temp <- ncvar_get(nc = nc, varid = 'temp')
temp <- temp[,,dim(temp)[3],1]

png(paste0(path,'tempsample.png'), width = 850, height = 850)
par(lwd = 2, mar = c(5,5,1,1))
image.plot(lon, lat, temp*mask,
           xlab = '', ylab = '',
           legend.width = 3,
           legend.cex = 5,
           zlim = c(15,30), xaxt= "n", yaxt= "n")
mtext(text = 'LON', side = 1, font = 2, line = 3, cex = 2)
mtext(text = 'LAT', side = 2, font = 2, line = 3, cex = 2)
axis(side = 1, font = 2, cex.axis=2, las = 1, lwd.ticks = 2)
axis(side = 2, font = 2, cex.axis=2, las = 2, lwd.ticks = 2)
map('worldHires', add = T, col = 'grey', fill = T)
box()
dev.off()
