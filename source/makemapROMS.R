library(ncdf4)
library(fields)
library(maps)
library(mapdata)

path <- 'E:/ROMS_SIMULATIONS/peru02km/'
ncfile <- list.files(path = path, pattern = '.nc', full.names = T)[13]
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
           zlim = c(15,30), xaxt= 'n', yaxt= 'n')
mtext(text = 'LON', side = 1, font = 2, line = 3, cex = 2)
mtext(text = 'LAT', side = 2, font = 2, line = 3, cex = 2)
axis(side = 1, font = 2, cex.axis=2, las = 1, lwd.ticks = 2)
axis(side = 2, font = 2, cex.axis=2, las = 2, lwd.ticks = 2)
map('worldHires', add = T, col = 'grey', fill = T)
box()

# # Usar estas lineas solo para graficar un segmento donde luego se calculo un perfil de una variable 
# lon1 <- -80.50; lat1 <- -6.75; lon2 <- -76.75; lat2 <- -13.50
# segments(x0 = lon1, y0 = lat1, x1 = lon2, y1 = lat2, lwd = 4)
dev.off()
