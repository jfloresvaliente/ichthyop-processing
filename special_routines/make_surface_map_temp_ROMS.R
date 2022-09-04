#=============================================================================#
# Name   : make_surface_map_temp_ROMS
# Author : 
# Date   : 
# Version:
# Aim    : Plot a sample map (all domain) of temperature for a ROMS file
# URL    : 
#=============================================================================#
library(ncdf4)
library(fields)
library(maps)
library(mapdata)

dirpath <- 'D:/ROMS_SILUMATIONS/10kmparent/'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
nc      <- nc_open(nc_file)
lon     <- ncvar_get(nc, 'lon_rho')
lat     <- ncvar_get(nc, 'lat_rho')
vari    <- ncvar_get(nc, 'temp'); vari <- vari[,,dim(vari)[3],1]
mask    <- ncvar_get(nc, 'mask_rho'); mask[mask == 0] <- NA

png(filename = paste0(dirpath, 'surface_map_temp_ROMS.png'), width = 850, height = 850, res = 120)
par(mar = c(4.5,4.5,.5,.5))
image.plot(lon, lat, vari*mask, xlab = '', ylab = '', zlim = c(15,30), axes = F,
           legend.width = 2.5, axis.args = list(cex.axis = 1.5, font.axis = 2))
map('worldHires', add = T, fill = T, col = 'black')
axis(side = 1, font = 2, lwd = 2, cex.axis = 1.5)
axis(side = 2, font = 2, lwd = 2, cex.axis = 1.5, las = 2)
box(lwd = 2)
mtext(side = 1, font = 2, line = 2.8, cex = 1.5, text = 'Longitude')
mtext(side = 2, font = 2, line = 2.8, cex = 1.5, text = 'Latitude')

# # Usar estas lineas solo para graficar un segmento donde luego se calculo un perfil de una variable 
# lon1 <- -80.50; lat1 <- -6.75; lon2 <- -76.75; lat2 <- -13.50
# segments(x0 = lon1, y0 = lat1, x1 = lon2, y1 = lat2, lwd = 4)
dev.off()

rm(list = ls())
#=============================================================================#
# END OF PROGRAM
#=============================================================================#