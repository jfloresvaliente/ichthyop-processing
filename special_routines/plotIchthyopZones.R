#=============================================================================#
# Name   : plotIchthyopZones
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(ncdf4)
library(fields)
library(maps)
library(mapdata)

dirpath <- 'E:/ROMS_SILUMATIONS/10kmparent/'
nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
nc      <- nc_open(nc_file)
lon     <- ncvar_get(nc, 'lon_rho')
lat     <- ncvar_get(nc, 'lat_rho')
h       <- ncvar_get(nc, 'h')
nc_close(nc)

latis <- c(-20,-2) # Latitudes de menor a mayor
longi <- c(-82.5, -70) # Longitudes de menor a mayor
bathy <- 2000 # Máxima batimería

#=================================#
# Get bathimetry between 0-200 m
h2 <- h
h2[h2 > 0 & h2 <= bathy] <- NA
h2[!is.na(h2)] <- 0
h2[is.na(h2)]  <- 1
h2[h2 == 0] <- NA

# Get longitudinal zone [-82.5 & -70 Wº]
lon2 <- lon
lon2[lon2 >= longi[1] & lon2 <= longi[2]] <- NA
lon2[!is.na(lon2)] <- 0
lon2[is.na(lon2)]  <- 1
lon2[lon2 == 0] <- NA

# Get latitudinal zone [-20 & -2 Sº]
lat2 <- lat
lat2[lat2 > latis[1] & lat2 <= latis[2]] <- NA
lat2[!is.na(lat2)] <- 0
lat2[is.na(lat2)]  <- 1
lat2[lat2 == 0] <- NA

# Sum of previous zones
m <- lon2 + lat2 + h2; rm(lon2, lat2, h2)
m[!is.na(m)] <- 1
m[is.na(m)]  <- 0

m2 <- m
new_mask <- m

# Get zones by 2º
lat_in <- seq(latis[2], latis[1]+2, -2)
lat_on <- lat_in - 2

for(i in seq_along(lat_in)){
  lat2 <- lat
  lat2[lat2 > lat_on[i] & lat2 <= lat_in[i]] <- NA
  lat2[!is.na(lat2)] <- 0
  lat2[is.na(lat2)]  <- i
  lat2 <- lat2 * m2

  new_mask <- new_mask + lat2
}

new_mask[new_mask == 0] <- NA
new_mask <- new_mask - 1

png(filename = 'C:/Users/jflores/Desktop/ich_zones_map2.png', width = 850, height = 850, res = 120)
par(mar = c(4,4,.5,.5))
image.plot(lon, lat, new_mask, xlab = '', ylab = '', xlim = c(-85,-70), ylim = c(-20,0),axes = F)
polygon(x = c(-82,-75,-75,-82), y = c(-6,-6,-14,-14), lty = 2, border = 'grey25', angle = 45, lwd = 2)
map('worldHires', add=T, fill=T, col='grey')
contour(lon[,1], lat[1,], h, levels = c(100, 500,2000), add = T, xlim = c(-85,-70), ylim = c(-20,-2), lty = 2)
axis(side = 1, font = 2, lwd = 2)
axis(side = 2, font = 2, lwd = 2, las = 2)
box(lwd = 2)
mtext(side = 1, font = 2, line = 2.5, text = 'Longitude')
mtext(side = 2, font = 2, line = 2.5, text = 'Latitude')
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#