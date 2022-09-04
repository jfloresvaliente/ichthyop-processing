#=============================================================================#
# Name   : get_release_zone_rowcol_index
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

dirpath <- 'D:/ROMS_SILUMATIONS/rsodi1/'
nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
nc      <- nc_open(nc_file)
lon     <- ncvar_get(nc, 'lon_rho')
lat     <- ncvar_get(nc, 'lat_rho')
h       <- ncvar_get(nc, 'h')
mask    <- ncvar_get(nc, 'mask_rho'); mask[mask == 0] <- NA
nc_close(nc)

latis <- c(-20,-2) # Latitudes de menor a mayor
longi <- c(-82.5, -70) # Longitudes de menor a mayor
bathy <- 2000 # Máxima batimería

#=================================#
# Get bathimetry between 0-2000 m
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
m <- mask * m

m <- which(m == 1, arr.ind = T)
csv_name <- paste0(dirpath, 'release_zone_rowcol_index.txt')
write.table(x = m, file = csv_name, col.names = F, row.names = F)
