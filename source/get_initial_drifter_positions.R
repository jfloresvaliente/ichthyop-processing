#=============================================================================#
# Name   : get_initial_drifter_positions
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
library(ncdf4)
library(fields)
library(maps)
library(mapdata)

dirpath       <- 'D:/ROMS_SIMULATIONS/peru10km/'
nc            <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
kmdist        <- 50 # Distancia en pixeles desde la costa
pixinterval   <- 2 # Intervalo de pixeles respecto a la costa
lat_1         <- -2 # Latmxax
lat_2         <- -20 # Latmin
maxdepth      <- 50 # Profundidad maxima para posicionar los drifters
intervaldepth <- 5 # Intervalo de profundaid para los drifters

#-------------- Do not change anything after here----------------#
mask <- ncvar_get(nc, 'mask_rho'); mask2 <- mask     # matrix
lon  <- ncvar_get(nc, 'lon_rho')[,1]   # matrix
lat  <- ncvar_get(nc, 'lat_rho')[1,]   # matrix
h    <- ncvar_get(nc, 'h')

a <- mask[-c(1:kmdist), ] # quito 6 filas de la parte superior
b <- matrix (0 , kmdist , ncol(a)) # crea una matriz de ceros para suplir las faltantes
c <- rbind(a,b) # suma a (con filas faltantes) y b (con las filas llenas de ceros)
mask <- mask - c # resto para tener la mascara deseada
mask[mask != 1] <- NA

ind1 <- which.min(abs(lat - lat_1))
ind2 <- which.min(abs(lat - lat_2))

mask[ , c(1:ind2, ind1:dim(mask)[2])] <- NA

# Para obtener los indices X&Y
index <- which(mask == 1, arr.ind = T)

# Para obtener lon-lat de la mascara anterior
lon_vals <- lon[index[,1]]
lat_vals <- lat[index[,2]]

# Obtener valores de profundidad en cada drifter elegido
h_vals <- NULL
for(i in 1:dim(index)[1]){
  h_vals <- c(h_vals, h[index[i,1], index[i,2]])
}
h_vals[h_vals > maxdepth] <- maxdepth
lonlat <- cbind(lon_vals, lat_vals, floor(h_vals))

# Para guardar posiciones por cada columna
lonlat <- cbind(lonlat, rep(kmdist:1, length.out = dim(index)[1]))
lonlat <- subset(lonlat, lonlat[,4] %in% seq(from = 2, to = kmdist, by = pixinterval))
lonlat <- lonlat[,-c(4)]
# Para guardar lon lat y el indice de distancia a la costa de cada particula
write.table(x = lonlat[,-c(3)], file = paste0(dirpath, 'peru_drifters.csv'), col.names = F, row.names = F, sep = ';')

x11()
image.plot(lon, lat, mask2, axes = F)
# map('worldHires', add=T, fill=T, col='gray', ylim = c(-20,0), xlim = c(-90,-70))
axis(1); axis(2, las = 2)
points(lonlat[,1], lonlat[,2], pch = 1, cex = 0.1)
box()

# Dar profundidad a los drifters
drif <- NULL
for(i in 1:dim(lonlat)[1]){
  d_h <- lonlat[i,3]/intervaldepth + 1
  
  depth <- 0
  for(j in 1:floor(d_h)){
    drif <- rbind(drif, cbind(lonlat[i,1], lonlat[i,2], depth))
    depth <- depth + intervaldepth
  }
}

drif[,3][drif[,3] == 0] <- 1 
write.table(x = drif, file = paste0(dirpath, 'peru_drifters.txt'), col.names = F, row.names = F)


# library(geosphere)
# distm(x = c(-80.41752, -6.481066), y = c(-86.17005, -6.389665))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#