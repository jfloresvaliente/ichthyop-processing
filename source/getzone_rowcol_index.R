#=============================================================================#
# Name   : getzone_rowcol_index
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Obtain the indexes [row col] of a grid for later calculations
# URL    : 
#=============================================================================#
getzone_rowcol_index <- function(
  nc_file,
  lon1 = NULL,
  lat1 = NULL,
  lon2 = NULL,
  lat2 = NULL,
  lon3 = NULL,
  lat3 = NULL,
  lon4 = NULL,
  lat4 = NULL)
{
  
  #============ ============ Arguments ============ ============#
  
  # ncfile = ROMS file name
  # lon1 = longitud del punto 1
  # lat1 = latitud del punto 1
  # lon2 = longitud del punto 2
  # lat2 = latitud del punto 2
  # lon3 = longitud del punto 3
  # lat3 = latitud del punto 3
  # lon4 = longitud del punto 4
  # lat4 = latitud del punto 4
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(fields)
  library(sp)
  
  nc <- nc_open(nc_file)
  
  x <- ncvar_get(nc, 'lon_rho')
  y <- ncvar_get(nc, 'lat_rho')
  z <- ncvar_get(nc, 'mask_rho')
  
  assign(x = 'lon' , value = x, envir = .GlobalEnv)
  assign(x = 'lat' , value = y, envir = .GlobalEnv)
  assign(x = 'mask', value = z, envir = .GlobalEnv)
  
  nc_close(nc)
  
  if(is.null(lon1) | is.null(lat1) | is.null(lon2) | is.null(lat2) |
     is.null(lon3) | is.null(lat3) | is.null(lon4) | is.null(lat4)){
    print('You need 4 latitudes and 4 longitudes, choose four points on the map:')
    x11()
    image.plot(x,y,z, xlab = 'Longitude', ylab = 'Latitude')
    pts <- locator(n = 4, type = 'p')
    pts <- cbind(pts$x, pts$y)
    polygon(x = pts[,1], y = pts[,2])
    colnames(pts) <- c('lon', 'lat')
    assign(x = 'pts', value = pts, envir = .GlobalEnv)
    
    Sr1 = Polygon(cbind(pts[,1],pts[,2]))
    inzone <- point.in.polygon(point.x = as.numeric(x), point.y = as.numeric(y),
                               pol.x = Sr1@coords[,1], pol.y = Sr1@coords[,2])
    indexoutzone <- which(inzone == 0)
    z2 <- z
    z2[indexoutzone] <- NA
    rowcol <- which(z2 == 1, arr.ind = T)
    for(i in 1:dim(rowcol)[1]){
      a <- rowcol[i,1]
      b <- rowcol[i,2]
      z[a,b] <- NA
    }
  }else{
    x11()
    image.plot(x,y,z, xlab = 'Longitude', ylab = 'Latitude')
    pts <- matrix(data = c(lon1, lat1, lon2, lat2, lon3, lat3, lon4, lat4), nrow = 4, ncol = 2, byrow = T)
    polygon(x = pts[,1], y = pts[,2])
    colnames(pts) <- c('lon', 'lat')
    assign(x = 'pts', value = pts, envir = .GlobalEnv)
    
    Sr1 = Polygon(cbind(pts[,1],pts[,2]))
    inzone <- point.in.polygon(point.x = as.numeric(x), point.y = as.numeric(y),
                               pol.x = Sr1@coords[,1], pol.y = Sr1@coords[,2])
    indexoutzone <- which(inzone == 0)
    z2 <- z
    z2[indexoutzone] <- NA
    rowcol <- which(z2 == 1, arr.ind = T)
    for(i in 1:dim(rowcol)[1]){
      a <- rowcol[i,1]
      b <- rowcol[i,2]
      z[a,b] <- NA
    }
  }

  colnames(rowcol) <- c('row_index', 'col_index')
  assign(x = 'ZoneIndex', value = rowcol, envir = .GlobalEnv)
  x11()
  image.plot(x,y,z, xlab = 'Longitude', ylab = 'Latitude')
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
dirpath <- 'E:/ROMS_SIMULATIONS/peru02km/'
nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[13]
lon1 <- -80
lon2 <- -75
lon3 <- -76
lon4 <- -81
lat1 <- -6
lat2 <- -15.5
lat3 <- -15.5
lat4 <- -6

getzone_rowcol_index(nc_file = nc_file,
                     lon1 = lon1, lat1 = lat1,
                     lon2 = lon2, lat2 = lat2,
                     lon3 = lon3, lat3 = lat3,
                     lon4 = lon4, lat4 = lat4
                     )
write.table(x = ZoneIndex, file = paste0(dirpath, 'xyZoneIndex.txt'), col.names = F, row.names = F)
