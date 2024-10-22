#=============================================================================#
# Name   : get_polygon_rowcol_index
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Get index [row col] from ROMS grid
# URL    : 
#=============================================================================#
getpolygon_rowcol_index <- function(
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
  graphics.off()
  
  library(ncdf4)
  library(fields)
  library(mgcv)
  
  nc <- nc_open(nc_file)
  x  <- ncvar_get(nc, 'lon_rho')
  y  <- ncvar_get(nc, 'lat_rho')
  z  <- ncvar_get(nc, 'mask_rho')
  xy <- cbind(as.vector(x), as.vector(y))

  nc_close(nc)

  if(is.null(lon1) | is.null(lat1) | is.null(lon2) | is.null(lat2) |
     is.null(lon3) | is.null(lat3) | is.null(lon4) | is.null(lat4)){
    print('You need 4 latitudes and 4 longitudes, choose four points on the map:')
    
    x11()
    image.plot(x,y,z, xlab = '', ylab = '', axes = F)
    axis(side = 1, font = 2, lwd.ticks = 2, lwd = 2)
    axis(side = 2, font = 2, lwd.ticks = 2, lwd = 2, las = 2)
    box(lwd = 2)
    mtext(side = 1, line = 2.5, font = 2, cex = 1.2, text = 'Longitude')
    mtext(side = 2, line = 2.5, font = 2, cex = 1.2, text = 'Latitude')
    
    pts <- locator(n = 4, type = 'p')
    pts <- cbind(pts$x, pts$y)
    colnames(pts) <- c('lon', 'lat')
    assign(x = 'pts', value = pts, envir = .GlobalEnv)
    polygon(x = pts[,1], y = pts[,2])

    inout <- in.out(bnd = pts, x = xy)
    inout <- matrix(data = inout, nrow = dim(z)[1], ncol = dim(z)[2])
    
    z <- z + inout
    z[z != 2] <- 0
    z[z == 2] <- 1
    
    rowcol <- which(z == 1, arr.ind = T)
  }else{
    
    x11()
    image.plot(x,y,z, xlab = '', ylab = '', axes = F)
    axis(side = 1, font = 2, lwd.ticks = 2, lwd = 2)
    axis(side = 2, font = 2, lwd.ticks = 2, lwd = 2, las = 2)
    box(lwd = 2)
    mtext(side = 1, line = 2.5, font = 2, cex = 1.2, text = 'Longitude')
    mtext(side = 2, line = 2.5, font = 2, cex = 1.2, text = 'Latitude')
    
    pts <- matrix(data = c(lon1, lat1, lon2, lat2, lon3, lat3, lon4, lat4), nrow = 4, ncol = 2, byrow = T)
    colnames(pts) <- c('lon', 'lat')
    assign(x = 'pts', value = pts, envir = .GlobalEnv)
    polygon(x = pts[,1], y = pts[,2])
    
    inout <- in.out(bnd = pts, x = xy)
    inout <- matrix(data = inout, nrow = dim(z)[1], ncol = dim(z)[2])
    
    z <- z + inout
    z[z != 2] <- 0
    z[z == 2] <- 1
    
    rowcol <- which(z == 1, arr.ind = T)
  }

  colnames(rowcol) <- c('row_index', 'col_index')
  assign(x = 'PolygIndex', value = rowcol, envir = .GlobalEnv)
  
  x11()
  image.plot(x,y,z, xlab = '', ylab = '', axes = F)
  axis(side = 1, font = 2, lwd.ticks = 2, lwd = 2)
  axis(side = 2, font = 2, lwd.ticks = 2, lwd = 2, las = 2)
  box(lwd = 2)
  mtext(side = 1, line = 2.5, font = 2, cex = 1.2, text = 'Longitude')
  mtext(side = 2, line = 2.5, font = 2, cex = 1.2, text = 'Latitude')
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
dirpath <- 'D:/ROMS_SIMULATIONS/10kmparent/'
nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]

# Polygon defined for JumboSquid Norte
lon1 <- -80 # esquina superior derecha
lon2 <- -79 # esquina inferior derecha
lon3 <- -83 # esquina inferior izquierda
lon4 <- -84 # esquina superior izquierda

lat1 <- -2  # esquina superior derecha
lat2 <- -8 # esquina inferior derecha
lat3 <- -8 # esquina inferior izquierda
lat4 <- -2  # esquina superior izquierda

# # Polygon defined for JumboSquid Sur
# lon1 <- -72 # esquina superior derecha
# lon2 <- -72 # esquina inferior derecha
# lon3 <- -77 # esquina inferior izquierda
# lon4 <- -80 # esquina superior izquierda
# 
# lat1 <- -14  # esquina superior derecha
# lat2 <- -17 # esquina inferior derecha
# lat3 <- -17 # esquina inferior izquierda
# lat4 <- -14  # esquina superior izquierda


# # Polygon defined off the Peruvian coast (6-14 S, spatial resolution test)
# lon1 <- -80 # esquina superior derecha
# lon2 <- -75 # esquina inferior derecha
# lon3 <- -77 # esquina inferior izquierda
# lon4 <- -82 # esquina superior izquierda
# 
# lat1 <- -6  # esquina superior derecha
# lat2 <- -14 # esquina inferior derecha
# lat3 <- -14 # esquina inferior izquierda
# lat4 <- -6  # esquina superior izquierda

# # Polygon defined off the Peruvian coast
# lon1 <- -80
# lon2 <- -75
# lon3 <- -76
# lon4 <- -81
# lat1 <- -6
# lat2 <- -15.5
# lat3 <- -15.5
# lat4 <- -6

# # Polygon defined in the Guayaquil Gulf
# lon1 <- -82.3
# lon2 <- -79.8
# lon3 <- -79.8
# lon4 <- -82.3
# lat1 <- -2
# lat2 <- -2
# lat3 <- -4.5
# lat4 <- -4.5

# # Polygon defined off the Jumbo Squid
# lon1 <- -80
# lon2 <- -70
# lon3 <- -78
# lon4 <- -87
# lat1 <- -2
# lat2 <- -19
# lat3 <- -19
# lat4 <- -2

getpolygon_rowcol_index(nc_file = nc_file,
                        lon1 = lon1, lat1 = lat1,
                        lon2 = lon2, lat2 = lat2,
                        lon3 = lon3, lat3 = lat3,
                        lon4 = lon4, lat4 = lat4
                     )

par(mfrow = c(1,2))
plot(PolygIndex[,1], PolygIndex[,2])

m <- seq(from = 1, to = dim(PolygIndex)[1], 6)
PolygIndex <- PolygIndex[m,]

plot(PolygIndex[,1], PolygIndex[,2])
txt_name <- paste0(dirpath, 'getpolygon_rowcol_indexJumboNorte.txt')
write.table(x = PolygIndex, file = txt_name, col.names = F, row.names = F)
