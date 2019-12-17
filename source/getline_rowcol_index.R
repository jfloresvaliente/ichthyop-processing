#=============================================================================#
# Name   : getline_rowcol_index
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Obtain the indexes [row col] of a grid for later calculations
# URL    : 
#=============================================================================#
getline_rowcol_index <- function(
  nc_file,
  lon1 = NULL,
  lat1 = NULL,
  lon2 = NULL,
  lat2 = NULL)
  {
  
  #============ ============ Arguments ============ ============#
  
  # ncfile = ROMS file name
  # lon1 = longitud del punto 1
  # lat1 = latitud del punto 1
  # lon2 = longitud del punto 2
  # lat2 = latitud del punto 2

  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(fields)
  
  nc <- nc_open(nc_file)
  
  x <- ncvar_get(nc, 'lon_rho')
  y <- ncvar_get(nc, 'lat_rho')
  z <- ncvar_get(nc, 'mask_rho')
  
  assign(x = 'lon' , value = x, envir = .GlobalEnv)
  assign(x = 'lat' , value = y, envir = .GlobalEnv)
  assign(x = 'mask', value = z, envir = .GlobalEnv)
  
  nc_close(nc)
  
  if(is.null(lon1) | is.null(lat1) | is.null(lon2) | is.null(lat2)){
    print('You need 2 latitudes and 2 longitudes, choose two points on the map:')
    x11()
    image.plot(x,y,z, xlab = 'Longitude', ylab = 'Latitude')
    pts <- locator(n = 2, type = 'p')
    pts <- cbind(pts$x, pts$y)
    pts <- pts[rev(order(pts[,1])),]
    
    colnames(pts) <- c('lon', 'lat')
    assign(x = 'pts', value = pts, envir = .GlobalEnv)
    
    mod <- lm(pts[,2] ~ pts[,1])
    coefi  <- coef(mod)
    
    newX <- seq(pts[1,1], pts[2,1], by = -1/3000)
    newY <- coefi[1] + (coefi[2] * newX)
    
    lines(newX, newY)
  }else{
    x11()
    image.plot(x,y,z, xlab = 'Longitude', ylab = 'Latitude')
    pts <- matrix(data = c(lon1, lat1, lon2, lat2), nrow = 2, ncol = 2, byrow = T)
    pts <- pts[rev(order(pts[,1])),]
    colnames(pts) <- c('lon', 'lat')
    assign(x = 'pts', value = pts, envir = .GlobalEnv)
    mod <- lm(pts[,2] ~ pts[,1])
    coefi  <- coef(mod)
    newX <- seq(pts[1,1], pts[2,1], by = -1/3000)
    newY <- coefi[1] + (coefi[2] * newX)
    
    lines(newX, newY)
  }

  # Obtain the indices through the line formed between both points
  rowcol <- NULL
  for(i in 1:length(newX)){
    x1 <- abs(x - newX[i])
    y1 <- abs(y - newY[i])
    xy1 <- x1 + y1
    n <- which(xy1 == min(xy1), arr.ind = T)
    rowcol <- rbind(rowcol, n[1,])
  }
  
  # Remove duplicates
  rowcol <- rowcol[!duplicated(rowcol), ]
  
  # Eliminate points on the ground
  cospoint <- NULL
  for(i in 1:dim(rowcol)[1]){
    if(z[rowcol[i,1], rowcol[i,2]] == 0) cospoint <- c(cospoint, i)
  }
  if(!is.null(cospoint)) rowcol <- rowcol[-c(cospoint),]

  colnames(rowcol) <- c('row_index', 'col_index')
  assign(x = 'LineIndex', value = rowcol, envir = .GlobalEnv)
  
  for(i in 1:dim(rowcol)[1]){
    a <- rowcol[i,1]
    b <- rowcol[i,2]
    z[a,b] <- NA
  }
  x11()
  image.plot(x,y,z, xlab = 'Longitude', ylab = 'Latitude')
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
dirpath <- 'E:/ROMS_SIMULATIONS/peru10km/'
nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[13]
lon1 <- -80.50; lat1 <- -6.75; lon2 <- -76.75; lat2 <- -13.50
getline_rowcol_index(nc_file = nc_file, lon1 = lon1, lat1 = lat1, lon2 = lon2, lat2 = lat2)
write.table(x = LineIndex, file = paste0(dirpath, 'xy.txt'), col.names = F, row.names = F)
