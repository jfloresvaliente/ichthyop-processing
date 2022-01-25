#=============================================================================#
# Name   : get_zone_rowcol_index
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Get index [row col] from ROMS grid
# URL    : 
#=============================================================================#
getzone_rowcol_index <- function(
  nc_file,
  polyg = polyg
  ){
  #============ ============ Arguments ============ ============#
  
  # ncfile = ROMS file name
  # polyg  = polygono (lon/lat) que representa la zone de reclutamiento
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(fields)
  library(mgcv)
  
  nc <- nc_open(nc_file)
  x  <- ncvar_get(nc, 'lon_rho')
  y  <- ncvar_get(nc, 'lat_rho')
  z  <- ncvar_get(nc, 'mask_rho'); z[z == 0] <- NA
  xy <- cbind(as.vector(x), as.vector(y))
  
  nc_close(nc)
  
  inout <- in.out(bnd = polyg, x = xy)
  inout <- matrix(data = inout, nrow = dim(z)[1], ncol = dim(z)[2])
  inout <- inout * 1
  
  z <- z + inout
  z[z != 2] <- 0
  z[z == 2] <- 1
  
  rowcol <- which(z == 1, arr.ind = T)
  
  colnames(rowcol) <- c('row_index', 'col_index')
  assign(x = 'ZoneIndex', value = rowcol, envir = .GlobalEnv)
  x11()
  image.plot(x,y,z, xlab = 'Longitude', ylab = 'Latitude')
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
dirpath <- 'E:/ROMS_SILUMATIONS/10kmparent/'
nc_file <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
polyg   <- as.matrix(read.table('E:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/ichthyop_recruitment_polygon.txt'))
getzone_rowcol_index(nc_file = nc_file, polyg = polyg)
csv_name <- paste0(dirpath, 'getzone_rowcol_index.txt')
write.table(x = ZoneIndex, file = csv_name, col.names = F, row.names = F)