#=============================================================================#
# Name   : get_ROMS_hovmuller_latitude
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('source/ROMS_hovmuller_latitude.R')
library(ncdf4)

dirpath   <- 'E:/ROMS_SIMULATIONS/10kmparent/'
sufijo    <- 'release_zone' # name of the directory where the map area to be averaged is defined
namevar   <- 'MESO'         # ROMS variable name
years     <- c(2012,2014)   # Years comprising the simulation
months    <- c(1,12)        # Months comprising the simulation
z_depth   <- -45            # Depth at which the variable is averaged. Must be a negative number
lat_lim   <- c(-20,-2)      # latitudinal range
lat_inter <- 2              # latitudinal interval, if smaller, takes longer to calculate
clim      <- F              # in case of climatological simulation

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
outpath   <- paste0(dirpath, 'interpolatedYearMonth', '/', sufijo, '/')
xy        <- read.table(paste0(outpath, sufijo, '_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(outpath, 'mask.txt')))
ver_lev   <- as.vector(read.table(paste0(outpath, 'depth.txt'), header = T))[,1]
lat       <- as.matrix(read.table(paste0(outpath, 'lat.txt')))

nc <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[2])
time_step <- length(ncvar_get(nc, 'scrum_time')) # Time steps in ROMS
nc_close(nc)

depth_lim <- range(ver_lev)

ROMS_hovmuller_latitude(
  dirpath   = outpath,
  namevar   = namevar,
  mask      = mask,
  xy        = xy,
  ver_lev   = ver_lev,
  time_step = time_step,
  years     = years,
  months    = months,
  z_depth   = z_depth,
  lat       = lat,
  lat_lim   = lat_lim,
  lat_inter = lat_inter
)

z <- hovmuller
if(clim){
  x <- seq(from = 1, to = years[2]-years[1] + 1.999, length.out = dim(z)[1]) # Para climatologico
}else{
  x <- seq(from = years[1], to = years[2]+0.999, length.out = dim(z)[1]) # Para interanual  
}
y <- seq(lat_lim[1], lat_lim[2], length.out = (lat_lim[2]-lat_lim[1])/2)
rownames(z) <- x
colnames(z) <- y

if(!is.null(k_x)){
  Rdata <- paste0(outpath, sufijo, '_',toupper(namevar), 'kx', k_x, '_hovmuller_latitude',0,z_depth,'m.Rdata')
}else{
  Rdata <- paste0(outpath, sufijo, '_', toupper(namevar), '_hovmuller_latitude',0,z_depth,'m.Rdata')
}

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = Rdata)
X11(); fields::image.plot(x,y,z)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#