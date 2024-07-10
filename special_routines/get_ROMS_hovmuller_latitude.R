#=============================================================================#
# Name   : get_ROMS_hovmuller_latitude
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath   <- 'D:/ROMS_SIMULATIONS/peru02km/'
sufijo    <- 'release_zone' # name of the directory where the map area to be averaged is defined.
namevar   <- 'u'
k_x       <- NULL           # en caso se quiere calcular f, la 'namevar' debe ser MESO y k_x diferente de NULL
years     <- c(2009, 2009)  # Years comprising the simulation (Revisar linea 52)
months    <- c(1,12)        # Months comprising the simulation
clim      <- T # en caso de ser una simulacion climatologica

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
outpath   <- paste0(dirpath, '/interpolatedYearMonth/', sufijo, '/')
xy        <- read.table(paste0(outpath, sufijo,'_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(outpath, '/mask.txt')))
ver_lev   <- as.vector(read.table(paste0(outpath, '/depth.txt'), header = T))[,1]

lat       <- as.matrix(read.table(paste0(outpath, '/lat.txt')))
z_depth   <- -45 # Debe ser un numero negativo
lat_lim   <- c(-20,-2)

nc <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
time_step <- length(ncvar_get(nc, 'scrum_time')) # Time steps in ROMS
nc_close(nc)

depth_lim <- range(ver_lev)

new_dir <- paste0(dirpath, '/interpolatedYearMonth/', sufijo,'/')
dir.create(path = paste0(new_dir), showWarnings = F)

ROMS_hovmuller_latitude(dirpath   = paste0(dirpath, '/interpolatedYearMonth/'),
               namevar   = namevar,
               mask      = mask,
               xy        = xy,
               ver_lev   = ver_lev,
               time_step = time_step,
               years     = years,
               months    = months,
               k_x       = k_x,
               z_depth   = z_depth,
               lat       = lat,
               lat_lim   = lat_lim
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