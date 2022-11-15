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

dirpath   <- 'E:/ROMS_SILUMATIONS/rsodi1/'
sufijo    <- 'release_zone'
namevar   <- 'MESO'
k_x       <- 0.6 # en caso se quiere calcular f, la 'namevar' debe ser MESO y k_x diferente de NULL
xy        <- read.table(paste0(dirpath, '/interpolatedYearMonth/',sufijo,'_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(dirpath, '/interpolatedYearMonth/mask.txt')))
ver_lev   <- as.vector(read.table(paste0(dirpath, '/interpolatedYearMonth/depth.txt'), header = T))[,1]
years     <- c(1980, 2000)    # Years comprising the simulation (Revisar linea 52)
months    <- c(1,12)          # Months comprising the simulation
lat       <- as.matrix(read.table(paste0(dirpath, '/interpolatedYearMonth/lat.txt')))
z_depth   <- -45 # Debe ser un numero negativo
lat       <- lat
lat_lim   <- c(-20,-2)

#======= Do not change anything from here=======#
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
x <- seq(from = years[1], to = years[2]+0.999, length.out = dim(z)[1]) # Para interanual
# x <- seq(from = 1, to = years[2]-years[1] + 1.999, length.out = dim(z)[1]) # Para climatologico
y <- seq(lat_lim[1], lat_lim[2], length.out = (lat_lim[2]-lat_lim[1])/2)
rownames(z) <- x
colnames(z) <- y

if(!is.null(k_x)){
  Rdata <- paste0(new_dir, sufijo, '_', toupper(namevar), 'kx', k_x, '_hovmuller_latitude',0,z_depth,'m.Rdata')
}else{
  Rdata <- paste0(new_dir, sufijo, '_', toupper(namevar), '_hovmuller_latitude',0,z_depth,'m.Rdata')
}

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = Rdata)
X11(); fields::image.plot(x,y,z)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#