#=============================================================================#
# Name   : get_ROMS_hovmuller
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath   <- 'E:/ROMS_SILUMATIONS/10kmparent/'
sufijo    <- 'release_zone' # name of the directory where the map area to be averaged is defined.
namevar   <- 'MESO'
k_x       <- 1.6 # en caso se quiere calcular f, la 'namevar' debe ser MESO y k_x diferente de NULL
years     <- c(2012, 2014)    # Years comprising the simulation (revisar linea 43)
months    <- c(1,12)          # Months comprising the simulation

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
outpath   <- paste0(dirpath, '/interpolatedYearMonth/', sufijo, '/')
xy        <- read.table(paste0(outpath, sufijo,'_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(outpath, '/mask.txt')))
ver_lev   <- as.vector(read.table(paste0(outpath, '/depth.txt'), header = T))[,1]

nc <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
time_step <- length(ncvar_get(nc, 'scrum_time')) # Time steps in ROMS
nc_close(nc)

depth_lim <- range(ver_lev)

ROMS_hovmuller(dirpath   = paste0(dirpath, '/interpolatedYearMonth/'),
               namevar   = namevar,
               mask      = mask,
               xy        = xy,
               ver_lev   = ver_lev,
               time_step = time_step,
               years     = years,
               months    = months,
               k_x       = k_x)

z <- hovmuller
# x <- seq(from = years[1], to = years[2]+0.999, length.out = dim(z)[1]) # Para interanual
x <- seq(from = 1, to = years[2]-years[1] + 1.999, length.out = dim(z)[1]) # Para climatologico
y <- rev(ver_lev)
rownames(z) <- x
colnames(z) <- y

if(!is.null(k_x)){
  Rdata <- paste0(outpath, sufijo, '_', toupper(namevar), 'kx', k_x, '_hovmuller.Rdata')
}else{
  Rdata <- paste0(outpath, sufijo, '_', toupper(namevar), '_hovmuller.Rdata')
}

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = Rdata)
X11(); fields::image.plot(x,y,z)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#