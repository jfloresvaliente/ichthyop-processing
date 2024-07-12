#=============================================================================#
# Name   : get_ROMSmat2NA
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('source/ROMSmat2NA.R')
library(ncdf4)

dirpath   <- 'E:/ROMS_SIMULATIONS/rsodi1/'
sufijo    <- 'release_zone' # name of the directory where the map area to be averaged is defined.
namevar   <- 'MESO'
years     <- c(1980,2000)  # Years comprising the simulation
months    <- c(1,12)        # Months comprising the simulation

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
outpath   <- paste0(dirpath, '/interpolatedYearMonth/', sufijo, '/')
xy        <- read.table(paste0(outpath, sufijo,'_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(outpath, '/mask.txt')))
ver_lev   <- as.vector(read.table(paste0(outpath, '/depth.txt'), header = T))[,1]

nc <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[2])
time_step <- length(ncvar_get(nc, 'scrum_time')) # Time steps in ROMS

nc_close(nc)

depth_lim <- range(ver_lev)

ROMSmat2NA(dirpath       = paste0(dirpath, 'interpolatedYearMonth/'),
               namevar   = namevar,
               mask      = mask,
               xy        = xy,
               ver_lev   = ver_lev,
               time_step = time_step,
               years     = years,
               months    = months,
               sufijo    = sufijo
)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#