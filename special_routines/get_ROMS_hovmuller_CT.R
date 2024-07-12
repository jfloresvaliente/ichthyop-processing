#=============================================================================#
# Name   : get_ROMS_hovmuller_CT
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('source/ROMS_hovmuller_CT.R')
library(ncdf4)

dirpath   <- 'E:/ROMS_SIMULATIONS/10kmparent/'
sufijo    <- 'release_zone' # name of the directory where the map area to be averaged is defined.
namevar   <- 'temp'         # variable name associated to the temperature in degrees Celsius.
sp        <- 'ringens' # species name.
years     <- c(2012, 2014)  # Years comprising the simulation (revisar linea 43).
months    <- c(1,12)        # Months comprising the simulation.
clim      <- T # en caso de ser una simulacion climatologica

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

T_K <- 273.15

if(sp == 'encrasicolus'){
  # Engraulis encrasicolus
  T_ref  <- 16
  T_A    <- 9800
}

if(sp == 'ringens'){
  # Engraulis ringens
  T_ref  <- 20
  T_A    <- 9576
  
}

# Calculate TC for Case 1
#===== CURVA 1 =====#
# Parameters
T_L  <- 6        # C Lower temp boundary
T_H  <- 21       # C Upper temp boundary
T_A  <- T_A      # K Arrhenius temperature
T_AL <- 20000    # K Arrh. temp for lower boundary
T_AH <- 190000/2 # K Arrh. temp for upper boundary

outpath   <- paste0(dirpath, 'interpolatedYearMonth', '/', sufijo, '/')
xy        <- read.table(paste0(outpath, sufijo, '_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(outpath, 'mask.txt')))
ver_lev   <- as.vector(read.table(paste0(outpath, 'depth.txt'), header = T))[,1]

nc <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[2])
time_step <- length(ncvar_get(nc, 'scrum_time')) # Time steps in ROMS
nc_close(nc)

depth_lim <- range(ver_lev)

ROMS_hovmuller_CT(dirpath   = outpath,
                  namevar   = namevar,
                  mask      = mask,
                  xy        = xy,
                  ver_lev   = ver_lev,
                  time_step = time_step,
                  years     = years,
                  months    = months,
                  T_K       = T_K,
                  T_ref     = T_ref,
                  T_A       = T_A, 
                  T_L       = T_L, 
                  T_H       = T_H, 
                  T_AL      = T_AL, 
                  T_AH      = T_AL
                  )

z <- hovmuller
if(clim){
  x <- seq(from = 1, to = years[2]-years[1] + 1.999, length.out = dim(z)[1]) # Para climatologico
}else{
  x <- seq(from = years[1], to = years[2]+0.999, length.out = dim(z)[1]) # Para interanual  
}
y <- rev(ver_lev)
rownames(z) <- x
colnames(z) <- y

Rdata <- paste0(outpath, sufijo, '_', toupper(namevar), 'CTcase1', sp, '_hovmuller.Rdata')

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = Rdata)
X11(); fields::image.plot(x,y,z)

# Calculate TC for Case 2
#===== CURVA 2 =====#
# Parameters
T_L  <- 6        # C Lower temp boundary
T_H  <- 24       # C Upper temp boundary
T_A  <- T_A      # K Arrhenius temperature
T_AL <- 20000    # K Arrh. temp for lower boundary
T_AH <- 190000*3 # K Arrh. temp for upper boundary

outpath   <- paste0(dirpath, 'interpolatedYearMonth', '/', sufijo, '/')
xy        <- read.table(paste0(outpath, sufijo, '_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(outpath, 'mask.txt')))
ver_lev   <- as.vector(read.table(paste0(outpath, 'depth.txt'), header = T))[,1]

nc <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[2])
time_step <- length(ncvar_get(nc, 'scrum_time')) # Time steps in ROMS
nc_close(nc)

depth_lim <- range(ver_lev)

ROMS_hovmuller_CT(dirpath   = outpath,
                  namevar   = namevar,
                  mask      = mask,
                  xy        = xy,
                  ver_lev   = ver_lev,
                  time_step = time_step,
                  years     = years,
                  months    = months,
                  T_K       = T_K,
                  T_ref     = T_ref,
                  T_A       = T_A, 
                  T_L       = T_L, 
                  T_H       = T_H, 
                  T_AL      = T_AL, 
                  T_AH      = T_AL
)

z <- hovmuller
if(clim){
  x <- seq(from = 1, to = years[2]-years[1] + 1.999, length.out = dim(z)[1]) # Para climatologico
}else{
  x <- seq(from = years[1], to = years[2]+0.999, length.out = dim(z)[1]) # Para interanual  
}
y <- rev(ver_lev)
rownames(z) <- x
colnames(z) <- y

Rdata <- paste0(outpath, sufijo, '_', toupper(namevar), 'CTcase2', sp, '_hovmuller.Rdata')

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = Rdata)
X11(); fields::image.plot(x,y,z)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#