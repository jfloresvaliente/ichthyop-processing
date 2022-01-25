#=============================================================================#
# Name   : get_ROMS_hovmuller
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('source/ROMS_hovmuller.R')

dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/interpolatedYearMonth/'
sufijo    <- 'Guayaquil'
# sufijo    <- 'PeruCoast'

namevar   <- 'O2'
k_x       <- NULL # en caso se quiere calcular f, la 'namevar' debe ser MESO y k_x diferente de NULL
xy        <- read.table(paste0(dirpath, sufijo,'getpolygon_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(dirpath, 'mask.txt')))
ver_lev   <- as.vector(read.table(paste0(dirpath, 'depth.txt'), header = T))[,1]
depth_lim <- range(ver_lev)   # Latitude extension of the area 
nlevels   <- 50               # Number of levels in the color palette
years     <- c(2012, 2014)    # Years comprising the simulation
months    <- c(1,12)          # Months comprising the simulation
time_step <- 10               # Time steps in ROMS

#======= Do not change anything from here=======#
new_dir <- paste0(dirpath, sufijo,'/')
dir.create(path = paste0(dirpath, sufijo), showWarnings = F)

ROMS_hovmuller(dirpath   = dirpath,
               namevar   = namevar,
               mask      = mask,
               xy        = xy,
               ver_lev   = ver_lev,
               time_step = time_step,
               years     = years,
               months    = months,
               k_x       = k_x)

year_ini <- 1
year_out <- years[2] - years[1] + 1

x <- round(seq(from = year_ini, to = (year_out+1), length.out = ((year_out) * time_step *months[2])), digits = 3)
y <- rev(ver_lev)

rownames(hovmuller) <- x
colnames(hovmuller) <- y

if(!is.null(k_x)){
  Rdata <- paste0(new_dir, sufijo, '_', toupper(namevar), 'f', '_hovmuller.Rdata')
}else{
  Rdata <- paste0(new_dir, sufijo, '_', toupper(namevar), '_hovmuller.Rdata')
}

save(hovmuller, file = Rdata)
X11(); fields::image.plot(x,y,hovmuller)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#