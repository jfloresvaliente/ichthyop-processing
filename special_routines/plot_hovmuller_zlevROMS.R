#=============================================================================#
# Name   : plot_curves_age_at_recruitment_byReleaseDepthYearMean
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
library(R.matlab)
library(fields)
library(vcd)
dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/interpolatedYearMonth/'
xy        <- read.table(paste0(dirpath, 'getline_rowcol_index.txt'))
depth_lim <- c(-100, -1)   # Latitude extension of the area 
nlevels   <- 25           # Number of levels in the color palette

# # Config for V var
# namevar <- 'vz'
# zlim    <- c(-0.076, 0.076)        # Retention rate interval to be plotted
# isolines <- round(seq(zlim[1], zlim[2], 0.01), 2) # Isolines to be plotted

# # Config for U var
# namevar <- 'uz'
# zlim    <- c(-0.15, 0.15)        # Retention rate interval to be plotted
# isolines <- round(seq(zlim[1], zlim[2], 0.02), 2) # Isolines to be plotted

# Config for O2 var
namevar <- 'O2'
zlim    <- c(13, 23)        # Retention rate interval to be plotted
isolines <- round(seq(zlim[1], zlim[2], 2), 2) # Isolines to be plotted

# no cambiar desde aqui
vari <- readMat(paste0(dirpath, namevar,'.mat'))$zvar
mask <- matrix(data = NA, nrow = dim(vari)[4], ncol = dim(vari)[3])

for(i in 1:dim(xy)[1]){
  mask[ xy[i,1] , xy[i,2] ] <- 1
}

ver_mean <- NULL
for(i in 1:dim(vari)[1]){
  depth_mean <- NULL
  for(j in dim(vari)[2]:1){
    depth_mean <- c(depth_mean, mean(vari[i,j,,], na.rm = T))
  }
  ver_mean <- rbind(ver_mean, depth_mean)
}


lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

x <- 1:12
y <- rev(read.table(file = paste0(dirpath, 'zlevels.txt'))[,1])

png(filename = paste0(dirpath, namevar,'hovmuller_zone.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = ver_mean, zlim = zlim,
               col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               # col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
               contour(x = x, y = y, z = ver_mean, levels = isolines, labels = isolines, add = T)
               axis(1, 1:12)
               axis(2, seq(depth_lim[1],depth_lim[2], by = 10))
               }
)
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#
# hcl.pals() # Funcion para listar la paleta de color disponible
