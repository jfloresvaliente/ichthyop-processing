#=============================================================================#
# Name   : plot_hovmuller_latitude_tx
# Author : 
# Date   : 
# Version:
# Aim    : Plot Hovmuller matrix of recruitment at higher spatial (spawning latitude) and temporal (spawning frequency) resolution.
# URL    : 
#=============================================================================#
library(fields)
dirpath       <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/peru02km/LatitudeBathyDepth/out/results/hovmuller/'
Rdata    <- 'hovmuller0.1' # Name of Rdata file (without extention)
zlim     <- 72            # Retention rate interval to be plotted
nlevels  <- 25            # Number of levels in the color palette
isolines <- seq(0,zlim,5) # Isolines to plot

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
lev <- seq(from = 0, to = zlim, length.out = nlevels)

load(paste0(dirpath, Rdata, '.Rdata'))

x <- hovmuller$x
y <- hovmuller$y
z <- hovmuller$z

latilim <- range(y)

png(filename = paste0(dirpath, Rdata, '.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(latilim[1],latilim[2], by = 2))
               })
dev.off()

png(filename = paste0(dirpath, Rdata, 'pixel.png'), width = 850, height = 850, res = 120)
image.plot(x,y,z, axes = F, xlab = 'Months', ylab = 'Latitude', zlim = c(0, zlim))
axis(1)
axis(2)
box()
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#