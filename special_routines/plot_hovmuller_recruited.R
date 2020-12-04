#=============================================================================#
# Name   : plot_hovmuller_recruited
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Hovmuller diagram of the retention rates
# URL    : 
#=============================================================================#
library(fields)

dirpath  <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/peru02km/LatitudeBathyDepth/out/results/'
latilim  <- c(-14, -6)    # Latitude extension of the area 
zlim     <- 55            # Retention rate interval to be plotted
nlevels  <- 25            # Number of levels in the color palette
isolines <- seq(0,zlim,5) # Isolines to be plotted

#------------- Do not change anything after here -------------#

# Read data output from Ichthyop simulation
dat <- read.table(paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')

# Plotear el promedio general
z <- tapply(dat$Recruitprop, list(dat$Day, dat$Zone_name), FUN = mean, na.rm = T)
z <- z[, c(dim(z)[2]:1)]
x <- 1:12
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])

lev <- seq(from = 0, to = zlim, length.out = nlevels)

png(filename = paste0(dirpath, 'hovmuller.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(latilim[1],latilim[2], by = 2))
               })
dev.off()

png(filename = paste0(dirpath, 'hovmuller_pixel.png'), width = 850, height = 850, res = 120)
image.plot(x,y,z, axes = F, xlab = 'Months', ylab = 'Latitude', zlim = c(0, zlim))
axis(1)
axis(2)
box()
dev.off()

# # Plotear por cada profundidad de liberacion
# depths <- levels(factor(dat$Depth))
# for(i in depths){
#   z <- subset(dat, dat$Depth == i)
#   z <- tapply(z$Recruitprop, list(z$Day, z$Zone_name), FUN = mean, na.rm = T)
#   z <- z[, c(dim(z)[2]:1)]
#   x <- 1:12
#   y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
#   
#   lev <- seq(from = 0, to = zlim, length.out = nlevels)
# 
#   png(filename = paste0(dirpath, 'results/hovmullerReleaseDepth',i,'.png'), width = 850, height = 850, res = 120)
#   filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
#                  xlab = 'Months', ylab = 'Latitude',
#                  plot.axes = {
#                    contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
#                    axis(1, 1:12)
#                    axis(2, seq(latilim[1],latilim[2], by = 2))
#                  })
#   dev.off()
# }
# 
# # Plotear por cada batimetria de liberacion
# bathys <- levels(factor(dat$Bathy))
# for(i in bathys){
#   z <- subset(dat, dat$Bathy == i)
#   z <- tapply(z$Recruitprop, list(z$Day, z$Zone_name), FUN = mean, na.rm = T)
#   z <- z[, c(dim(z)[2]:1)]
#   x <- 1:12
#   y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
#   
#   lev <- seq(from = 0, to = zlim, length.out = nlevels)
#   
#   png(filename = paste0(dirpath, 'results/hovmullerReleaseBathy',i,'.png'), width = 850, height = 850, res = 120)
#   filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
#                  xlab = 'Months', ylab = 'Latitude',
#                  plot.axes = {
#                    contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
#                    axis(1, 1:12)
#                    axis(2, seq(latilim[1],latilim[2], by = 2))
#                  })
#   dev.off()
# }
#=============================================================================#
# END OF PROGRAM
#=============================================================================#