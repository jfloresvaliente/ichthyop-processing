#=============================================================================#
# Name   : plot_hovmuller
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Hovmuller diagram of the retention rates
# URL    : 
#=============================================================================#
library(fields)

dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica/out/results/'
latilim <- c(-14, -6)   # Latitude extension of the area 
zlim    <- c(50)        # Retention rate interval to be plotted
nlevels <- 25           # Number of levels in the color palette
isolines <- c(5,10,20,30,40,50) # Isolines to be plotted

#------------- Do not change anything after here -------------#
dat <- read.table(paste0(dirpath,'/ichthyop_output.csv'), header = T, sep = ';')
dat <- tapply(dat$Recruitprop, list(dat$Day, dat$Zone_name), FUN = mean, na.rm = T)
dat <- dat[, c(dim(dat)[2]:1)]

lev <- seq(from = 1, to = zlim, length.out = nlevels)

x <- 1:12
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(dat)[2])

png(filename = paste0(dirpath, 'hovmuller.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = dat, zlim = c(0,zlim), col = tim.colors(length(lev)-1),levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = dat, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(latilim[1],latilim[2], by = 2))
               })
dev.off()
