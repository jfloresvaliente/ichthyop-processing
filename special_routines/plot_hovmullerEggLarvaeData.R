#=============================================================================#
# Name   : plot_hovmullerEggLarvaeData
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Para los datos de P. Ayon
# URL    : 
#=============================================================================#
library(fields)

dirpath  <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/'
lat_div  <- 1  # Latitudinal resolution
nlevels  <- 64 # Number of levels in the color palette
zlim     <- c(0,600)
isolines <- round(seq(zlim[1], zlim[2], 100), 2) # Isolines to be plotted
Rdata    <- paste0(dirpath, '/hovmuller/hovmullerEgg', lat_div, 'degrees.Rdata')
load(Rdata)

x <- hovmuller$x
y <- hovmuller$y
z <- hovmuller$z

z[z >= zlim[2]] <- zlim[2]

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

png_name <- paste0(dirpath, '/hovmuller/hovmullerEgg', lat_div, 'degrees.png')
png(filename = png_name, width = 850, height = 850, res = 120)
par(mar = c(4.5, 4.5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z, zlim = zlim,
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = .75)
                 axis(side = 1, font = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, at = seq(from = range(y)[1], to = range(y)[2], by = 2))
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.3)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.9, adj = 0.35, text = 'Month')
mtext(side = 2, line = 3.0, font = 2, cex = 1.9, adj = 0.50, text = 'Latitude')
# mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.0, text = 'Recruitment [%]')

dev.off()

print(range(hovmuller$z))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#