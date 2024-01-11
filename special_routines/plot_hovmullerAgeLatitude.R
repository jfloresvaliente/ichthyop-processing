#=============================================================================#
# Name   : plot_hovmullerAgeLatitude
# Author : 
# Date   : 
# Version:
# Aim    : Plot Hovmuller of anual mean Age.
# URL    : 
#=============================================================================#
library(fields)

dirpath  <- 'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case1/results/'
lat_div  <- 2  # Latitudinal resolution
nlevels  <- 64 # Number of levels in the color palette
zlim     <- c(20,90)
isolines1 <- round(seq(zlim[1], zlim[2], 5), 2) # Isolines to be plotted
isolines2 <- round(seq(zlim[1], zlim[2], 20), 2) # Isolines to be plotted

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

# Plot Climatology
Rdata    <- paste0(dirpath, '/hovmuller/hovmullerAgeLatitude', lat_div, 'degrees.Rdata')
load(Rdata)

x <- hovmuller$x
y <- hovmuller$y
z <- hovmuller$z

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

png_name <- paste0(dirpath, '/hovmuller/hovmullerAgeLatitude', lat_div, 'degrees.png')
png(filename = png_name, width = 850, height = 850, res = 120)
par(mar = c(4.5, 4.5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = rev(tim.colors(length(lev)-1)),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines1, labels = isolines1, add = T)
                 axis(side = 1, font = 2, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 2))
                 box(lwd = 2)
               }
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.3, adj = 0.3, text = 'Spawning Month')
mtext(side = 2, line = 3.0, font = 2, cex = 1.3, adj = 0.5, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.0, text = 'Age at recruitment [d]')
dev.off()

print(range(hovmuller$z, na.rm = T))

# Plot Interanual
Rdata    <- paste0(dirpath, '/hovmuller/hovmullerAgeLatitudeByYear', lat_div, 'degrees.Rdata')
load(Rdata)

x <- hovmuller$x
y <- hovmuller$y
z <- hovmuller$z

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

png_name <- paste0(dirpath, '/hovmuller/hovmullerAgeLatitudeByYear', lat_div, 'degrees.png')
png(filename = png_name, width = 1850, height = 750, res = 120)
par(mar = c(4.5, 4.5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = rev(tim.colors(length(lev)-1)),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines2, labels = isolines2, add = T)
                 axis(side = 1, font = 2, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 2))
                 box(lwd = 2)
               }
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.3, adj = 0.5, text = 'Spawning Year')
mtext(side = 2, line = 3.0, font = 2, cex = 1.3, adj = 0.5, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.0, text = 'Age at recruitment [d]')

dev.off()

print(range(hovmuller$z, na.rm = T))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#