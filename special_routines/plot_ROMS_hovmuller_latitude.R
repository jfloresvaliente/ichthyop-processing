#=============================================================================#
# Name   : plot_ROMS_hovmuller_latitude
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')

dirpath   <- 'E:/ROMS_SILUMATIONS/rsodi1/interpolatedYearMonth/'
sufijo    <- 'release_zone'
nlevels   <- 64 # Number of levels in the color palette
z_depth   <- -45 # debe ser un numero negativo

#===== Config for temp var =====#
namevar  <- 'TEMP'
zlim     <- c(13, 28)
isolines <- seq(zlim[1], zlim[2], 3) # Isolines to be plotted
caption  <- 'Temperature [ºC]'

# #===== Config for temp var =====#
# namevar  <- 'TEMP'
# zlim     <- c(12, 22)
# isolines <- seq(zlim[1], zlim[2], 2) # Isolines to be plotted
# caption  <- 'Correction factor'

# #===== Config for MESO var =====#
# namevar  <- 'MESO'
# zlim     <- c(0, 3,5)
# isolines <- seq(zlim[1], zlim[2], 1.5) # Isolines to be plotted
# caption  <- 'Mesozooplankton [umol C L-1]'

# #===== Config for functional response (f) var =====#
# namevar  <- 'MESOkx0.2'
# zlim     <- c(0.7, 1)
# isolines <- seq(zlim[1], zlim[2], 0.2) # Isolines to be plotted
# caption  <- 'Functional response'

# #===== Config for salt var =====#
# namevar  <- 'SALT'
# zlim     <- c(34, 35.15)
# isolines <- seq(zlim[1], zlim[2], 0.1) # Isolines to be plotted
# caption  <- 'Salinity [PSU]'

# #===== Config for O2 var =====#
# namevar  <- 'O2'
# zlim     <- c(0, 250)
# isolines <- seq(zlim[1], zlim[2], 20) # Isolines to be plotted
# caption  <- 'Oxygen [umol L-1]'

# #===== Config for V var =====#
# namevar  <- 'V'
# zlim     <- c(-0.15, 0.15)
# isolines <- round(seq(zlim[1], zlim[2], 0.05), 2) # Isolines to be plotted
# caption  <- 'Velocity V [m/s]'

# #===== Config for U var =====#
# namevar  <- 'U'
# zlim     <- c(-0.18, 0.08)
# isolines <- round(seq(zlim[1], zlim[2], 0.05), 2) # Isolines to be plotted
# caption  <- 'Velocity U [m/s]'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
Rdata    <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller_latitude',0,z_depth,'m.Rdata')
png_name <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller_latitude',0,z_depth,'m.png')
load(Rdata)

x <- hovmuller$x
y <- hovmuller$y
z <- hovmuller$z

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels) # Niveles para la paleta de color
ytics <- seq(from = range(y)[1], to = range(y)[2], by = 2)
ylabs <- paste0(abs(ytics), 'ºS')

png(filename = png_name, width = 1850, height = 750, res = 120)
par(mar = c(5, 5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T, lwd = 2, labcex = 1)
                 axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 # axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = seq(1,3,.5))
                 axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = ytics, labels = ylabs)
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Years')
mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.00, text = caption)

dev.off()

print(range(z))
# hcl.pals() # Funcion para listar la paleta de color disponible
#=============================================================================#
# END OF PROGRAM
#=============================================================================#