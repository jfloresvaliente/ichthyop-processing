#=============================================================================#
# Name   : plot_ROMS_hovmuller
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')

dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/interpolatedYearMonth/'
sufijo    <- 'Guayaquil'
# sufijo    <- 'PeruCoast'
nlevels   <- 64 # Number of levels in the color palette

# #===== Config for temp var =====#
# namevar  <- 'TEMP'
# zlim     <- c(12, 24)
# isolines <- seq(zlim[1], zlim[2], 2) # Isolines to be plotted
# caption  <- expression('Temperature [ºC]')

# #===== Config for MESO var =====#
# namevar  <- 'MESO'
# zlim     <- c(0, 5.5)
# isolines <- seq(zlim[1], zlim[2], 1) # Isolines to be plotted
# caption  <- expression('Mesozooplankton [umol C L-1]')

# #===== Config for functional response (f) var =====#
# namevar  <- 'MESOf'
# zlim     <- c(0.1, 0.8)
# isolines <- seq(zlim[1], zlim[2], 0.1) # Isolines to be plotted
# caption  <- expression('Functional response [#]')

# #===== Config for salt var =====#
# namevar  <- 'SALT'
# zlim     <- c(34, 35.15)
# isolines <- seq(zlim[1], zlim[2], 0.1) # Isolines to be plotted
# caption  <- expression('Salinity [PSU]')

# #===== Config for O2 var =====#
# namevar  <- 'O2'
# zlim     <- c(0, 250)
# isolines <- seq(zlim[1], zlim[2], 20) # Isolines to be plotted
# caption  <- expression('Oxygen [umol L-1]')

# #===== Config for V var =====#
# namevar  <- 'V'
# zlim     <- c(-0.15, 0.15)
# isolines <- round(seq(zlim[1], zlim[2], 0.05), 2) # Isolines to be plotted
# caption  <- expression('Velocity V [m/s]')

#===== Config for U var =====#
namevar  <- 'U'
zlim     <- c(-0.18, 0.08)
isolines <- round(seq(zlim[1], zlim[2], 0.05), 2) # Isolines to be plotted
caption  <- expression('Velocity U [m/s]')

#======= Do not change anything from here=======#
Rdata    <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller.Rdata')
png_name <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller.png')
load(Rdata)

x <- as.numeric(rownames(hovmuller))
y <- as.numeric(colnames(hovmuller))

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)

png(filename = png_name, width = 1250, height = 750, res = 120)
par(mar = c(4.5, 4.5, 3.5, 3.5))
filled.contour(x = x, y = y, z = hovmuller, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = hovmuller, levels = isolines, labels = isolines, add = T)
                 axis(side = 1, font = 2, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 10))
                 box(lwd = 2)
               }
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.3, text = 'Years of simulation')
mtext(side = 2, line = 3.0, font = 2, cex = 1.3, text = 'Depth [m]')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, text = caption, adj = 0)

dev.off()

print(range(hovmuller))
# hcl.pals() # Funcion para listar la paleta de color disponible
#=============================================================================#
# END OF PROGRAM
#=============================================================================#