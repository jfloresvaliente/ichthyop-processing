#=============================================================================#
# Name   : plot_ROMS_hovmuller_latitude_Climatology
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')

dirpath   <- 'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/interpolatedYearMonth/'
sufijo    <- 'release_zone'
nlevels   <- 64 # Number of levels in the color palette
z_depth   <- -45 # debe ser un numero negativo
# years <- 1:3 # 10 km
years <- 1980:2000 # rsodi

#===== Config for temp var =====#
namevar  <- 'TEMP'
zlim     <- c(12, 28)
isolines <- seq(zlim[1], zlim[2], 1) # Isolines to be plotted
caption  <- 'Temperature [ºC]'

# #===== Config for MESO var =====#
# namevar  <- 'MESO'
# zlim     <- c(0, 5)
# isolines <- seq(zlim[1], zlim[2], 1.5) # Isolines to be plotted
# caption  <- 'Mesozooplankton [umol C L-1]'

# #===== Config for functional response (f) var =====#
# namevar  <- 'MESOf'
# zlim     <- c(0.2, 0.7)
# isolines <- seq(zlim[1], zlim[2], 0.1) # Isolines to be plotted
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
png_name <- paste0(dirpath, sufijo, '/',sufijo,'_', namevar, '_hovmuller_latitude',0,z_depth,'m_Climatology.png')
load(Rdata)

z <- hovmuller$z
y <- hovmuller$y
new_x <- dim(z)[1]/length(years)
x <- seq(from = 1, to = 12.999, length.out = new_x)

z2 <- array(data = NA, dim = c(new_x, dim(z)[2], length(years)))
ind_in <- seq(from = 1, length.out = length(years), by = new_x)
ind_on <- seq(from = new_x, length.out = length(years), by = new_x)
for(i in 1:length(years)){
  z2[,,i] <- z[ind_in[i] : ind_on[i],]
}
z <- apply(z2, c(1,2), mean, na.rm = T)

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels) # Niveles para la paleta de color

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
                 axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 2))
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
)
mtext(side = 1, line = 3.5, font = 2, cex = 1.5, text = 'Month')
mtext(side = 2, line = 3.5, font = 2, cex = 1.5, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, text = caption, adj = 0)

dev.off()

print(range(z))
# hcl.pals() # Funcion para listar la paleta de color disponible
#=============================================================================#
# END OF PROGRAM
#=============================================================================#