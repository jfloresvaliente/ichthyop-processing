#=============================================================================#
# Name   : plot_ROMS_kx_hovmuller_Climatology
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')

dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/'
sufijo  <- 'release_zone'
nlevels <- 64 # Number of levels in the color palette
years   <- 1:3 # 10 km
# years <- 1980:2000 # rsodi

#===== Config for MESO kx var =====#
namevar  <- 'MESO'
kx       <- 1.6
zlim     <- c(0.2, .8)
isolines <- seq(zlim[1], zlim[2], 0.1) # Isolines to be plotted
caption  <- 'Functional response (f)'

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
Rdata    <- paste0(dirpath, sufijo, '/', sufijo,'_', namevar, '_hovmuller.Rdata')
png_name <- paste0(dirpath, sufijo, '/', sufijo,'_', namevar, 'kx', kx, '_hovmuller_Climatology.png')
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
z <- z/(z + kx)

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
                 axis(side = 1, font = 2, cex.axis = 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, cex.axis = 1.5, lwd = 2, lwd.ticks = 2, at = seq(from = range(y)[1], to = range(y)[2], by = 10))
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines, font = 2, lwd.ticks = 2, cex.axis = 1.5)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Months')
mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Depth [m]')
mtext(side = 3, line = 0.2, font = 2, cex = 2.0, adj = 0.00, text = caption)

dev.off()

print(range(z))
# hcl.pals() # Funcion para listar la paleta de color disponible
#=============================================================================#
# END OF PROGRAM
#=============================================================================#