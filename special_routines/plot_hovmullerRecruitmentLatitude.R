#=============================================================================#
# Name   : plot_hovmullerRecruitmentLatitude
# Author : 
# Date   : 
# Version:
# Aim    : Plot Hovmuller of anual mean Recruitment.
# URL    : 
#=============================================================================#
library(fields)

dirpath  <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj_shape_pecq/case1/'
lat_div  <- 2  # Latitudinal resolution
nlevels  <- 64 # Number of levels in the color palette
zlim     <- c(0,85)
isolines1 <- round(seq(zlim[1], zlim[2], 10), 1) # Isolines to be plotted
isolines2 <- round(seq(zlim[1], zlim[2], 10), 1) # Isolines to be plotted

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#

# Plot Climatology
Rdata    <- paste0(dirpath, '/results/hovmuller/hovmullerRecruitmentLatitude', lat_div, 'degrees.Rdata')
load(Rdata)

x <- hovmuller$x
y <- hovmuller$y
z <- hovmuller$z

lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)
ytics <- seq(from = range(y)[1], to = range(y)[2], by = 2)
ylabs <- paste0(abs(ytics), 'ºS')

# Plot Climatology 1
png_name <- paste0(dirpath, '/results/hovmuller/hovmullerRecruitmentLatitude', lat_div, 'degrees.png')
png(filename = png_name, width = 850, height = 850, res = 120)
par(mar = c(5, 5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines1, labels = isolines1, add = T, lwd = 2, labcex = 1)
                 axis(side = 1, font = 2, cex.axis = 1.0, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 axis(side = 2, font = 2, cex.axis = 1.0, lwd = 2, lwd.ticks = 2, at = ytics, labels = ylabs)
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines1, font = 2, lwd.ticks = 2, cex.axis = 1.3)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.25, text = 'Spawning month')
mtext(side = 2, line = 3.5, font = 2, cex = 1.5, adj = 0.45, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.00, text = 'Recruitment [%]')
dev.off()

# Plot Climatology 2
png_name <- paste0(dirpath, '/results/hovmuller/hovmullerRecruitmentLatitude', lat_div, 'degrees2.png')
png(filename = png_name, width = 1850, height = 750, res = 120)
par(mar = c(5, 5, 3.5, 3.5))
filled.contour(x = x, y = y, z = z, zlim = zlim,
               # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
               col = tim.colors(length(lev)-1),
               levels = lev,
               xlab = '', ylab = '',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines1, labels = isolines1, add = T, lwd = 2, labcex = 1)
                 axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                 # axis(side = 1, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = seq(1,3,.5))
                 axis(side = 2, font = 2, cex.axis= 1.5, lwd = 2, lwd.ticks = 2, at = ytics, labels = ylabs)
                 box(lwd = 2)
               },
               key.axes = axis(4, isolines1, font = 2, lwd.ticks = 2, cex.axis = 1.5)
)
mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Spawning month')
mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Latitude')
mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.00, text = 'Recruitment [%]')

dev.off()

print(range(hovmuller$z, na.rm = T))

# Plot Interanual
Rdata    <- paste0(dirpath, '/results/hovmuller/hovmullerRecruitmentLatitudeByYear', lat_div, 'degrees.Rdata')

if(file.exists(Rdata)){
  load(Rdata)
  
  x <- hovmuller$x
  y <- hovmuller$y
  z <- hovmuller$z
  
  lev <- seq(from = zlim[1], to = zlim[2], length.out = nlevels)
  
  png_name <- paste0(dirpath, '/results/hovmuller/hovmullerRecruitmentLatitudeByYear', lat_div, 'degrees.png')
  png(filename = png_name, width = 1850, height = 750, res = 120)
  par(mar = c(5, 5, 3.5, 3.5))
  filled.contour(x = x, y = y, z = z, zlim = zlim,
                 # col = hcl.colors(n = length(lev)-1, palette = 'Blue-Red 3'),
                 col = tim.colors(length(lev)-1),
                 levels = lev,
                 xlab = '', ylab = '',
                 plot.axes = {
                   contour(x = x, y = y, z = z, levels = isolines2, labels = isolines2, add = T, lwd = 2, labcex = 1)
                   axis(side = 1, font = 2, cex.axis = 1.5, lwd = 2, lwd.ticks = 2, at = (1:range(x)[2]))
                   axis(side = 2, font = 2, cex.axis = 1.5, lwd = 2, lwd.ticks = 2, at = ytics, labels = ylabs)
                   box(lwd = 2)
                 },
                 key.axes = axis(4, isolines2, font = 2, lwd.ticks = 2, cex.axis = 1.5)
  )
  mtext(side = 1, line = 3.0, font = 2, cex = 1.5, adj = 0.45, text = 'Spawning year')
  mtext(side = 2, line = 3.8, font = 2, cex = 1.5, text = 'Latitude')
  mtext(side = 3, line = 0.2, font = 2, cex = 1.5, adj = 0.00, text = 'Recruitment [%]')
  
  dev.off()
  
  print(range(hovmuller$z, na.rm = T))
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#