#=============================================================================#
# Name   : barplotDepth_curvesBathymetry
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Barplot associated to spawning depth & Lines associated to spawning bathymetry.
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/peru10km/LatitudeBathyDepth/out/results/'
ymax    <- 80

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat   <- read.table(file = paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')
depth <- tapply(dat$Recruitprop, list(dat$Month, dat$Depth), mean)
bathy <- tapply(dat$Recruitprop, list(dat$Month, dat$Bathy), mean)

cols <- c('grey5', 'grey50', 'grey80')
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Agu','Sep','Oct','Nov','Dec')

png(filename = paste0(dirpath, 'barplotDepth_curvesBathymetry.png'), height = 750, width = 1050, res = 120)
par(mar = c(3,4,1,.5))

null_mat <- depth; null_mat[!is.na(null_mat)] <- NA

plot1 <- barplot(t(null_mat), beside = T, ylim = c(0,ymax), axes = F, names.arg = meses)
axis(2, las = 2)
abline(h = seq(0,ymax,10), lty = 2, lwd = .1, col = 'grey80')
plot1 <- barplot(t(depth), beside = T, ylim = c(0,ymax), col = cols, add = T, axes = F, names.arg = meses)
mtext(side = 2, text = 'Pre-recruitment (%)', lwd = 2, font = 2, line = 2.5)

lines(plot1[2,], bathy[,1])
points(plot1[2,], bathy[,1], pch = 17, cex = 1.3)

lines(plot1[2,], bathy[,2])
points(plot1[2,], bathy[,2], pch = 8, cex = 1.3)

lines(plot1[2,], bathy[,3])
points(plot1[2,], bathy[,3], pch = 16, cex = 1.3)

legend(x = 15, y = 70, legend = colnames(depth), bty = 'n', horiz = T, title.adj = 0.5,
       fill = cols, title = 'Spawning Depth [m] (Histograms)', cex = 1)

legend(x = 15, y = 60, legend = colnames(bathy), bty = 'n', horiz = T, title.adj = 0.5,
       lty = 1, pch = c(17,8,16), title = 'Spawning Bathymetry [m] (Curves)', cex = 1)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#