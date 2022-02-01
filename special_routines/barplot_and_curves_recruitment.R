#=============================================================================#
# Name   : barplot_and_curves_recruitment
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Barplot associated to spawning depth & Lines associated to spawning bathymetry.
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu7/results_no_shelf/'
ymax    <- 100

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat   <- read.table(file = paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')
depth <- tapply(dat$Recruitprop, list(dat$Month, dat$ReleaseDepth), mean)
bathy <- tapply(dat$Recruitprop, list(dat$Month, dat$ReleaseBathy), mean)

cols <- c('grey5', 'grey50', 'grey80')
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

png(filename = paste0(dirpath, 'barplot_and_curves.png'), height = 750, width = 1050, res = 120)
par(mar=c(6,4,1,.5), xpd=TRUE)
plot1 <- barplot(t(depth), beside = T, ylim = c(0,ymax), axes = F, col = cols, names.arg = rep('',length(meses)))

axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = plot1[2,], labels = meses)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, las = 2)
mtext(side = 2, text = 'Recruitment (%)', lwd = 2, font = 2, line = 2.5, cex = 1.5)

lines(plot1[2,], bathy[,1])
points(plot1[2,], bathy[,1], pch = 17, cex = 1.3)

lines(plot1[2,], bathy[,2])
points(plot1[2,], bathy[,2], pch = 8, cex = 1.3)

lines(plot1[2,], bathy[,3])
points(plot1[2,], bathy[,3], pch = 16, cex = 1.3)

legend('bottomleft', inset=c(0.05,-0.23), legend = colnames(depth), bty = 'n', horiz = T, title.adj = 0, text.font = 2,
       fill = cols, title = 'Spawning Depth [m] (Histograms)', cex = 1)

legend('bottomleft', inset=c(0.50,-0.23), legend = colnames(bathy), bty = 'n', horiz = T, title.adj = 0, text.font = 2,
       lty = 1, pch = c(17,8,16), title = 'Spawning Bathymetry [m] (Curves)', cex = 1)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#