#=============================================================================#
# Name   : barplot_and_curves_recruitment
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Barplot associated to spawning depth & Lines associated to spawning bathymetry.
# URL    : 
#=============================================================================#
dirpath <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj_shape_pecq/case1/'
ymax    <- 80
yticks  <- seq(from = 0, to = ymax, by = 10)
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat   <- read.table(file = paste0(dirpath,'/resultsV/ichthyop_output.csv'), header = T, sep = ';')
# dat$Recruitprop <- dat$N_constantprop
depth <- tapply(dat$Recruitprop, list(dat$Month, dat$ReleaseDepth), mean)
bathy <- tapply(dat$Recruitprop, list(dat$Month, dat$ReleaseBathy), mean)

cols_legend <- c('grey5', 'grey50', 'grey80')

cols <- c('grey5', 'grey50', 'grey80')
border <- c('black', 'black', 'black')

# cols <- c('white', 'white', 'grey80')
# border <- c('white', 'white', 'white')

meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

png(filename = paste0(dirpath, '/resultsV/barplot_and_curves.png'), height = 750, width = 1050, res = 120)
par(mar=c(6,4,1,.5), xpd=TRUE)
plot1 <- barplot(t(depth), beside = T, ylim = c(0,ymax), axes = F, col = cols, names.arg = rep('',length(meses)), border = border)

axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.3, font = 2, at = plot1[2,], labels = meses)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.5, font = 2, at = yticks, labels = yticks, las = 2)
mtext(side = 2, text = 'Recruitment (%)', lwd = 2, font = 2, line = 2.8, cex = 1.5)

lines(plot1[2,], bathy[,1])
points(plot1[2,], bathy[,1], pch = 17, cex = 1.3)

lines(plot1[2,], bathy[,2])
points(plot1[2,], bathy[,2], pch = 8, cex = 1.3)

lines(plot1[2,], bathy[,3])
points(plot1[2,], bathy[,3], pch = 16, cex = 1.3)

mtext(side = 1, lwd = 2, font = 2, line = 2.9, cex = 1.2, adj = 0.03, text = 'Spawning Depth [m] (Histograms)')
legend('bottomleft', inset=c(0.05,-0.23), legend = colnames(depth), bty = 'n', horiz = T, title.adj = 0, text.font = 2,
       fill = cols_legend, title = '', cex = 1.1)

mtext(side = 1, lwd = 2, font = 2, line = 2.9, cex = 1.2, adj = 0.90, text = 'Spawning Bathymetry [m] (Curves)')
legend('bottomleft', inset=c(0.50,-0.23), legend = colnames(bathy), bty = 'n', horiz = T, title.adj = 0, text.font = 2,
       lty = 1, pch = c(17,8,16), title = '', cex = 1.1)

dev.off()

print(depth)
print(bathy)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#