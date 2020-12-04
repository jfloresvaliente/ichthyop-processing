dirpath <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/peru10km/Brochier2008/LatitudeDepthBathy/out/results/'

dat <- read.table(file = paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')

depth <- tapply(dat$Recruitprop, list(dat$Month, dat$Depth), mean)
bathy <- tapply(dat$Recruitprop, list(dat$Month, dat$Bathy), mean)

cols <- c('grey30', 'grey60', 'grey90')

png(filename = paste0(dirpath, 'plot_recruitmentIchthyopBathyDepth.png'), height = 850, width = 850, res = 120)
par(mar = c(3,3,1,1))
plot1 <- barplot(t(depth), beside = T, ylim = c(0,70), col = cols)

lines(plot1[2,], bathy[,1])
points(plot1[2,], bathy[,1], pch = 17)

lines(plot1[2,], bathy[,2])
points(plot1[2,], bathy[,2], pch = 8)

lines(plot1[2,], bathy[,3])
points(plot1[2,], bathy[,3], pch = 16)

legend(x = 1, y = 70, legend = colnames(depth), bty = 'n', horiz = T, title.adj = 0,
       fill = cols, title = 'Spawning Depth [m] (Histograms)')

legend(x = 1, y = 60, legend = colnames(bathy), bty = 'n', horiz = T, title.adj = 0,
       lty = 1, pch = c(17,8,16), title = 'Spawning Bathymetry [m] (Curves)')

dev.off()
