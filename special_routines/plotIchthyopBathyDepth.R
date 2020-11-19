dirpath <- 'C:/Users/jflores/Desktop/'

dat <- read.table(file = paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')

depth <- tapply(dat$Recruitprop, list(dat$Day, dat$Depth), mean)
bathy <- tapply(dat$Recruitprop, list(dat$Day, dat$Bathy), mean)

cols <- c('grey90', 'grey60', 'grey30')

png(filename = paste0(dirpath, 'plot_recruitmentIchthyopBathyDepth.png'), height = 850, width = 850, res = 120)
plot1 <- barplot(t(depth), beside = T, ylim = c(0,70))

lines(plot1[2,], bathy[,1])
points(plot1[2,], bathy[,1], pch = 15)

lines(plot1[2,], bathy[,2])
points(plot1[2,], bathy[,2], pch = 16)

lines(plot1[2,], bathy[,3])
points(plot1[2,], bathy[,3], pch = 17)

legend('topleft', legend = colnames(bathy), bty = 'n',
       lty = 1, pch = 15:17, title = 'Spawning Bathymetry [m] (Curves)')

legend('topright', legend = colnames(depth), bty = 'n',
       fill = cols, title = 'Spawning Depth [m] (Histograms)')
dev.off()
