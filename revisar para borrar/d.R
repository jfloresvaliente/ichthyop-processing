dat <- read.csv('E:/ICHTHYOP/peru10km/LatitudeBathyDepth/out/results/ichthyop_output.csv', header = T, sep = ';')

depth <- tapply(dat$Recruitprop, list(dat$Day, dat$Depth), mean, na.rm = T)
bathy <- tapply(dat$Recruitprop, list(dat$Day, dat$Bathy), mean, na.rm = T)

col_bars <- c('grey10','grey50','grey90')

png('C:/Users/jflores/Desktop/depth - bathy barplot lines.png', width = 850, height = 850, res = 120)
graph <- barplot(t(depth), beside = T, col = col_bars, ylim = c(0,70))
lines(graph[2,], bathy[,1], type = 'o', pch = 15, lwd = 2)
lines(graph[2,], bathy[,2], type = 'o', pch = 16, lwd = 2)
lines(graph[2,], bathy[,3], type = 'o', pch = 17, lwd = 2)
legend('topright', legend = colnames(depth), bty = 'n', title = 'Spawning Depth (Histograms)', fill = col_bars, cex = 1)
legend('topleft' , legend = colnames(bathy), bty = 'n', title = 'Bathymetry of Spawning Area (Curves)', lty = 1, lwd = 2, pch = 15:17, cex = 1)
dev.off()
