png(filename = paste0(dirpath, '/results/ichthyop_output.png'), height = 850, width = 1250, res = 120)
par(mfrow = c(2,2), mar = c(4,4,1,1))

# yearlab <- NULL
# for(i in 1:length(years)) yearlab <- c(yearlab, paste0('Y', years[i]))
# 
# yearplot <- barplot(year[,1], ylim = c(0, ymax), axes = F, names.arg = yearlab)
# axis(2, las = 2)
# abline(h = hlines, lty = 3, lwd = .05)
# arrows(yearplot, year[,2], yearplot, year[,3], angle = 90, code = 3, length = 0.05)
# mtext(text = 'Release Year', side = 1, line = 2, cex = 0.75)

dayplot <- barplot(day[,1], ylim = c(0, ymax), axes = F)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(dayplot, day[,2], dayplot, day[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Month', side = 1, line = 2, cex = 0.75)

depthplot <- barplot(depth[,1], ylim = c(0, ymax), axes = F, names.arg = rownames(depth))
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(depthplot, depth[,2], depthplot, depth[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Depth', side = 1, line = 2, cex = 0.75)

bathyplot <- barplot(bathy[,1], ylim = c(0, ymax), axes = F)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(bathyplot, bathy[,2], bathyplot, bathy[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Bathymetry', side = 1, line = 2, cex = 0.75)

latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))

zoneplot <- barplot(zone[,1], ylim = c(0, ymax), names.arg = latlab, axisnames = FALSE, axes = F)
axis(1, at=dayplot, labels = FALSE, tick = FALSE)
text(zoneplot, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(zoneplot, zone[,2], zoneplot, zone[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Latitude', side = 1, line = 2, cex = 0.75)

# areaplot <- barplot(area[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# arrows(areaplot, area[,2], areaplot, area[,3], angle = 90, code = 3, length = 0.05)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#