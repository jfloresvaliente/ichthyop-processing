#=============================================================================#
# Name   : barplot_ichthyop_comparison3bars
# Author : 
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath <- 'C:/Users/jflores/Desktop/'
dat1 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/peru10km/LatitudeBathyDepth/out/results/ichthyop_output.csv', header = T, sep = ';')
dat2 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/peru02km/LatitudeBathyDepth/out/results/ichthyop_output.csv', header = T, sep = ';')
dat3 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/peru02km_new/LatitudeBathyDepth/out/results/ichthyop_output.csv', header = T, sep = ';')

ylab <- 'Retention (%)'
# ylab <- 'Recruitment (%)'
# ylab <- 'Pre-recruitment (%)'

lats     <- seq(from = 6, to = 14, by = 2)
ymax     <- c(0,70)
col_bars <- c('grey10','grey50','grey90')

# legend   <- c('10 km', '02 km', '02 km (interpolated)')
# legend   <- c('Age criteria', 'Size criteria k_x = 0', 'Size criteria k_x = 1.6')
# legend   <- c('simu 30 days', 'simu 90 days: I', 'simu 90 days: II')
# legend   <- c('sans seiul', 'seiul = 0.052', 'seiul = 1')
legend   <- c('D01', 'D02s', 'D02r')

png_name <- paste0(dirpath, 'barplot_ichthyop_comparison3bars.png')
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
arrow_col <- 'blue'
yticks <- seq(ymax[1],ymax[2],10)

day1 <- recruitment_month(dat1)
day2 <- recruitment_month(dat2)
day3 <- recruitment_month(dat3); #day3[day3 != 0] = 0
day  <- rbind(day1[,1], day2[,1], day3[,1])

depth1 <- recruitment_depth(dat1)
depth2 <- recruitment_depth(dat2)
depth3 <- recruitment_depth(dat3); # depth3[depth3 != 0] = 0
depth  <- rbind(depth1[,1], depth2[,1], depth3[,1])

bathy1 <- recruitment_bathy(dat1)
bathy2 <- recruitment_bathy(dat2)
bathy3 <- recruitment_bathy(dat3); # bathy3[bathy3 != 0] = 0
bathy  <- rbind(bathy1[,1], bathy2[,1], bathy3[,1])

latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))
zone1  <- recruitment_zone(dat1)
zone2  <- recruitment_zone(dat2)
zone3  <- recruitment_zone(dat3); # zone3[zone3 != 0] = 0
zone   <- rbind(zone1[,1], zone2[,1], zone3[,1]); colnames(zone) <- latlab

#========================= PLOT =========================#
png(png_name, height = 850, width = 1250, res = 120)
par(mfrow = c(2,2))

#========================= Plot by spawning month =========================#
par(mar = c(3.5,4,.5,4))
dayplot   <- barplot(day, beside = T, xlab='', ylab= '' ,ylim = ymax,
                     axes = T, axisnames = F, col = col_bars, yaxt='n')
# mtext(side = 1, line = 2  , cex = 1.3, font = 2, text = 'Spawning Month')
mtext(side = 1, line = 2  , cex = 1.3, font = 2, text = 'Release Month')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'a)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(dayplot,2,mean), labels = 1:12)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

arrows(dayplot[1,], day1[,2],
       dayplot[1,], day1[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(dayplot[2,], day2[,2],
       dayplot[2,], day2[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(dayplot[3,], day3[,2],
       dayplot[3,], day3[,3],
       angle=90,code=3,length=0.025, col = arrow_col)

legend('topright',
       bty = 'n',
       fill = col_bars,
       border = col_bars,
       seg.len = 5,
       pt.cex  = 1.5,
       legend  = legend,
       text.font = 2,
       cex = 0.9
)

#========================= Plot by spawning latitude =========================#
par(mar = c(3.5,4,.5,4))
zoneplot   <- barplot(zone, beside = T, xlab='', ylab= '' ,ylim = ymax,
                      axes = T, axisnames = F, col = col_bars, yaxt='n')
# mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Latitude')
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Release Latitude')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'b)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(zoneplot,2,mean), labels = rep('',length(latlab)))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)
text(apply(zoneplot,2,mean), -ymax[2]/20, labels = latlab, srt = 20, xpd = TRUE, cex = 1, font = 2)

arrows(zoneplot[1,], zone1[,2],
       zoneplot[1,], zone1[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(zoneplot[2,], zone2[,2],
       zoneplot[2,], zone2[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(zoneplot[3,], zone3[,2],
       zoneplot[3,], zone3[,3],
       angle=90,code=3,length=0.025, col = arrow_col)

#========================= Plot by spawning depth =========================#
par(mar = c(3.5,4,.5,4))
depthplot   <- barplot(depth, beside = T, xlab='', ylab= '' ,ylim = ymax,
                       axes = T, axisnames = F, col = col_bars, yaxt='n')
# mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Depth [m]')
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Release Depth [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'c)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(depthplot,2,mean), labels = colnames(depth))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

arrows(depthplot[1,], depth1[,2],
       depthplot[1,], depth1[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(depthplot[2,], depth2[,2],
       depthplot[2,], depth2[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(depthplot[3,], depth3[,2],
       depthplot[3,], depth3[,3],
       angle=90,code=3,length=0.025, col = arrow_col)

#========================= Plot by spawning bathymetry =========================#
par(mar = c(3.5,4,.5,4))
bathyplot   <- barplot(bathy, beside = T, xlab='', ylab= '' ,ylim = ymax,
                       axes = T, axisnames = F, col = col_bars, yaxt='n')
# mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Release Bathymetry [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'd)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(depthplot,2,mean), labels = colnames(bathy))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

arrows(bathyplot[1,], bathy1[,2],
       bathyplot[1,], bathy1[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(bathyplot[2,], bathy2[,2],
       bathyplot[2,], bathy2[,3],
       angle=90,code=3,length=0.025, col = arrow_col)
arrows(bathyplot[3,], bathy3[,2],
       bathyplot[3,], bathy3[,3],
       angle=90,code=3,length=0.025, col = arrow_col)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#