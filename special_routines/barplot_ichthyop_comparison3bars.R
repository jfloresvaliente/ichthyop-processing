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
dat2 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/peru02km_new/LatitudeBathyDepth/out/results/ichthyop_output.csv', header = T, sep = ';')
dat3 <- read.table('C:/Users/jflores/Documents/ICHTHYOP/peru02km/LatitudeBathyDepth/out/results/ichthyop_output.csv', header = T, sep = ';')

# ylab <- 'Retention (%)'
# ylab <- 'Recruitment (%)'
ylab <- 'Pre-recruitment (%)'

lats     <- seq(from = 6, to = 14, by = 2)
ymax     <- 70
col_bars <- c('grey10','grey50','grey90')

legend   <- c( '10 km', '02 km', '02 km (interpolated)')
# legend   <- c( 'Age criteria', 'Size criteria k_x = 0', 'Size criteria k_x = 1.6')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
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

## PLOT ##

png(paste0(dirpath, 'barplot_ichthyop_comparison3bars.png'), height = 850, width = 1250, res = 120)

#=====Plot by Month=====#
par(mfrow = c(2,2))
par(mar=c(4 , 5 , 1.5 , 0.3))
dayplot   <- barplot(day, beside = T, xlab='', ylab= '' ,ylim = c(0,ymax),
                     axes = T, axisnames = T, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(dayplot[1,], day1[,2],
       dayplot[1,], day1[,3],
       angle=90,code=3,length=0.025)
arrows(dayplot[2,], day2[,2],
       dayplot[2,], day2[,3],
       angle=90,code=3,length=0.025)
arrows(dayplot[3,], day3[,2],
       dayplot[3,], day3[,3],
       angle=90,code=3,length=0.025)
legend('topright', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2.5, cex = 0.75, font = 2, text = 'Spawning Month')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)

#=====Plot by release latitude=====#
par(mar=c(4 , 5 , 1.5 , 0.3))
zoneplot   <- barplot(zone, beside = T, xlab='', ylab= '' ,ylim = c(0,ymax),
                      axes = T, axisnames = F, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(zoneplot[1,], zone1[,2],
       zoneplot[1,], zone1[,3],
       angle=90,code=3,length=0.025)
arrows(zoneplot[2,], zone2[,2],
       zoneplot[2,], zone2[,3],
       angle=90,code=3,length=0.025)
arrows(zoneplot[3,], zone3[,2],
       zoneplot[3,], zone3[,3],
       angle=90,code=3,length=0.025)
# legend('topleft', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2.5, cex = 0.75, font = 2, text = 'Spawning Latitude')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)
text((zoneplot[1,]+zoneplot[2,]+zoneplot[3,])/3, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

#=====Plot by release depth=====#
par(mar=c(4 , 5 , 1.5 , 0.3))
depthplot   <- barplot(depth, beside = T, xlab='', ylab= '' ,ylim = c(0,ymax),
                       axes = T, axisnames = T, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(depthplot[1,], depth1[,2],
       depthplot[1,], depth1[,3],
       angle=90,code=3,length=0.025)
arrows(depthplot[2,], depth2[,2],
       depthplot[2,], depth2[,3],
       angle=90,code=3,length=0.025)
arrows(depthplot[3,], depth3[,2],
       depthplot[3,], depth3[,3],
       angle=90,code=3,length=0.025)
# legend('topleft', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2.5, cex = 0.75, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)

#=====Plot by release bathymetry=====#
par(mar=c(4 , 5 , 1.5 , 0.3))
bathyplot   <- barplot(bathy, beside = T, xlab='', ylab= '' ,ylim = c(0,ymax),
                       axes = T, axisnames = T, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(bathyplot[1,], bathy1[,2],
       bathyplot[1,], bathy1[,3],
       angle=90,code=3,length=0.025)
arrows(bathyplot[2,], bathy2[,2],
       bathyplot[2,], bathy2[,3],
       angle=90,code=3,length=0.025)
arrows(bathyplot[3,], bathy3[,2],
       bathyplot[3,], bathy3[,3],
       angle=90,code=3,length=0.025)
# legend('topleft', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2.5, cex = 0.75, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#