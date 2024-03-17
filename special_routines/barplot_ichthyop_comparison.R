#=============================================================================#
# Name   : barplot_ichthyop_comparison
# Author : 
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath <- 'C:/Users/jflores/Desktop/'
csv1 <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052/case2/'
csv2 <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/out_case2/'

# ylab <- 'Retention (%)'
ylab <- 'Recruitment (%)'
# ylab <- 'Pre-recruitment (%)'
meses <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

lats     <- seq(from = 2, to = 20, by = 2)
ymax     <- c(0,45)
col_bars <- c('grey10','grey50')

# legend   <- c( 'Age criteria', 'Size criteria')
# legend   <- c( '10 km', '02 km')
# legend   <- c( '30-days', '60-days')
# legend   <- c( 'E. encrasicolus', 'E. ringens')
# legend   <- c( 'f = ~', 'f = 1')
# legend   <- c( 'k = 0', 'k = 1.6')
# legend   <- c( '2 km', '2 km new')
# legend   <- c( 'ROMS', 'ROMS-PISCES')
# legend   <- c('Size criteria k_x = 0', 'Size criteria k_x = 1.6')
# legend   <- c('Case1', 'Case2')
legend   <- c('E. encrasicolus', 'E. ringens')

png_name <- paste0(dirpath, 'barplot_ichthyop_comparison.png')
#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dat1 <- read.table(paste0(csv1, '/results/ichthyop_output.csv'), header = T, sep = ';')
dat2 <- read.table(paste0(csv2, '/results/ichthyop_output.csv'), header = T, sep = ';')

yticks <- seq(ymax[1],ymax[2],10)

day1 <- recruitment_month(dat1)
day2 <- recruitment_month(dat2)
day  <- rbind(day1[,1], day2[,1])

depth1 <- recruitment_depth(dat1)
depth2 <- recruitment_depth(dat2)
depth  <- rbind(depth1[,1], depth2[,1])

bathy1 <- recruitment_bathy(dat1)
bathy2 <- recruitment_bathy(dat2)
bathy  <- rbind(bathy1[,1], bathy2[,1])

latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))
zone1  <- recruitment_zone(dat1)
zone2  <- recruitment_zone(dat2)
zone   <- rbind(zone1[,1], zone2[,1]); colnames(zone) <- latlab

#========================= PLOT =========================#
png(png_name, height = 850, width = 1250, res = 120)
par(mfrow = c(2,2))

#========================= Plot by spawning month =========================#
par(mar = c(3.5,4,.5,4))
dayplot   <- barplot(day, beside = T, xlab='', ylab= '' ,ylim = ymax,
                     axes = T, axisnames = F, col = col_bars, yaxt='n')
mtext(side = 1, line = 2  , cex = 1.3, font = 2, text = 'Spawning Month')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'a)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(dayplot,2,mean), labels = 1:12)
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

arrows(dayplot[1,], day1[,2],
       dayplot[1,], day1[,3],
       angle=90,code=3,length=0.025, col = 'grey25')
arrows(dayplot[2,], day2[,2],
       dayplot[2,], day2[,3],
       angle=90,code=3,length=0.025, col = 'grey25')

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
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Latitude')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'b)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(zoneplot,2,mean), labels = rep('',length(latlab)))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)
text(apply(zoneplot,2,mean), -ymax[2]/20, labels = latlab, srt = 20, xpd = TRUE, cex = 1, font = 2)

arrows(zoneplot[1,], zone1[,2],
       zoneplot[1,], zone1[,3],
       angle=90,code=3,length=0.025, col = 'grey25')
arrows(zoneplot[2,], zone2[,2],
       zoneplot[2,], zone2[,3],
       angle=90,code=3,length=0.025, col = 'grey25')

#========================= Plot by spawning depth =========================#
par(mar = c(3.5,4,.5,4))
depthplot   <- barplot(depth, beside = T, xlab='', ylab= '' ,ylim = ymax,
                     axes = T, axisnames = F, col = col_bars, yaxt='n')
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Depth [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'c)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(depthplot,2,mean), labels = colnames(depth))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

arrows(depthplot[1,], depth1[,2],
       depthplot[1,], depth1[,3],
       angle=90,code=3,length=0.025, col = 'grey25')
arrows(depthplot[2,], depth2[,2],
       depthplot[2,], depth2[,3],
       angle=90,code=3,length=0.025, col = 'grey25')

#========================= Plot by spawning bathymetry =========================#
par(mar = c(3.5,4,.5,4))
bathyplot   <- barplot(bathy, beside = T, xlab='', ylab= '' ,ylim = ymax,
                       axes = T, axisnames = F, col = col_bars, yaxt='n')
mtext(side = 1, line = 2.1, cex = 1.3, font = 2, text = 'Spawning Bathymetry [m]')
mtext(side = 2, line = 2.5, cex = 1.3, font = 2, text = ylab)
mtext(side = 3, line = -1 , cex = 1.5, font = 2, text = 'd)', adj = 0.025)
axis(side = 1, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = apply(depthplot,2,mean), labels = colnames(bathy))
axis(side = 2, lwd = 2, lwd.ticks = 2, cex.axis = 1.2, font = 2, at = yticks, labels = yticks, las = 2)

arrows(bathyplot[1,], bathy1[,2],
       bathyplot[1,], bathy1[,3],
       angle=90,code=3,length=0.025, col = 'grey25')
arrows(bathyplot[2,], bathy2[,2],
       bathyplot[2,], bathy2[,3],
       angle=90,code=3,length=0.025, col = 'grey25')

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#