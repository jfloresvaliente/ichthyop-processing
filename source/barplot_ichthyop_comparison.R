source('source/ichthyop_libraries.R')
source('source/ichthyop_functions.R')

dirpath <- 'E:/ICHTHYOP/10kmparent/'
dat1 <- read.table('E:/ICHTHYOP/10kmparent/FISICA/out/results_60days/ichthyop_output.csv', header = T, sep = ';')
dat2 <- read.table('E:/ICHTHYOP/10kmparent/DEB/out/encrasicolus/results_60days/ichthyop_output.csv', header = T, sep = ';')

lats     <- seq(from = 2, to = 20, by = 2)
ymax     <- 65
col_bars <- c('grey30','grey80')
legend   <- c( 'Age criteria', 'Size criteria')
# legend   <- c( '30-days', '60-days')
# legend   <- c( 'E. encrasicolus', 'E. ringens')
######---------#
day1 <- recruitment_day(dat1)
day2 <- recruitment_day(dat2)
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

## PLOT ##

png(paste0(dirpath, 'ich-deb_comparison.png'), height = 850, width = 1250, res = 120)

#=====Plot by Month=====#
par(mfrow = c(2,2))
par(mar=c(4 , 5 , 1.5 , 0.3))
dayplot   <- barplot(day, beside = T, xlab="", ylab= "" ,ylim = c(0,ymax),
                     axes = T, axisnames = T, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(dayplot[1,], day1[,2],
       dayplot[1,], day1[,3],
       angle=90,code=3,length=0.025)
arrows(dayplot[2,], day2[,2],
       dayplot[2,], day2[,3],
       angle=90,code=3,length=0.025)
legend('topleft', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2, cex = 0.75, font = 2, text = 'Release Month')
mtext(side = 2, line = 2, cex = 0.75, font = 2, text = 'Recruitment (%)')

#=====Plot by release depth=====#
par(mar=c(4 , 5 , 1.5 , 0.3))
depthplot   <- barplot(depth, beside = T, xlab="", ylab= "" ,ylim = c(0,ymax),
                     axes = T, axisnames = T, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(depthplot[1,], depth1[,2],
       depthplot[1,], depth1[,3],
       angle=90,code=3,length=0.025)
arrows(depthplot[2,], depth2[,2],
       depthplot[2,], depth2[,3],
       angle=90,code=3,length=0.025)
legend('topleft', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2, cex = 0.75, font = 2, text = 'Release Depth')
mtext(side = 2, line = 2, cex = 0.75, font = 2, text = 'Recruitment (%)')

#=====Plot by release bathymetry=====#
par(mar=c(4 , 5 , 1.5 , 0.3))
bathyplot   <- barplot(bathy, beside = T, xlab="", ylab= "" ,ylim = c(0,ymax),
                       axes = T, axisnames = T, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(bathyplot[1,], bathy1[,2],
       bathyplot[1,], bathy1[,3],
       angle=90,code=3,length=0.025)
arrows(bathyplot[2,], bathy2[,2],
       bathyplot[2,], bathy2[,3],
       angle=90,code=3,length=0.025)
legend('topleft', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2, cex = 0.75, font = 2, text = 'Release Bathymetry')
mtext(side = 2, line = 2, cex = 0.75, font = 2, text = 'Recruitment (%)')

#=====Plot by release latitude=====#
par(mar=c(4 , 5 , 1.5 , 0.3))
zoneplot   <- barplot(zone, beside = T, xlab="", ylab= "" ,ylim = c(0,ymax),
                       axes = T, axisnames = F, col = col_bars, yaxt='n')
axis(2, las = 2)
arrows(zoneplot[1,], zone1[,2],
       zoneplot[1,], zone1[,3],
       angle=90,code=3,length=0.025)
arrows(zoneplot[2,], zone2[,2],
       zoneplot[2,], zone2[,3],
       angle=90,code=3,length=0.025)
legend('topleft', legend = legend, bty = 'n', fill = col_bars)
mtext(side = 1, line = 2, cex = 0.75, font = 2, text = 'Release Latitude')
mtext(side = 2, line = 2, cex = 0.75, font = 2, text = 'Recruitment (%)')
text((zoneplot[1,]+zoneplot[2,])/2, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)

dev.off()
