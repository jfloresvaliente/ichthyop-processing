dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO60dias/results/'

col <- c('red', 'blue', 'green')
drif <- c(1,5,9)
chose <- 3

png(filename = 'C:/Users/jflores/Desktop/3particles.png', height = 550, width = 1050, res = 120)
par(mfrow = c(1,3), mar = c(4,4,1,1))

plot(1:721, 1:721, type = 'n', ylim = c(0, 10), axes = F, xlab = 'Days', ylab = 'Mesozooplankton [umol C L-1]')
axis(side = 2)
axis(side = 1, at = seq(1,721,24), labels = seq(1,61,2))
box()
legend('topleft', legend = paste('M',drif), bty = 'n', lty = 1, col = col)
for(i in 1:length(drif)){
  dat <- read.table(paste0(dirpath, 'dat', i, '.csv'), header = T, sep = ';')
  dat <- subset(dat, dat$Drifter == chose)
  lines(dat$Timer, dat$MESO, col = col[i])
}

plot(1:721, 1:721, type = 'n', ylim = c(15, 27), axes = F, xlab = 'Days', ylab = 'Temperature [ºC]')
axis(side = 2)
axis(side = 1, at = seq(1,721,24), labels = seq(1,61,2))
box()
legend('topleft', legend = paste('M',drif), bty = 'n', lty = 1, col = col)
for(i in 1:length(drif)){
  dat <- read.table(paste0(dirpath, 'dat', i, '.csv'), header = T, sep = ';')
  dat <- subset(dat, dat$Drifter == chose)
  lines(dat$Timer, dat$temp, col = col[i])
}

plot(1:721, 1:721, type = 'n', ylim = c(0, 60), axes = F, xlab = 'Days', ylab = 'Length [mm]')
axis(side = 2)
axis(side = 1, at = seq(1,721,24), labels = seq(1,61,2))
box()
legend('topleft', legend = paste('M',drif), bty = 'n', lty = 1, col = col)
for(i in 1:length(drif)){
  dat <- read.table(paste0(dirpath, 'dat', i, '.csv'), header = T, sep = ';')
  dat <- subset(dat, dat$Drifter == chose)
  lines(dat$Timer, dat$length, col = col[i])
}

dev.off()


library(ncdf4)
nc <- nc_open('E:/ICHTHYOP/10kmparent/Fisica-DEB/out/out_ichthyop-run202007211206.nc')


temp <- ncvar_get(nc, 'temp')
f <- ncvar_get(nc, 'f')
length <- ncvar_get(nc, 'length')
