library(ncdf4)
library(stringr)
library(fields)
library(maps)
library(mapdata)
source('source/compute_recruitment_ichthyop_drifters.R')

dirpath   <- 'E:/ICHTHYOP/peru10km/DistCoast/out/drifters/'
new_path  <- 'E:/ICHTHYOP/peru10km/DistCoast/cfg/'
ymax      <- 40

#---- Do not change anythig after here ----#
# firstdrifter = 1
# lastdrifter = 19448
# firsttime = 1
# lasttime = 31
# recruitmentzone = 1
# dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')
# xy <- read.table(paste0(new_path, 'lonlatDrifters.csv'), sep = ';')
# 
# x11()
# pch = 0
# map('worldHires', add=F, fill=T, col='gray', ylim = c(-15,-5), xlim = c(-85.5,-75.5))
# axis(1); axis(2); box()
# for(i in 1:50){
#   
#   pun <- subset(xy, xy[,3] == i)
#   if(dim(pun)[1] == 0) next() else pch = pch + 1; points(pun[,1], pun[,2], pch = pch, cex = 0.5)
# }

# dat <- compute_recruitment_ichthyop_drifters(dirpath = dirpath,
#                                              firstdrifter = firstdrifter,
#                                              lastdrifter = lastdrifter,
#                                              firsttime = firsttime,
#                                              lasttime = lasttime,
#                                              recruitmentzone = recruitmentzone,
#                                              old_path = old_path,
#                                              new_path = new_path)
# 
# dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
# write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', row.names = F)

dat <- read.csv('E:/ICHTHYOP/peru10km/DistCoast/out/drifters/results/ichthyop_output.csv', sep = ';')
x11();par(mfrow = c(1,3))

# # Por year
# year <- levels(factor(dat$Year))
# yearper <- NULL
# for(i in 1:length(year)){
#   df <- subset(dat, dat$Year == year[i])
#   rec <- sum(df$Recruited)
#   yearper <- c(yearper, rec/dim(df)[1] * 100)
# }
# year <- cbind(year, yearper)

# Por mes
day <- levels(factor(dat$Day))
dayper <- NULL
for(i in 1:length(day)){
  df <- subset(dat, dat$Day == day[i])
  rec <- sum(df$Recruited)
  dayper <- c(dayper, rec/dim(df)[1] * 100)
}
day <- cbind(as.numeric(day), as.numeric(dayper))
barplot(day[,2], names.arg = day[,1], xlab = 'Release Month', ylab = 'Larval Retention (%)', ylim = c(0,20))

# Por depth
depth <- levels(factor(dat$Depth))
depthper <- NULL
for(i in 1:length(depth)){
  df <- subset(dat, dat$Depth == depth[i])
  rec <- sum(df$Recruited)
  depthper <- c(depthper, rec/dim(df)[1] * 100)
}
depth <- cbind(as.numeric(depth), as.numeric(depthper))
barplot(depth[,2], names.arg = depth[,1], xlab = 'Release Depth (m)', ylab = 'Larval Retention (%)', ylim = c(0,20))

# # Por pixel
# pixel <- levels(factor(dat$PixelCoast))
# pixelper <- NULL
# for(i in 1:length(pixel)){
#   df <- subset(dat, dat$PixelCoast == pixel[i])
#   rec <- sum(df$Recruited)
#   pixelper <- c(pixelper, rec/dim(df)[1] * 100)
# }
# pixel <- cbind(pixel, pixelper)
#
# km <- as.numeric(pixel[,1])*10
# re <- as.numeric(pixel[,2])

# plot(km, re)

# Por latitud
lati <- levels(factor(dat$ReleaseArea))
latiper <- NULL
for(i in 1:length(lati)){
  df <- subset(dat, dat$ReleaseArea == lati[i])
  rec <- sum(df$Recruited)
  latiper <- c(latiper, rec/dim(df)[1] * 100)
}
lati <- cbind(as.numeric(lati), as.numeric(latiper))
barplot(lati[,2], names.arg = lati[,1], xlab = 'Latitude of Release (º)', ylab = 'Larval Retention (%)', ylim = c(0,20))


# png(filename = paste0(dirpath, '/results/ichthyop_output.png'), height = 850, width = 1250, res = 120)
# par(mfrow = c(2,3), mar = c(4,4,1,1))
# 
# yearplot <- barplot(year[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# arrows(yearplot, year[,2], yearplot, year[,3], angle = 90, code = 3, length = 0.05)
# 
# dayplot <- barplot(day[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# arrows(dayplot, day[,2], dayplot, day[,3], angle = 90, code = 3, length = 0.05)
# 
# depthplot <- barplot(depth[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# arrows(depthplot, depth[,2], depthplot, depth[,3], angle = 90, code = 3, length = 0.05)
# 
# bathyplot <- barplot(bathy[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# arrows(bathyplot, bathy[,2], bathyplot, bathy[,3], angle = 90, code = 3, length = 0.05)
# 
# # # zoneplot <- barplot(zone[,1], ylim = c(0, ymax), names.arg = c('6º-8º','8º-10º','10º-12º','12º-14º')); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# # zoneplot <- barplot(zone[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# # arrows(zoneplot, zone[,2], zoneplot, zone[,3], angle = 90, code = 3, length = 0.05)
# 
# areaplot <- barplot(area[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# arrows(areaplot, area[,2], areaplot, area[,3], angle = 90, code = 3, length = 0.05)
# dev.off()
# 
