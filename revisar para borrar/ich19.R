#=============================================================================#
# Name   : plot_hovmuller_drifters
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : Hovmuller diagram of the retention rates
# URL    : 
#=============================================================================#
library(fields)

dirpath  <- 'C:/Users/jflores/Desktop/results_DEB/'
latilim  <- c(-20, -2)    # Latitude extension of the area 
zlim     <- 25            # Retention rate interval to be plotted
nlevels  <- 25            # Number of levels in the color palette
isolines <- seq(0,zlim,5) # Isolines to be plotted
ylim     <- c(0,15)
#------------- Do not change anything after here -------------#

# Read data output from Ichthyop simulation
dat <- read.table(paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')
dat$NumberReleased <- 1

mean_month <- 100*tapply(dat$IfRecruited, dat$Day, sum)/tapply(dat$NumberReleased, dat$Day, sum)
mean_depth <- 100*tapply(dat$IfRecruited, dat$Depth, sum)/tapply(dat$NumberReleased, dat$Depth, sum)
mean_area  <- 100*tapply(dat$IfRecruited, dat$ReleaseArea, sum)/tapply(dat$NumberReleased, dat$ReleaseArea, sum)
mean_pixel <- 100*tapply(dat$IfRecruited, dat$PixelCoast, sum)/tapply(dat$NumberReleased, dat$PixelCoast, sum)

mean_lat_month   <- 100*tapply(dat$IfRecruited, list(dat$Day, dat$ReleaseArea), sum)/tapply(dat$NumberReleased, list(dat$Day, dat$ReleaseArea), sum)
mean_pixel_month <- 100*tapply(dat$IfRecruited, list(dat$Day, dat$PixelCoast), sum)/tapply(dat$NumberReleased, list(dat$Day, dat$PixelCoast), sum)

z <- mean_lat_month[, c(dim(mean_lat_month)[2]:1)]
x <- 1:12
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
lev <- seq(from = 0, to = zlim, length.out = nlevels)
png(filename = paste0(dirpath, 'hovmuller.png'), width = 850, height = 850, res = 120)
filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
               xlab = 'Months', ylab = 'Latitude',
               plot.axes = {
                 contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                 axis(1, 1:12)
                 axis(2, seq(latilim[1],latilim[2], by = 2))
               })
dev.off()

latis <- seq(latilim[1], latilim[2], length.out = 4)

for(i in 1:(length(latis)-1)){
  sub_sec <- subset(dat, dat$Lat >= latis[i] & dat$Lat < (latis[i]+6))
  
  # Plot Mean_month
  mean_sub_sec <- 100*tapply(sub_sec$IfRecruited, list(sub_sec$Day, sub_sec$ReleaseArea), sum)/tapply(sub_sec$NumberReleased, list(sub_sec$Day, sub_sec$ReleaseArea), sum)
  
  z <- mean_sub_sec[, c(dim(mean_sub_sec)[2]:1)]
  x <- 1:12
  y <- seq(from = latis[i], to = latis[i]+6, length.out = dim(z)[2])
  lev <- seq(from = 0, to = zlim, length.out = nlevels)
  png_file <- paste0(dirpath, 'lati', abs(latis[i]), '-',abs(latis[i]+6),'hovmuller.png')
  png(filename = png_file, width = 850, height = 850, res = 120)
  filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
                 xlab = 'Months', ylab = 'Latitude',
                 plot.axes = {
                   contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
                   axis(1, 1:12)
                   axis(2, seq(latilim[1],latilim[2], by = 2))
                 })
  dev.off()
}

pixels <- as.numeric(levels(factor(dat$PixelCoast))) * 10
for(i in 1:(length(latis)-1)){
  sub_sec <- subset(dat, dat$Lat >= latis[i] & dat$Lat < (latis[i]+6))
  
  # Plot Mean_month
  mean_sub_sec <- 100*tapply(sub_sec$IfRecruited, list(sub_sec$Day, sub_sec$PixelCoast), sum)/tapply(sub_sec$NumberReleased, list(sub_sec$Day, sub_sec$PixelCoast), sum)
  
  plot(pixels, pixels, type = 'n', ylim = c(0,50))
  for(j in 1:12){
   lines(pixels, mean_sub_sec[j,]) 
  }
}


# # Plot curva de reclutamiento
# plot(1:length(mean_pixel), mean_pixel, type = 'l', xlab = 'Distance to the Coast (km)', ylab = 'Recruitment (%)', axes = F)
# axis(1, at = 1:length(dist), labels = (as.numeric(dist)*10))
# axis(2)
# box()
# 
# lines(1:length(dist), distmean, col = 'red')
# legend('topright', legend = c('Ichthyop', 'Ichthyop-DEB'),
#        bty = 'n', text.col = c('black','red'))
#
#
# x11()
# ylab = 'Recruitment (%)'
# xlab = 'Distance to the coast'
# months = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
# cols <- rep(c('red','blue','green','black'), each = 3)
# pc <- rep(c(1,2,5), 4)
# plot(0:500, type = 'n', ylim = c(0,50), xlab = xlab, ylab = ylab)
# for(i in 1:12){
#   lines(as.numeric(pixel)*10, pixelmat[,i] , col = cols[i])
#   points(as.numeric(pixel)*10, pixelmat[,i], col = cols[i], pch = pc[i])
# }
# legend('topright', legend = months, bty = 'n', col = cols, lty = 1, pch = pc)
#
#
# 
# regiones <- NULL
# for(i in 1:(length(latis)-1)){
#   seg <- subset(dat, dat$Lat >= latis[i] & dat$Lat < latis[i+1])
#   
#   month_reg <- NULL
#   for(j in 1:12){
#     segmonth <- subset(seg, seg$Day == j)
#     segmonth <- (sum(segmonth$IfRecruited)*100)/dim(segmonth)[1]
#     month_reg <- c(month_reg, segmonth)
#   }
#   regiones <- cbind(regiones, month_reg)
# }
# #
# x11()
# par(mfrow = c(3,1), mar = c(1,3,2,1))
# barplot(regiones[,3], ylim = ylim)
# barplot(regiones[,2], ylim = ylim)
# barplot(regiones[,1], ylim = ylim)
# 
# 
# 
#
# # Plotear el promedio general
# z <- tapply(dat$Recruitprop, list(dat$Day, dat$ReleaseArea), FUN = mean, na.rm = T)
# z <- z[, c(dim(z)[2]:1)]
# x <- 1:12
# y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
# 
# lev <- seq(from = 0, to = zlim, length.out = nlevels)
# 
# png(filename = paste0(dirpath, 'results/hovmuller.png'), width = 850, height = 850, res = 120)
# filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
#                xlab = 'Months', ylab = 'Latitude',
#                plot.axes = {
#                  contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
#                  axis(1, 1:12)
#                  axis(2, seq(latilim[1],latilim[2], by = 2))
#                })
# dev.off()
# 
# # Plotear por cada profundidad de liberacion
# depths <- levels(factor(dat$Depth))
# for(i in depths){
#   z <- subset(dat, dat$Depth == i)
#   z <- tapply(z$Recruitprop, list(z$Day, z$Zone_name), FUN = mean, na.rm = T)
#   z <- z[, c(dim(z)[2]:1)]
#   x <- 1:12
#   y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
#   
#   lev <- seq(from = 0, to = zlim, length.out = nlevels)
#   
#   png(filename = paste0(dirpath, 'results/hovmullerReleaseDepth',i,'.png'), width = 850, height = 850, res = 120)
#   filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
#                  xlab = 'Months', ylab = 'Latitude',
#                  plot.axes = {
#                    contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
#                    axis(1, 1:12)
#                    axis(2, seq(latilim[1],latilim[2], by = 2))
#                  })
#   dev.off()
# }
# 
# # Plotear por cada batimetria de liberacion
# bathys <- levels(factor(dat$Bathy))
# for(i in bathys){
#   z <- subset(dat, dat$Bathy == i)
#   z <- tapply(z$Recruitprop, list(z$Day, z$Zone_name), FUN = mean, na.rm = T)
#   z <- z[, c(dim(z)[2]:1)]
#   x <- 1:12
#   y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
#   
#   lev <- seq(from = 0, to = zlim, length.out = nlevels)
#   
#   png(filename = paste0(dirpath, 'results/hovmullerReleaseBathy',i,'.png'), width = 850, height = 850, res = 120)
#   filled.contour(x = x, y = y, z = z, zlim = c(0,zlim), col = tim.colors(length(lev)-1), levels = lev,
#                  xlab = 'Months', ylab = 'Latitude',
#                  plot.axes = {
#                    contour(x = x, y = y, z = z, levels = isolines, labels = isolines, add = T)
#                    axis(1, 1:12)
#                    axis(2, seq(latilim[1],latilim[2], by = 2))
#                  })
#   dev.off()
# }
#=============================================================================#
# END OF PROGRAM
#=============================================================================#