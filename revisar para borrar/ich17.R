dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/lati220/results_DEB/'
dat     <- read.table(paste0(dirpath,'ichthyop_output.csv'), header = T, sep = ';')

# MEAN ( month, lati-zone [norte/centro/sur] )
latis <- c(-20, -2)    # Latitude extension of the area
lat1 <- seq(latis[2], latis[1], by = -6)
lat2 <- lat1 - 6
lty <- c(2,1,3)

lat_mean <- NULL
rowhead  <- NULL
for(i in 1:(length(lat1)-1)){
  month_mean <- NULL
  for(j in 1:12){
    reg <- subset(dat, dat$Lat >= lat2[i] & dat$Lat < lat1[i] & dat$Day == j)
    month_mean <- c(month_mean, (sum(reg$IfRecruited)*100)/dim(reg)[1])
  }
  lat_mean <- rbind(lat_mean, month_mean)
  rowhead <- c(rowhead, paste0(abs(lat1[i]), 'º-', abs(lat2[i]),'º'))
}
rownames(lat_mean) <- rowhead
colnames(lat_mean) <- 1:12

png(filename = paste0(dirpath, 'recruitment_month_zone.png'), height = 850, width = 850, res = 120)
par(mar = c(3,3,1,1))
plot( 1:12, lat_mean[1,], lty = 2, type = 'l', ylim = c(0,20), xlab = '', ylab = '', axes = F, yaxs = 'i')
lines(1:12, lat_mean[2,], lty = 1)
lines(1:12, lat_mean[3,], lty = 3)
axis(1)
axis(2, las = 2)
box()
mtext(side = 1, line = 2, text = 'Release Month')
mtext(side = 2, line = 2, text = 'Recruitment (%)')
legend('topright', legend = rowhead, bty = 'n', lty = lty)
dev.off()

# MEAN ( dist-coast, lati-zone [norte/centro/sur] )
pixels <- levels(factor(dat$PixelCoast))
lat_mean <- NULL
rowhead  <- NULL
for(i in 1:(length(lat1)-1)){
  month_mean <- NULL
  for(j in 1:length(pixels)){
    reg <- subset(dat, dat$Lat >= lat2[i] & dat$Lat < lat1[i] & dat$PixelCoast == pixels[j])
    month_mean <- c(month_mean, (sum(reg$IfRecruited)*100)/dim(reg)[1])
  }
  lat_mean <- rbind(lat_mean, month_mean)
  rowhead <- c(rowhead, paste0(abs(lat1[i]), 'º-', abs(lat2[i]),'º'))
}
rownames(lat_mean) <- rowhead
colnames(lat_mean) <- pixels

png(filename = paste0(dirpath, 'recruitment_distcoast_zone.png'), height = 850, width = 850, res = 120)
par(mar = c(3,3,1,1))
plot (as.numeric(pixels)*10, lat_mean[1,], lty = 2, type = 'l', ylim = c(0,40), xlab = '', ylab = '', axes = F, yaxs = 'i')
lines(as.numeric(pixels)*10, lat_mean[2,], lty = 1)
lines(as.numeric(pixels)*10, lat_mean[3,], lty = 3)
axis(1)
axis(2, las = 2)
box()
mtext(side = 1, line = 2, text = 'Distance to the Coast (km)')
mtext(side = 2, line = 2, text = 'Recruitment (%)')
legend('topright', legend = rowhead, bty = 'n', lty = lty)
dev.off()

# MEAN (month, dist-coast, lati-zone [norte/centro/sur] )
pixels <- levels(factor(dat$PixelCoast))
lat_mean <- array(data = NA, dim = c(3, 12, 25))
rowhead  <- NULL

rowhead
for(i in 1:(length(lat1)-1)){
  for(j in 1:12){
    for(k in 1:length(pixels)){
      sub1 <- subset(dat, dat$Lat >= lat2[i] & dat$Lat < lat1[i] & dat$Day == j & dat$PixelCoast == pixels[k])
      sub1 <- (sum(sub1$IfRecruited)*100)/dim(sub1)[1]
      lat_mean[i,j,k] <- sub1
    }
  }
  rowhead <- c(rowhead, paste0(abs(lat1[i]), 'º-', abs(lat2[i]),'º'))
}

png(filename = paste0(dirpath, 'recruitment_month_distcoast_zone.png'), height = 700, width = 1650, res = 120)
par(mfrow = c(1,3), mar = c(3,4,3,1))
cols <- rep(c('red','blue','green','yellow'), each = 3)
pch  <- rep(1:3, times = 4)
for(i in 1:3){
  zona <- lat_mean[i,,]
  plot (as.numeric(pixels)*10, as.numeric(pixels)*10, type = 'n', ylim = c(0,50), xlab = '', ylab = '')
  mtext(side = 1, line = 2, text = 'Distance to the Coast (km)')
  mtext(side = 2, line = 2, text = 'Recruitment (%)')
  mtext(side = 3, line = .5, text = paste0(abs(lat1[i]), 'º-', abs(lat2[i]),'º'))
  grid()
  for(j in 1:12){
    mon <- zona[j,]
    lines(as.numeric(pixels)*10, mon, col = cols[j])
    points(as.numeric(pixels)*10, mon, col = cols[j], pch = pch[j])
  }
  legend('topright', legend = paste0('M',1:12), col = cols, lty = 1, , pch = pch, bty = 'n')
}
dev.off()


# # Promedio por mes y por zona [norte, centro, sur] y distancia a la costa
# lat_mean <- NULL
# rowhead  <- NULL
# for(i in 1:(length(lat1)-1)){
#   month_mean <- NULL
#   for(j in 1:12){
#     reg <- subset(dat, dat$Lat >= lat2[i] & dat$Lat < lat1[i] & dat$Day == j)
#     month_mean <- c(month_mean, (sum(reg$IfRecruited)*100)/dim(reg)[1])
#   }
#   lat_mean <- rbind(lat_mean, month_mean)
#   rowhead <- c(rowhead, paste0(abs(lat1[i]), 'º-', abs(lat2[i]),'º'))
# }
# rownames(lat_mean) <- rowhead
# colnames(lat_mean) <- 1:12
# cols <- c('grey90', 'grey60', 'grey30')
# barplot(lat_mean, beside = T, col = cols)
# legend('topright', legend = rowhead, bty = 'n', col = cols, fill = cols)
# 
# 
# 
# 
# 
# # Promedio por mes y distancia a la costa
# 
# pixel_mean <- NULL
# for(i in 1:length(pixels)){
#   month_mean <- NULL
#   for(j in 1:12){
#     reg <- subset(dat, dat$PixelCoast == pixels[i] & dat$Day == j)
#     month_mean <- c(month_mean, (sum(reg$IfRecruited)*100)/dim(reg)[1])
#   }
#   pixel_mean <- rbind(pixel_mean, month_mean)
# }
# rownames(pixel_mean) <- pixels
# colnames(pixel_mean) <- 1:12